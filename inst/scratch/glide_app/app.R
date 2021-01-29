library(shiny)
library(shinyjs)
library(shinyglide)
# remotes::install_github("colearendt/shinytail")
library(shinytail)
library(connectapi)

# CONNECT_SERVER http://localhost:3939
# API_KEY igZJrDEiKTM1pO3tdd0MBh8Df9Uh2ixE

# CONNECT_SERVER https://rsc.radixu.com/
# API_KEY MOqoQUbKQDXc7isdty52Ro9zuWouiCaj

# readRenviron("../.Renviron.alt")
# Sys.setenv(
#   CONNECT_SERVER = "https://rsc.radixu.com/",
#   CONNECT_API_KEY = "MOqoQUbKQDXc7isdty52Ro9zuWouiCaj"
# )

# pretty hacky, basically depend on:
# - storing state / progress in the R6 object
# - have to run the poll_task_reactive() inside observe() so invalidateLater() works
# - have to gate on "deploying" reactiveVal, and switch it to "off" when done, lest the reactive keep deploying
# - tweaking interval will change the responsiveness
# - the UI still struggles / is not beautiful... :p

ReactiveTask <- R6::R6Class(
  "ReactiveTask", inherit = connectapi::Task,
  public = list(
    finished = FALSE,
    code = -1,
    first = 0
  )
)

poll_task_reactive <- function(task, doneReactiveVal, wait = 1, interval = 100, callback = message) {
  # message("Function invalidated")
  connectapi:::validate_R6_class(task, c("Task", "VariantTask"))
  if (!task$finished) {
    invalidateLater(interval)
    message("Deploying...")
    task_data <- task$get_connect()$task(task$get_task()$task_id, wait = wait,
                                         first = task$first)
    task$finished <- task_data[["finished"]]
    task$code <- task_data[["code"]]
    task$first <- task_data[["last"]]
    lapply(task_data[["output"]], callback)
  } else {
    lapply("Done!", callback)
    message("Done!")
    doneReactiveVal(FALSE)
  }
}

show_bundling = FALSE

ui <- fixedPage(
  # style = "max-width: 800px;",
  useShinyjs(),
  titlePanel("New and Improved Publishing Wizard"),
  glide(
    # height = "600px",
    screen(
      h3("Publish to RStudio Connect!"),
      p("This wizard will guide you through the process of publishing your content to the RStudio Connect server.")
    ),
    screen(
      h3("Where is your content located?"),
      p("Enter the directory on your machine where the content you want to deploy is located."),
      textInput("directory", "Content Directory", value = getwd(), width = "400px")
    ),
    screen(
      h3("Describe your content"),
      p("Set the attributes which describe your content on the server."),
      textInput("name", "Name (Optional)", value = "Your name (must be unique)", width = "400px"),
      textInput("title", "Title", value = "Give me a title!", width = "400px"),
      textInput("description", "Description", value = "One of the most amazing apps I have ever done!", width = "400px"),
      textInput("vanity_url", "Vanity URL (ex. /my-awesome-app)", value = "/my-awesome-app", width = "400px"),
      textInput("image_path", "Image Path on server (ex. /etc/stuff/image.png)", value = "", width = "400px")
    ),
    screen(
      h3("Set the Advanced Options"),
      p("Normally, modifying these values is not necessary, but if you need to, there are here!"),
      numericInput("min_processes", "Min Number of Processes", value = 0, min = 0, width = "400px"),
      numericInput("max_processes", "Max Number of Processes", value = 3, min = 1, width = "400px"),
      numericInput("max_conns_per_process", "Max Connections Per Process", value = 20, min = 0, width = "400px")
    ),
    screen(
      h3("Environment Vars for pins"),
      p("If you are using pins, then you'll want to set these values. Defaults are coming out of your environment, so they may be correct unless you need something different"),
      textInput("CONNECT_SERVER", "CONNECT_SERVER", value = Sys.getenv("CONNECT_SERVER"), width = "400px"),
      textInput("CONNECT_API_KEY", "CONNECT_API_KEY", value = Sys.getenv("CONNECT_API_KEY"), width = "400px")
    ),
    screen(
      h3("Any additional Environment Variables you rely upon?"),
      p("Here is one placeholder you can set - This page really should have a better UX.."),
      textInput("another_env_var", "MY_SPECIAL_NAME", value = "Honey Bear", width = "400px")
    ),
    screen(
      h3("Any diagnostic options?"),
      p("Connect will analyze your content and produce a diagnostic report if things go bad."),
      checkboxInput("debug_output", "Output Diagnostic Report for content")
    ),
    screen(
      actionButton("deploy", "Deploy to Connect!"),
      p(),
      hidden(
        p(id = "init_msg", "Initializing connection to server...")
      ),
      hidden(
        p(id = "manifest_msg", "Creating manifest for directory...")
      ),
      hidden(
        p(id = "bundling_msg", "Bundling up content in directory...")
      ),
      hidden(
        p(id = "deploying_msg", "Starting deployment to server...")
      ),
      hidden(
        p(id = "log_label", "Deployment log:")
      ),
      shinyTail("stream")
      # next_label = ""
    ),
    screen(
      p("You are all done!"),
      actionButton("close", "Close wizard and launch content")
    )
  )
)

server <- function(input, output, session) {
  streamText <- reactiveVal()
  deploying <- reactiveVal(FALSE)
  taskItem <- reactiveVal(NULL)

  observeEvent(input$close, {
    taskItem() %>% browse_dashboard()
    stopApp()
  })

  observeEvent(input$deploy, {
    # readRenviron("../.Renviron.alt")
    show("init_msg")
    client <- connect()
    show("manifest_msg")
    rsconnect::writeManifest(input$directory)
    show("bundling_msg")
    bnd <- bundle_dir(input$directory)

    env_vars <- list(
      CONNECT_API_KEY = input$CONNECT_API_KEY,
      CONNECT_SERVER = input$CONNECT_SERVER,
      MY_SPECIAL_NAME = input$another_env_var,
      vanity_url = input$vanity_url,
      min_processes = input$min_processes,
      max_processes = input$max_processes,
      max_conns_per_process = input$max_conns_per_process
    )

    show("deploying_msg")
    show("log_label")
    cont <- deploy(
      client,
      bnd,
      # name = "my-awesome-special-application",
      title = input$title,
      description = input$description,
      env_vars = env_vars,
      .pre_deploy = {
        env <- get_environment(content)
        set_environment_new(env,
                            CONNECT_API_KEY = env_vars$CONNECT_API_KEY,
                            CONNECT_SERVER = env_vars$CONNECT_SERVER,
                            MY_SPECIAL_NAME = env_vars$MY_SPECIAL_NAME
        )
        set_vanity_url(content, env_vars$vanity_url) # "/my-awesome-app"
        # set_image_path(content, input$image_path) # "./my/local/image.png"
        content$update(
          min_processes = env_vars$min_processes,
          max_processes = env_vars$max_processes,
          max_conns_per_process = env_vars$max_conns_per_process
        )
      },
      .pre_deploy_env = list(env_vars = env_vars) # passing in as list
    )

    newtask <- ReactiveTask$new(
      connect = cont$connect,
      content = cont$content,
      task = cont$task
    )
    taskItem(newtask)
    deploying(TRUE)
  })

  observe({
    req(deploying())
    poll_task_reactive(
      taskItem(), doneReactiveVal = deploying, wait = 0,
      interval = 50,
      callback = function(x) {
        # message("Appending...")
        isolate(streamText(paste(streamText(), x, sep = "\n")))
      }
    )
  })
  output$stream <- renderText(streamText())
}

shinyApp(ui, server)
