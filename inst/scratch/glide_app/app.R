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
  titlePanel("Simple shinyglide app"),
  glide(
    # height = "600px",
    screen(
      p("Title"),
      p("Instructions")
    ),
    screen(
      p("Enter the directory of the content you want to deploy."),
      textInput("directory", "Content Directory", value = getwd(), width = "400px"),
      textInput("name", "Name (Optional)", value = "", width = "400px"),
      textInput("title", "Title", value = "", width = "400px")
    ),
    screen(
      p("Environment Vars for pins"),
      textInput("CONNECT_SERVER", "CONNECT_SERVER", value = Sys.getenv("CONNECT_SERVER"), width = "400px"),
      textInput("CONNECT_API_KEY", "CONNECT_API_KEY", value = Sys.getenv("CONNECT_API_KEY"), width = "400px")
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
      actionButton("close", "Close wizard")
    )
  )
)

server <- function(input, output, session) {
  streamText <- reactiveVal()
  deploying <- reactiveVal(FALSE)
  taskItem <- reactiveVal(NULL)

  observeEvent(input$close, {
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

    api_key <- input$CONNECT_API_KEY
    server <- input$CONNECT_SERVER

    show("deploying_msg")
    show("log_label")
    cont <- deploy(
      client,
      bnd,
      # name = "my-awesome-special-application",
      title = input$title,
      # other content settings like access_type, min_procs, etc.
      .pre_deploy = {
        env <- get_environment(content)
        set_environment_new(env,
                            CONNECT_API_KEY = "MOqoQUbKQDXc7isdty52Ro9zuWouiCaj",
                            CONNECT_SERVER = "https://rsc.radixu.com/"
        )
      }
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
