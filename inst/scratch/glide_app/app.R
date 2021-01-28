library(shiny)
library(shinyglide)
library(connectapi)

ui <- fixedPage(style = "max-width: 500px;",
                titlePanel("Simple shinyglide app"),

                glide(
                    height = "350px",
                    screen(
                        p("Title"),
                        p("Instructions"),
                        input$ready <- FALSE
                    ),
                    screen(
                        p("Enter the directory of the content you want to deploy."),
                        textInput("directory", "Content Directory", value = "", width = "400px"),
                        textInput("name", "Name (Optional)", value = "", width = "400px"),
                        textInput("title", "Title", value = "", width = "400px")
                    ),
                    screen(
                        p("Environment Vars for pins"),
                        textInput("CONNECT_SERVER", "CONNECT_SERVER", value = Sys.getenv("CONNECT_SERVER"), width = "400px"),
                        textInput("CONNECT_API_KEY", "CONNECT_API_KEY", value = Sys.getenv("CONNECT_API_KEY"), width = "400px"),
                        next_label = "Deploy",
                    ),
                    screen(
                      output$result %>% poll_task()
                    )
                )
)


server <- function(input, output, session) {
  client <- connect()

  output$result <- function() {
    if (input$ready) {
      rsconnect::writeManifest(input$directory)
      bnd <- bundle_dir(input$directory)
      mycontent <- deploy(
        client,
        bnd,
        # name = "my-awesome-special-application",
        title = input$title,
        # other content settings like access_type, min_procs, etc.
        .pre_deploy = {
          env <- get_environment(content)
          set_environment_new(env,
                              CONNECT_API_KEY = input$CONNECT_API_KEY,
                              CONNECT_SERVER = input$CONNECT_SERVER)
        }
      )
    }
  }
}

shinyApp(ui, server)
