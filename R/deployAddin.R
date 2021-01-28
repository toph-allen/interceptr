library(shiny)
library(miniUI)
library(shinyglide)

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'deployAddin()'.
deployAddin <- function() {

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- fixedPage(style = "max-width: 500px;",
                        titlePanel("Simple shinyglide app"),

                        glide(
                          height = "350px",
                          screen(
                            p("This is a very simple shinyglide application."),
                            p("Please click on Next to go to the next screen.")
                          ),
                          screen(
                            p("Please choose a value."),
                            numericInput("n", "n", value = 10, min = 10)
                          ),
                          screen(
                            p("And here is the result."),
                            plotOutput("plot")
                          )
                        )
  )

  server <- function(input, output, session) {

    output$plot <- renderPlot({
      hist(
        rnorm(input$n),
        main = paste("n =", input$n),
        xlab = ""
      )
    })

  }

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- shiny::paneViewer(300)
  shiny::runGadget(ui, server, viewer = viewer)
}

