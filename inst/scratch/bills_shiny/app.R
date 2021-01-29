#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyglide)

fib <- function(n) ifelse(n<3, 1, fib(n-1)+fib(n-2))

ui <- fluidPage(
    style = "max-width: 500px;",
    titlePanel("Simple shinyglide app"),

    glide(
        height = "350px",
        deployNow = FALSE,
        screen(
            p("This is a very simple shinyglide application."),
            p("Please click on Next to go to the next screen."),
            p("And here is the result."),
            actionButton("minus", "-1"),
            actionButton("plus", "+1"),
            br(),
            textOutput("value")
        ),
        screen(
            deployNow = FALSE,
            p("Please choose a value."),
            numericInput("n", "n", value = 10, min = 10),
            p("And here is the result."),
            textOutput("value2")
        ),
        screen(
            deployNow = TRUE,
            n = 20,
            p("And here is the result.")
        )
    )
)

server <- function(input, output, session) {
    value <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
    value2 <- reactiveVal(0)       # rv <- reactiveValues(value = 0)

    observeEvent(input$minus, {
        newValue <- value() - 1     # newValue <- rv$value - 1
        value(newValue)             # rv$value <- newValue
    })

    observeEvent(input$plus, {
        newValue <- value() + 1     # newValue <- rv$value + 1
        value(newValue)             # rv$value <- newValue
    })

    output$value <- renderText({
        value()                     # rv$value
    })

    output$value2 <- renderText({
        value()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
