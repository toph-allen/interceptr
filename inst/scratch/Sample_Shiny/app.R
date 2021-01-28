#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

is_connect <- function() {
    r_config_active <- Sys.getenv("R_CONFIG_ACTIVE")
    logname <- Sys.getenv("LOGNAME")
    return (r_config_active == "rsconnect" || logname == "rstudio-connect")
}

is_connect_string <- function() {
    if (is_connect()) {
        return ("Thank you for using RStudio Connect")
    }
    return ("")
}

# return list(
#  "found" <- list()
#  "missing" <- list()
#  "error" <- TRUE / FALSE
#  "error_exit" <- function()
# )

get_environment_vars <- function(var_names) {
    sys_env <- Sys.getenv()
    vars <- list()
    missing_vars <- character()
    for (name in var_names) {
        if (!name %in% names(sys_env)) {
            missing_vars <- append(missing_vars, name)
        } else {
            vars[name] <- sys_env[name]
        }
    }
    error <- FALSE
    error_exit <- function() {}
    if (length(missing_vars) > 0) {
        error_exit <- function(is_shiny = FALSE) {
            if (is_shiny == TRUE) {
                return(shinyApp(
                    ui = basicPage(
                        tags$h3("The following environment variables could not be found:"),
                        verbatimTextOutput("text"),
                        tags$h5(is_connect_string())
                    ),
                    server = function(input, output) {
                        output$text <- renderText(paste(missing_vars, sep="", collapse=", "))
                    }
                ))
            } else {
                print("The following environment variables could not be found:")
                print(missing_vars)
                print(is_connect_string())
                knitr::knit_exit()
            }
        }
        error <- TRUE
    }
    return(list("found" = vars, "missing" = missing_vars, "error" = error, "error_exit" = error_exit))
}

env_vars <- get_environment_vars(c("CONNECT_SERVER", "CONNECT_API_KEY"))

if (env_vars$error == TRUE) {
    return (env_vars$error_exit(TRUE))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)

