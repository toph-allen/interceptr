## shinyglide modal example app
## Live version at : https://data.nozav.org/app/shinyglide/03_modal/

library(shiny)
library(shinyglide)

ui <- fixedPage(

    titlePanel("shinyglide!")

)


server <- function(input, output, session) {

    modal_controls <- glideControls(
        list(
            prevButton(),
            firstButton(
                class = "btn btn-danger",
                `data-dismiss`="modal",
                "No, thanks !"
            )
        ),
        list(
            nextButton(),
            lastButton(
                class = "btn btn-success",
                `data-dismiss`="modal",
                "Done"
            )
        )
    )

    glide_modal <- modalDialog(
        title = "Startup assistant",
        easyClose = FALSE,
        footer = NULL,
        glide(
            custom_controls = modal_controls,
            screen(
                next_label = 'Yes, please ! <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
                p("Let's initialize some values, would you ?")
            ),
            screen(
                p("First, please select a mean value"),
                numericInput("mean_modal", "Mean", value = 0)
            ),
            screen(
                p("Next, please select a standard deviation value"),
                numericInput("sd_modal", "Standard deviation", value = 1, min = 0)
            ),
            screen(
                p("Thanks, we're all set !")
            )
        )
    )

    showModal(glide_modal)

    observe({
        updateNumericInput(session, "mean", value = input$mean_modal)
    })

    observe({
        updateNumericInput(session, "sd", value = input$sd_modal)
    })


}

shinyApp(ui, server)
