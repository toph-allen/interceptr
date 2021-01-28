get_env <- function(vars = NULL, context = "rmd") {
  found_vars <- na.omit(Sys.getenv(vars, unset = NA))
  missing_vars <- setdiff(vars, names(found_vars))

  if (length(found_vars) == length(vars)) {
    return(as.list(found_vars))
  } else {
    if (context == "rmd") {
      fail_rmd(missing_vars)
    } else if (context == "shiny") {
      fail_shiny(missing_vars)
    }
  }
}

fail_rmd <- function(missing_vars) {
  message <- paste(
    "Rendering halted because following environment variables could not be found:",
    paste(missing_vars, collapse = ", "),
    ifelse(
      is_connect(),
      "Please enter these variables in this document's settings panel under \"Vars\".",
      "Please ensure these variables are available."),
    sep = "\n"
  )
  cat(message)
  knitr::knit_exit()
}

fail_shiny <- function(missing_vars) {
  error_app <- shinyApp(
    ui = basicPage(
      tags$h3("The following environment variables could not be found:"),
      verbatimTextOutput("text"),
      tags$h5(ifelse(
        is_connect(),
        "Please enter these variables in this document's settings panel under \"Vars\".",
        "Please ensure these variables are available."))
    ),
    server = function(input, output) {
      output$text <- renderText(paste(missing_vars, sep="", collapse=", "))
    }
  )
  runApp(error_app)
}
