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
      "Please enter these variable to the right under \"Vars\".",
      "Please ensure these variables are available"),
    sep = "\n"
  )
  cat(message)
  knitr::knit_exit()
}

fail_shiny <- function(missing_vars) {
  return()
}
