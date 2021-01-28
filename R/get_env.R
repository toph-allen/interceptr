get_env <- function(vars = NULL, context = "rmd") {
  found_vars <- na.omit(Sys.getenv(vars, unset = NA))
  missing_vars <- setdiff(vars, names(found_vars))

  if (length(found_vars) == length(vars)) {
    return(as.list(found_vars))
  } else {
    if (context == "rmd") {
      rmd_missing_vars(missing_vars)
    }
  }
}

rmd_missing_vars <- function(missing_vars) {
  message <- paste(
    "The following environment variables could not be found:",
    missing_vars,
    ifelse(
      is_connect(),
      "Please enter these variable to the right. ->",
      "Please ensure these variables are available"),
    sep = "\n",
    collapse = ", "
  )
  cat(message)
  knitr::knit_exit()
}
