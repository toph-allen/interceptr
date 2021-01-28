get_env <- function(vars) {
  found_vars <- na.omit(Sys.getenv(vars, unset = NA))
  missing_vars <- setdiff(vars, names(found_vars))

  if (length(missing_vars) > 0) {
    print("The following environment variables could not be found:")
    print(missing_vars)
    knitr::knit_exit()
    return()
  } else {
    return(found_vars)
  }
}
