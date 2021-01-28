get_env <- function(vars) {
  sys_env <- Sys.getenv()
  vars <- list()
  missing_vars <- character()

  for (name in vars) {
    if (!name %in% names(sys_env)) {
      missing_vars <- append(missing_vars, name)
    } else {
      vars[name] <- sys_env[name]
    }
  }

  if (length(missing_vars) > 0) {
    print("The following environment variables could not be found:")
    print(missing_vars)
    knitr::knit_exit()
    return()
  } else {
    return(vars)
  }
}
