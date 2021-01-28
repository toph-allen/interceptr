is_connect <- function() {
  r_config_active <- Sys.getenv("R_CONFIG_ACTIVE")
  logname <- Sys.getenv("LOGNAME")
  return (r_config_active == "rsconnect" || logname == "rstudio-connect")
}
