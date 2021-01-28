library(connectapi)
client <- connect()

#> Defining Connect with host: https://colorado.rstudio.com/rsc
# a directory with an app.R and a manifest.json
# if you need a manifest.json, run rsconnect::writeManifest()
# or edit my bundle_dir() code to write the manifest _in_ the temp directory I copy to
# (which is what rsconnect does)

rsconnect::writeManifest("inst/scratch/Sample_Shiny")

bnd <- bundle_dir("inst/scratch/Sample_Shiny")
#> Bundling directory ~/rstudio/connectapi/tests/testthat/examples/shiny
mycontent <- deploy(
  client,
  bnd,
  # name = "my-awesome-special-application",
  title = "What Users See",
  # other content settings like access_type, min_procs, etc.
  .pre_deploy = {
    env <- get_environment(content)
    set_environment_new(env, CONNECT_API_KEY=Sys.getenv("CONNECT_API_KEY"))
  }
)

mycontent %>% poll_task
