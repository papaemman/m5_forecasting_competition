#################
#               #
#   Set renv    #
#               #
#################

library(renv)


# Init propjects
renv::init()
renv::snapshot()


# Restore
renv::restore()

.libPaths()

renv::paths$root()
renv::paths$library()
renv::paths$cache()

renv::settings$ignored.packages("lightgbm")
renv::settings$ignored.packages()

renv::settings$external.libraries()
renv::settings$use.cache()
renv::settings$package.dependency.fields()

renv::install("microsoft/lightgbm")

renv::rebuild(library = "lightgbm")

renv::paths$root()
renv::paths$library()
renv::paths$cache()


renv::dependencies(
  path = getwd(),
  root = "~/m5_forecasting_competition",
  progress = TRUE,
  errors = "reported",
  dev = FALSE
)


