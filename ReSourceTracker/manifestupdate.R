library(rsconnect)
proj <- "/Users/kylieboenischoakes/Documents/ResourceTracker/ReSourceTracker"   # change to your repo folder; use / not \

rsconnect::writeManifest(
  appDir        = proj,
  appPrimaryDoc = "app.R",
  appFiles      = c("app.R",
                    file.path("data-raw",
                              list.files(file.path(proj, "data-raw"), pattern = "_app\\.rds$")))
)
