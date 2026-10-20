rsconnect::writeManifest(
  appPrimaryDoc = "app.R",
  appFiles = c("app.R",
               list.files("data-raw", pattern = "_app\\.rds$", full.names = TRUE))
)