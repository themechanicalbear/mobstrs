
runshiny <- function() {
  appDir <- system.file("shiny", "mobstrs", package = "mobstrs")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `mobstrs`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
