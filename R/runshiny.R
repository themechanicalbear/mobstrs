
runshiny <- function() {
  appDir <- system.file("shiny", "Smobstr", package = "Smobstr")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `Smobstr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
