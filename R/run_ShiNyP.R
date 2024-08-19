# R/run_ShiNyP.R
run_ShiNyP <- function() {
  appDir <- system.file("app", package = "MyShinyApp")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `MyShinyApp`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
