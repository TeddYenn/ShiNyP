#' Launch the ShiNyP Shiny app
#'
#' @export
run_ShiNyP <- function() {
  appDir <- system.file("app", package = "ShiNyP")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `ShiNyP`.", call. = FALSE)
  }
  options(warn = -1)
  shiny::runApp(appDir, display.mode = "normal")
  options(warn = -1)
}
