runShiNyP <- function() {
  app_dir <- system.file("app", package = "ShiNyP")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing `ShiNyP`.", call. = FALSE)
  }
  shiny::runShiNyP(app_dir, display.mode = "normal")
}