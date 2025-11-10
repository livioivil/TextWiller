#' Run TextWiller Shiny Application
#' 
#' Launches the TextWiller Shiny application for interactive text analysis.
#' 
#' @param ... Additional parameters passed to \code{shiny::runApp}
#' @export
run_app <- function(...) {
  app_dir <- system.file("apps/textwiller_app", package = "TextWiller3")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `TextWiller3`.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, ...)
}