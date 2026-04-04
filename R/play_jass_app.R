#' Launch the Jass Shiny app
#'
#' @description Opens an interactive Shiny app for playing a game of Cross Jass
#'   against one or more AI players. Supports hot-seat play for multiple human
#'   players on the same device.
#'
#' @param ... Arguments passed to \code{\link[shiny]{runApp}} (e.g.
#'   \code{port}, \code{launch.browser}).
#' @return Invisible \code{NULL}. Called for its side effect of launching the app.
#' @export
play_jass_app <- function(...)
{
  if (!requireNamespace("shiny",   quietly = TRUE))
    stop("Package 'shiny' is required. Install it with: install.packages('shiny')")
  if (!requireNamespace("shinyjs", quietly = TRUE))
    stop("Package 'shinyjs' is required. Install it with: install.packages('shinyjs')")

  appDir <- system.file("shiny", "jass", package = "Jass")
  if (!nzchar(appDir))
    stop("Shiny app not found. Is the Jass package installed?")
  shiny::runApp(appDir, ...)
}
