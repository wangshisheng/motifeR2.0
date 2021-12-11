#' Runs the PTMoreR Shiny web application.
#' @export
motifeR2_app <- function() {
  shiny::runApp(system.file('motifeR2app', package='motifeR2'),
                host=getOption("0.0.0.0"), port =getOption("8989"))
}
