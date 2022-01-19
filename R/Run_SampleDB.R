#' @import dplyr
#' @import shiny
#' @import shinyFeedback
#' @import markdown
#' @import lubridate
#' @import shinyWidgets
#' @export

Run_SampleDB <- function(){
  shiny::runApp(system.file('sampleDB', package='sampleDB'))
}
