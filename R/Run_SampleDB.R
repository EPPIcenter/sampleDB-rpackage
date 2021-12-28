#' @import dplyr
#' @import shiny
#' @import DT
#' @import shinyFeedback
#' @import markdown
#' @import lubridate
#' @export

Run_SampleDB <- function(){
  shiny::runApp(system.file('sampleDB', package='sampleDB'))
}
