#' Run sampleDB as a R Shiny App
#' 
#' @description
#' 
#' @examples
#' \dontrun{
#' RunSampleDB()
#' }
#' @import dplyr
#' @import shiny
#' @import shinyFeedback
#' @import markdown
#' @import lubridate
#' @import shinyWidgets
#' @import shinyjs
#' @export

Run_SampleDB <- function(){
  shiny::runApp(system.file('sampleDB', package='sampleDB'))
}
