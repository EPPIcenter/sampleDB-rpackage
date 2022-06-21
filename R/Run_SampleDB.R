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
#' @importFrom shinyjs hidden
#' @export

Run_SampleDB <- function(){
  # make a copy of the database before each run
  shiny::runApp(system.file('sampleDB', package='sampleDB'))
}
