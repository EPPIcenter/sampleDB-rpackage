#' Run sampleDB as a R Shiny App
#' 
#' @description Launch sampleDB as a R Shiny application.
#' 
#' @examples
#' \dontrun{
#' RunSampleDB()
#' }
#' @export

Run_SampleDB <- function(){
  # make a copy of the database before each run
  shiny::runApp(system.file('sampleDB', package='sampleDB'))
}
