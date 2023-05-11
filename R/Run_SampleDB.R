#' Run sampleDB as a R Shiny App
#' 
#' @description Launch sampleDB as a R Shiny application.
#' 
#' @examples
#' \dontrun{
#' RunSampleDB()
#' }
#' @export

Run_SampleDB <- function(host="0.0.0.0", port=3838) {
  message("Starting up application...")
  # make a copy of the database before each run
  shiny::runApp(system.file('sampleDB', package='sampleDB'), host=host, port=port)
}
