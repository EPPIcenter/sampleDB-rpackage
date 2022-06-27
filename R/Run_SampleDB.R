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
  system("if test -f \"/bin/sampleDB_backup_generator.sh\"; then bash /bin/sampleDB_backup_generator.sh; fi")
  shiny::runApp(system.file('sampleDB', package='sampleDB'))
}
