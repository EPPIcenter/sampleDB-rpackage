#' @import shiny tidyverse DT shinyFeedback markdown
#' @export

Run_SampleDB <- function(){
  shiny::runApp(system.file('sampleDB', package='sampleDB'))
}
