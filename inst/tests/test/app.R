#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(

    tags$div(sliderInput("slide1", "Slider1", min = 0, max=10, value=4),  style="display:inline-block"),
    tags$div(sliderInput("slide1=2", "Slider2", min = 0, max=10, value=4),  style="display:inline-block"),
    tags$div(sliderInput("slide3", "Slider3", min = 0, max=10, value=4),  style="display:inline-block")

)

server <- function(input, output, session) {

}

shinyApp(ui, server)

# Run the application
shinyApp(ui = ui, server = server)
