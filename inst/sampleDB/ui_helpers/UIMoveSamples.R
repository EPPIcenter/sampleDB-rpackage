UIMoveSamples <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 4,
      HTML("<h4><b>Move Samples</b></h4>"),
      HTML("<p>To move samples please select a storage type and fill out the sections below.</p>"),
      radioButtons("MoveSampleType","Sample Storage Type", c("Micronix" = "micronix", "Cryovial" = "cryovial", "RDT" = "rdt", "Paper" = "paper"), inline = T),
      hr(),
      fileInput("MoveDataSet", "Move Samples File(s)", width = '47%', multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      shinyjs::hidden(textInput("ActionMoveMatrix", label = NULL)),
      fluidRow(column(width = 6, radioButtons("MoveFileType", label = NULL, choices = c("VisionMate" = "visionmate", "Traxcer" = "traxcer", "NA" = "na"), inline = T)),
               column(width = 6, tags$a(href='micronix_format_info.html', target='blank', 'More Info'))),
      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("MoveAction", width = '100%', label = "Move Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
               column(width = 6, actionButton("ClearMoveForm", width = '100%', label = "Clear Form", style="color:#c4244c; background-color: #fff4f4; border-color: #c4244c"))),
      
      br(),
      #output messages
      textOutput("WarningMoveLogisticalColnames"),
      textOutput("WarningMoveBarcodesExist"),
      verbatimTextOutput("MoveReturnMessage1"),
      verbatimTextOutput("MoveReturnMessage2"),
    ),
    mainPanel(
      width = 7,
      HTML("<h2>Guide to Moving Samples</h2>"),
      br(),
      HTML("<h4><code>1. Create a Move File for Each Plate</code></h4>"),
      hr(),
      # HTML("<h4>Currently in the database...</h4>"),
      # fluidRow(
      #   column(width = 6,
      #          tableOutput("InDatabasePlateOne")),
      #   column(width = 6,
      #          tableOutput("InDatabasePlateTwo")),
      # ),
      HTML("Example: Moving samples in plate 1 & plate 2"),
      fluidRow(
        column(width = 6,
               HTML("<h4>Plate 1 Move File</h4>"),
               tableOutput("PlateOneMove")
        ),
        column(width = 6,
               HTML("<h4>Plate 2 Move File</h4>"),
               tableOutput("PlateTwoMove")
        ),
      ),
      br(),
      HTML("<h4><code>2. Name File After Plate Used to Create the File</code></h4>"),
      HTML("<h4><i>platename1.csv</i></h4>"),
      HTML("<h4><i>platename2.csv</i></h4>")
    ))
}