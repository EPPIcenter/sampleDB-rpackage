UIMoveSamples <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 4,
      HTML("<h4><b>Move Samples</b></h4>"),
      HTML("<p>To move samples please select a storage type and fill out the sections below.</p>"),
      radioButtons("MoveSampleType","1. Sample Storage Type", c("Micronix" = "micronix", "Cryovial" = "cryovial", "RDT" = "rdt", "Paper" = "paper"), inline = T),
      hr(),
      fileInput("MoveDataSet", "2. Move Samples File(s)", width = '47%', multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      shinyjs::hidden(textInput("ActionMoveMatrix", label = NULL)),
      fluidRow(column(width = 6, radioButtons("MoveFileType", label = NULL, choices = c("VisionMate" = "visionmate", "Traxcer" = "traxcer", "NA" = "na"), inline = T)),
               column(width = 6, tags$a(href='micronix_format_info.html', target='blank', 'More Info'))),
      conditionalPanel(condition = "input.MoveFileType == \"traxcer\"",
                       HTML("Strip Suffix From Filename"),
                       radioButtons("MoveTraxcerStripFromFilename", label = NULL, choices = c("Yes" = "strip", "No" = "no_strip"), selected = "no_strip", inline = T)),
      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("MoveAction", width = '100%', label = "Move Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
               column(width = 6, actionButton("ClearMoveForm", width = '100%', label = "Clear Form", style="color:#c4244c; background-color: #fff4f4; border-color: #c4244c"))),
      
      br(),
      #output messages
      textOutput("WarningMoveLogisticalColnames"),
      textOutput("WarningMoveBarcodesExist"),
      actionButton("CreateEmptyMicronixPlate", "Create Empty Micronix Plate"),
      verbatimTextOutput("CreateEmptyMicronixPlateMessage"),
      verbatimTextOutput("MoveReturnMessage2"),
    ),
    mainPanel(
      width = 7,
      HTML("<h2>Guide to Moving Samples</h2>"),
      br(),
      HTML("<h4><code>1. Choose Sample Storage Type</code></h4>"),
      hr(),
      HTML("<p>Use the <b>Sample Storage Type</b> section to select the sample storage type being moved.</p>"),
      br(),
      HTML("<h4><code>2. Create a Move File for Each Plate</code></h4>"),
      hr(),
      # HTML("<h4>Currently in the database...</h4>"),
      # fluidRow(
      #   column(width = 6,
      #          tableOutput("InDatabasePlateOne")),
      #   column(width = 6,
      #          tableOutput("InDatabasePlateTwo")),
      # ),
      HTML("<h4>A Real World Example:</h4>"),
      br(),
      HTML("<h4>Move samples amoungst Plate1 & Plate2</h4>"),
      HTML("Plate1 and Plate2 move files reflects what the database should contain once the sample storage items have been moved."),
      br(),
      br(),
      fluidRow(
        column(width = 6,
               HTML("<b>Plate1 Move File</b>"),
               tableOutput("PlateOneMove")
        ),
        column(width = 6,
               HTML("<b>Plate2 Move File</b>"),
               tableOutput("PlateTwoMove")
        ),
      ),
      br(),
      HTML("<h4>Save these files in a .csv format, after the plate associated with the data.</h4>"),
      br(),
      HTML("<b>Plate1 Move File -> </b><i>plate1.csv</i>"),
      br(),
      HTML("<b>Plate2 Move File -> </b><i>plate2.csv</i>")
    ))
}