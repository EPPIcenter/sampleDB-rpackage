library(DBI)

UIMoveSamples <- function(){

  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  ui <- sidebarLayout(
    sidebarPanel(
      width = 4,
      HTML("<h4><b>Move Samples</b></h4>"),
      HTML("<p>To move samples please select a storage type and fill out the sections below.</p>"),
      radioButtons("MoveSampleType","1. Sample Storage Type", get_sample_types(), inline = T),
      hr(),
      radioButtons("MoveFileType", label = "2. Move File Type", choices = c("NA"="na"), inline = T),
      
      conditionalPanel(
        condition = "input.MoveFileType == 'traxcer'",
        checkboxInput("MoveTraxcerStripFromFilename", "Remove datetime strip from filename",value = TRUE, width = NULL)
      ),
      fileInput("MoveDataSet", "3. Move Samples File", width = '47%', multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("MoveAction", width = '100%', label = "Move Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
               column(width = 6, actionButton("ClearMoveForm", width = '100%', label = "Clear Form", style="color:#c4244c; background-color: #fff4f4; border-color: #c4244c"))),
      
      br(),
      actionButton("CreateNewManifest", label = "Create Container"),
      hr(),
      #output messages
      verbatimTextOutput("MoveOutputConsole"),
    ),
    mainPanel(
      tags$div(style = "width: 100%",
        tags$h3("Guide to Moving Samples"),
        tags$h4("1. Select a Sample Storage Type"),
        tags$p("Use the", tags$strong("Sample Storage Type"), "section to select the storage type."),
        tags$h4("2. Create a Sample Move File to Upload"),
        tags$p("Create a file that follows the template below and", tags$strong("name the file the name of the container."), "For example, if you are creating a move file for a container named", tags$em("LN2_XXXX"), ", your file should be named", tags$em("LN2_XXXX.csv")),
        tags$p("If you would like to download a template file, press the button below."),
        downloadButton("MoveFileTemplate"),
        tags$h5("Required Fields"),
        tags$p("Below are", tags$strong("required"), "columns that", tags$strong("must"), "be included in your file."),
        reactableOutput("MoveFileExampleRequired"),
        tags$h4(tags$strong("Important")),
        tags$p("The destination container must exist in the database before moving the file. Use ", tags$strong("Create Container"), " to create an empty container.")
    )))

  dbDisconnect(con)

  return(ui)
}