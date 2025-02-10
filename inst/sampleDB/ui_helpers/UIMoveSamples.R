library(DBI)
library(shinyjs)
library(reactable)

UIMoveSamples <- function() {
  
  ui <- sidebarLayout(
    sidebarPanel(
      width = 4,
      shinyjs::useShinyjs(), # Enables the use of shinyjs
      HTML("<h4><b>Move Samples</b></h4>"),
      HTML("<p>To move samples please select a storage type and fill out the sections below.</p>"),
      
      # Move Type radio button
      radioButtons("MoveType", "1. Type", global_upload_type_list, inline = TRUE),
      
      # Conditional panels based on MoveType selection
      conditionalPanel(
        condition = "input.MoveType == 'samples'",
        # Sample Storage Type radio button
        radioButtons("MoveSampleType", "2. Sample Storage Type", get_sample_types(), inline = TRUE),
        # conditionalPanel(
        #   condition = "input.MoveSampleType == 'dbs_sample'",
        #   radioButtons("MoveDBSSampleType", "Choose the container type", choices = list("Box" = "box", "Bag" = "bag"), inline = TRUE)
        # ),
        hr(),
        
        # Move File Type radio button
        radioButtons("MoveFileType", "3. Move File Type", choices = c("NA" = "na", "traxcer" = "traxcer"), inline = TRUE),
        
        # Conditional panel for 'traxcer' option
        conditionalPanel(
          condition = "input.MoveFileType == 'traxcer'",
          checkboxInput("MoveTraxcerStripFromFilename", "Remove datetime strip from filename", value = TRUE, width = NULL)
        ),
        
        # File input for move samples file
        fileInput("MoveDataSet", "4. Move Samples File", width = '47%', multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      
      # Conditional panel for 'controls' MoveType
      conditionalPanel(
        condition = "input.MoveType == 'controls'",
        h4("Move Controls"),
        radioButtons("MoveControlType", "Control Storage Type", choices = get_all_control_types(), selected = "dbs_sheet", inline = TRUE),
        fileInput("MoveControlDataSet", label = "Upload DBS Control Sheet", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      
      hr(),
      
      # Action buttons
      fluidRow(
        column(width = 6, actionButton("MoveAction", width = '100%', label = "Move Samples")),
        column(width = 6, actionButton("ClearMoveForm", width = '100%', label = "Clear Form"))
      ),
      
      br(),
      actionButton("CreateNewManifest", label = "Create Empty Container"),
      actionButton("DeleteEmptyManifest", label = "Delete Empty Container"),
      hr(),
      
      # Output messages
      verbatimTextOutput("MoveOutputConsole")
    ),
    
    mainPanel(
      tags$div(
        style = "width: 100%",
        tags$h3("Guide to Moving Samples"),
        tags$h4("1. Select a Sample Storage Type"),
        tags$p("Use the", tags$strong("Sample Storage Type"), "section to select the storage type."),
        tags$h4("2. Create a Sample Move File to Upload"),
        tags$p("Create a file that follows the template below and", tags$strong("name the file the name of the container."), "For example, if you are creating a move file for a container named", tags$em("LN2_XXXX"), ", your file should be named", tags$em("LN2_XXXX.csv")),
        tags$p("If you would like to download a template file, press the button below."),
        uiOutput("MoveFileTemplatePlaceholder"),
        
        tags$h5("Required Fields"),
        tags$p("Below are", tags$strong("required"), "columns that", tags$strong("must"), "be included in your file."),
        reactableOutput("MoveFileExampleRequired"),
        
        tags$h4(tags$strong("Important")),
        tags$p("The destination container must exist in the database before moving the file. Use ", tags$strong("Create Container"), " to create an empty container.")
      )
    )
  )
  
  return(ui)
}
