library(DBI)
library(reactable)
library(shinyjs)

UIUploadSamples <- function() {
  
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 3,
      radioButtons("UploadType", "Choose Your Upload Type", choices = global_upload_type_list),
      conditionalPanel(
        condition = "input.UploadType == 'samples'",
        h4("Upload Samples"),
        radioButtons("UploadSampleType","Sample Storage Type", choices = global_sample_names_ids_list, inline = TRUE),
        hr(),
        radioButtons("UploadFileType", "Choose a file type", choices = global_sample_file_types[["micronix"]], inline = TRUE),
        #upload data
        fileInput("UploadSampleDataSet", "Upload Samples File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        #add freezer infor
        shinyjs::hidden(selectizeInput("UploadManifestName", label = "Plate Name", choices = c(),  options = list(create = TRUE))),
        shinyjs::hidden(selectInput("UploadLocationRoot", label = "Upload Location", choices = c())),        
        shinyjs::hidden(selectInput("UploadLocationLevelI", label = "Shelf Name", choices = c())),
        shinyjs::hidden(selectInput("UploadLocationLevelII", label = "Basket Name", choices = c()))
      ),
      conditionalPanel(
        condition = "input.UploadType == 'Controls'",
        h4("Upload Controls"),
        radioButtons("UploadControlType","Control Storage Type", choices = c("DBS Sheet" = "dbs_sheet", "Whole Blood" = "whole_blood"), inline = TRUE),
        hr(),
        radioButtons("UploadControlAction", "Action", choices = c("Created" = "create", "Extracted" = "extraction"), inline = TRUE),
        fileInput("InputUploadControls", label = "Upload DBS Control Sheet", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("UploadAction", width = '100%', label = "Upload")),
               column(width = 6, actionButton("ClearUploadForm", width = '100%', label = "Clear Form"))),
      span(verbatimTextOutput("UploadOutputConsole"))
    ),
    mainPanel(
      tags$div(style = "width: 100%",
        tags$h3("Guide to Uploading Samples"),
        tags$h4("1. Select a Sample Storage Type"),
        tags$p("Use the", tags$strong("Sample Storage Type"), "section to select the storage type for uploading."),
        tags$h4("2. Create a Sample Data File to Upload"),   
        tags$p("Some sample storage types will accept multiple file formats, but the", tags$strong("NA"), "file format will always be available. To see what columns are required and optional, you can change the file type in the left hand panel and the tables below will update accordingly."),
        tags$p("If you would like to download a template file, press the button below."),
        downloadButton("UploadFileTemplate"),
        tags$h5("Required Fields"),
        tags$p("Below are", tags$strong("required"), "columns that", tags$strong("must"), "be included in your file."),
        reactableOutput("UploadFileExampleRequired"),
        tags$br(),
        tags$p("Some fields may be included in your file", tags$strong("or"), "can be entered after upload."),
        reactableOutput("UploadFileExampleUserInput"),
        tags$h5("Conditional Fields"),
        tags$p("Some fields", tags$strong("may"), "be required depending on the contents of your upload."),
        reactableOutput("UploadFileExampleConditional"),
        tags$h5("Optional Fields"),
        tags$p("The columns below do not need to be included in your upload."),
        reactableOutput("UploadFileExampleOptional")
    )))

    DBI::dbDisconnect(con)

    return(ui)
}