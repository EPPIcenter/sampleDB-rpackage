library(DBI)
library(shinyBS)
library(reactable)

UIUploadSamples <- function() {
  
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  file_specs_json <- rjson::fromJSON(file = system.file(
    "extdata", "file_specifications.json", package = .sampleDB$pkgname))

  file_type_ids <- lapply(file_specs_json$file_types, function(x) x$id)
  names(file_type_ids) <- lapply(file_specs_json$file_types, function(x) x$name)

  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 3,
      h4("Upload Samples"),
      radioButtons("UploadSampleType","Sample Storage Type", choices = DBI::dbReadTable(con, "sample_type") %>% pull(id, name = "name"), inline = TRUE),
      hr(),
      radioButtons("UploadFileType", "Choose a file type", choices = file_type_ids, inline = TRUE),
      #upload data
      fileInput("UploadSampleDataSet", "Upload Samples File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      #add freezer infor
      shinyjs::hidden(selectizeInput("UploadManifestName", label = "Plate Name", choices = c(),  options = list(create = TRUE))),
      shinyjs::hidden(selectInput("UploadLocationRoot", label = "Upload Location", choices = c())),        
      shinyjs::hidden(selectInput("UploadLocationLevelI", label = "Shelf Name", choices = c())),
      shinyjs::hidden(selectInput("UploadLocationLevelII", label = "Basket Name", choices = c())),
      #output messages
      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("UploadAction", width = '100%', label = "Upload Samples")),
               column(width = 6, actionButton("ClearUploadForm", width = '100%', label = "Clear Form"))
      ),
      br(),
      span(verbatimTextOutput("UploadOutputConsole"))
    ),
    mainPanel(
      tags$div(style = "width: 100%",
        tags$h3("Guide to Uploading Samples"),
        tags$h4("1. Choose Sample Storage Type"),
        tags$p("Use the", tags$strong("Sample Storage Type"), "section to select the storage type for uploading."),
        tags$h4("2. Create Data File"),   
        tags$p("Some sample storage types will accept multiple file formats, but the `NA` file format will always be available. To see what columns are required and optional, you can change the file type in the left hand panel and the table below will update accordingly."),
        tags$h5("Required Fields"),
        tags$p("Below are", tags$strong("required"), "columns that", tags$strong("must"), "be included in your file."),
        reactableOutput("UploadFileExampleRequired"),
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