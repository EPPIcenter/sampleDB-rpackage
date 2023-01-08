library(DBI)
UIUploadSamples <- function() {
  
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 3,
      h4("Upload Samples"),
      radioButtons("UploadSampleType","Sample Storage Type", choices = DBI::dbReadTable(con, "sample_type") %>% pull(id, name = "name"), inline = TRUE),
      hr(),
      radioButtons("UploadFileType", "Choose a file type", choices = c("NA" = "na", "Traxcer" = "traxcer", "VisionMate" = "visionmate"), inline = TRUE),
      #upload data
      fileInput("UploadSampleDataSet", "Upload Samples File", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      #add freezer infor
      # selectizeInput("UploadManifestName", label = "Plate Name", choices = c(),  options = list(create = TRUE)),
      # selectInput("UploadLocationRoot", label = "Upload Location", choices = c()),        
      # selectInput("UploadLocationLevelI", label = "Shelf Name", choices = c()),
      # selectInput("UploadLocationLevelII", label = "Basket Name", choices = c()),
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
      tags$head(
        tags$style(
          HTML(".shiny-notification {
               position:fixed;
               top: calc(50%);
               left: calc(50%);
               }
               "
              )
          )
      ),
      width = 7,
      HTML("<h2>Guide to Uploading Micronix Samples</h2>"),
      br(),
      HTML("<h4><code>1. Choose Sample Storage Type</code></h4>"),
      hr(),
      HTML("<p>Use the <b>Sample Storage Type</b> section to select the storage type for uploading.</p>"),
      br(),
      HTML("<h4><code>2. Create Micronix Data File</code></h4>"),
      hr(),
      HTML("<p>Combine 2 pieces of information, <i>logistical information</i> and <i>metadata information</i>, to form a micronix data file.</p>"),
      fluidRow(
        column(width = 6,
               HTML("<h4>Logistical Information</h4>"),
               HTML("<p>Plate position and barcode.</p>"),
               tableOutput("LogisticsItems")),
        column(width = 6,
               HTML("<h4>Metadata Information</h4>"),
               HTML("<p>Assay specific information.</p>"),
               tableOutput("MetadataItems")),
      ),
      # HTML("<p>...in order to form a micronix data file."),
      HTML("<h4>Micronix Data File</h4>"),
      tableOutput("CombinedItems"),
      HTML("<p>Upload this file to the <b>Upload Samples File</b> section.</p>"),
      br(),
      HTML("<h4><code>3. Create a Plate Name</code></h4>"),
      hr(),
      HTML("<p>Input in the <b>Human Readable Name</b> section, the plate name that will be written on the plate. Plate names must follow the form: <i>Study Code-Year-Plate Number</i>.</p>"),
      HTML("<p>In addition to human readable names, plate barcodes can be added to keep track of plates. Use the <b>Barcode</b> section to input a plate barcode.</i></b></p>"),
      br(),
      HTML("<h4><code>4. Select Freezer Address for Sample Storage</code></h4>"),
      hr(),
      HTML("<p>Use the <b>Freezer Address</b> section to select the freezer address that the samples will be stored in.</p>"),
      br(),
    ))

    DBI::dbDisconnect(con)

    return(ui)
}