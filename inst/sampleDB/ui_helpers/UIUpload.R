

UIUpload <- function(){
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      HTML("<h4><b>Upload Samples</b></h4>"),
      HTML("<p>To upload samples please select a storage type and fill out the sections below.</p>"),
      radioButtons("UploadSampleType","1. Sample Storage Type", c("Micronix" = "micronix", "Cryovial" = "cryovial", "RDT" = "rdt", "Paper" = "paper"), inline = T),
      hr(),
      #upload data
      fileInput("UploadSampleDataSet", "2. Upload Samples File", width = '47%', multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      conditionalPanel(condition = "input.UploadSampleType == \"micronix\"",
                fluidRow(column(width = 6, radioButtons("UploadFileType", label = NULL, choices = c("VisionMate" = "visionmate", "Traxcer" = "traxcer", "NA" = "na"), inline = T)),
                         column(width = 6, tags$a(href='micronix_format_info.html', target='blank', 'More Info'))
                )
      ),
      conditionalPanel(condition = "input.UploadSampleType == \"cryovial\"",
                fluidRow( column(width = 6, radioButtons("UploadFileType", label = NULL, choices = c("NA" = "na"), inline = T)),
                          column(width = 6, tags$a(href='micronix_format_info.html', target='blank', 'More Info'))
                )
      ),
      textOutput("WarningMicronixSpecimenExists"),
      #insert plate infor
      conditionalPanel(condition = "input.UploadSampleType == \"micronix\"",
        HTML("<h5><b>3. Plate Name</b></h5>"),
        fluidRow(column(width = 6, HTML("<p>Human Readable Name</p>"), selectizeInput("UploadStorageContainerDestID", label = NULL, choices = c("", sampleDB::CheckTable(database = database, "micronix_plate")$name), options = list(create = TRUE))),
                 column(width = 6,  HTML("<p>Barcode (Optional)</p>"), textInput("UploadStorageContainerDestBarcode", label = NULL)))
      ),
      conditionalPanel(condition = "input.UploadSampleType == \"cryovial\"",
        HTML("<h5><b>3. Box Name</b></h5>"),
        fluidRow(column(width = 6, HTML("<p>Human Readable Name</p>"), selectizeInput("UploadStorageContainerDestID", label = NULL, choices = c("", sampleDB::CheckTable(database = database, "cryovial_box")$name), , options = list(create = TRUE))),
                 column(width = 6,  HTML("<p>Barcode (Optional)</p>"), textInput("UploadStorageContainerDestBarcode", label = NULL)))
      ),
      textOutput("WarningMicronixUploadContainerName"), 
      textOutput("WarningMicronixUploadContainerBarcode"),
      #add freezer infor
      HTML("<h5><b>4. Freezer Address</b></h5>"),
      HTML("<p>Freezer Name</p>"), selectInput("UploadStorageContainerDestLocation", label = NULL, width = '47%', choices = c("", sampleDB::CheckTable(database = database, "location")$name) %>% sort()),
      HTML("<p>Shelf Name</p>"), selectInput("UploadStorageContainerDestLocationLevelI", label = NULL, width = '47%', choices = NULL),
      HTML("<p>Basket Name</p>"), selectInput("UploadStorageContainerDestLocationLevelII", label = NULL, width = '47%', choices = NULL),
      
      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("UploadAction", width = '100%', label = "Upload Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
               column(width = 6, actionButton("ClearUploadForm", width = '100%', label = "Clear Form", style="color:#c4244c; background-color: #fff4f4; border-color: #c4244c"))),
      
      br(),
      #output messages
      span(verbatimTextOutput("UploadOutputConsole"), style="font-size: 28px")
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
}