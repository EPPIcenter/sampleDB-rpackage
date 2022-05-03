

UIMicronixUpload <- function(){
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      HTML("<h4><b>Upload Samples</b></h4>"),
      HTML("<p>To upload samples please select a storage type and fill out the sections below.</p>"),
      radioButtons("UploadSampleType","Sample Storage Type", c("Micronix" = "micronix", "Cryovial" = "cryovial", "RDT" = "rdt", "Paper" = "paper"), inline = T),
      hr(),
      #upload data
      fileInput("UploadMicronixDataSet", "Upload Samples File", width = '47%', multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fluidRow(column(width = 6, radioButtons("MicronixFileType", label = NULL, choices = c("VisionMate" = "visionmate", "Traxcer" = "traxcer", "NA" = "na"), inline = T)),
               column(width = 6, tags$a(href='micronix_format_info.html', target='blank', 'More Info'))),
      shinyjs::hidden(textInput("ActionUploadMatrix", label = NULL)),
      textOutput("WarningMicronixUploadSampleID"),
      textOutput("WarningMicronixUploadBarcodeRepeats"),
      textOutput("WarningMicronixUploadLogisticalColnames"),
      textOutput("WarningMicronixUploadMetadataColnames"),
      textOutput("WarningUploadMicronixSpecimenTypes"),
      textOutput("WarningMicronixUploadDateFormat"),
      textOutput("WarningUploadMicronixStudyShortCodes"),
      textOutput("WarningMicronixSpecimenExists"),
      #insert plate infor
      HTML("<h5><b>Plate Name</b></h5>"),
      fluidRow(column(width = 6, HTML("<p>Human Readable Name</p>"), textInput("UploadMicronixPlateID", label = NULL, placeholder = "PRISM-2022-001")),
               column(width = 6,  HTML("<p>Barcode (Optional)</p>"), textInput("UploadMicronixPlateBarcode", label = NULL))),
      textOutput("WarningMicronixUploadContainerName"), 
      textOutput("WarningMicronixUploadContainerBarcode"),
      #add freezer infor
      HTML("<h5><b>Freezer Address</b></h5>"),
      HTML("<p>Freezer Name</p>"), selectInput("UploadMicronixLocation", label = NULL, width = '47%', choices = c("", sampleDB::CheckTable(database = database, "location")$location_name) %>% sort()),
      HTML("<p>Shelf Name</p>"), selectInput("UploadLocationMicronixLevelI", label = NULL, width = '47%', choices = NULL),
      HTML("<p>Basket Name</p>"), selectInput("UploadLocationMicronixLevelII", label = NULL, width = '47%', choices = NULL),
      
      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("UploadMicronixActionButton", width = '100%', label = "Upload Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
               column(width = 6, actionButton("ClearMicronixUploadForm", width = '100%', label = "Clear Form", style="color:#c4244c; background-color: #fff4f4; border-color: #c4244c"))),
      
      br(),
      #output messages
      span(verbatimTextOutput("UploadMicronixReturnMessage1"), style="font-size: 28px"),
      span(verbatimTextOutput("UploadMicronixReturnMessage2"), style="font-size: 28px")
    ),
    mainPanel(
      width = 7,
      HTML("<h2>Guide to Uploading Micronix Samples</h2>"),
      br(),
      HTML("<h4><code>1. Create Micronix Data File</code></h4>"),
      hr(),
      HTML("<p>Combine 2 pieces of information, <i>storage information</i> and <i>metadata information</i>, to form a micronix data file.</p>"),
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
      br(),
      HTML("<h4><code>2. Create a Plate Name</code></h4>"),
      hr(),
      HTML("<p>Input a plate name following the form: <b><i>Study Code-Year-Plate Number</i></b>.</p>"),
      HTML("<p>Plate barcodes can also be to name plates.</i></b></p>"),
      br(),
      HTML("<h4><code>3. Select Freezer Address for Sample Storage</code></h4>"),
      hr(),
      HTML("<p>Select the freezer address that the samples will be stored in.</p>"),
      br(),
    ))
}