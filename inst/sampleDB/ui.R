library(dplyr)
library(sampleDB)
library(shinyFeedback)
library(shiny)
library(markdown)
library(DT)
library(shinyWidgets)


#SET PATH TO SQLITE DATABASE
database <- Sys.getenv("SDB_PATH")

navbarPage("EPPIcenter SampleDB",
           
           navbarMenu("Upload Samples",
           tabPanel("Micronix",

                    #css setup
                    shinyFeedback::useShinyFeedback(),
                    shinyjs::useShinyjs(),
                    tags$style(".shiny-file-input-progress {display: none}"),
                    tags$head(
                      tags$style(HTML("
                        .progress-bar {
                            color: transparent!important
                        }
                        h5 {
                            line-height: 150%;
                        }
                        ::-webkit-input-placeholder {
                            font-style: italic;
                        }
                        :-moz-placeholder {
                            font-style: italic;
                        }
                        ::-moz-placeholder {
                            font-style: italic;
                        }
                        :-ms-input-placeholder {  
                          font-style: italic; 
                        }
                        .shiny-output-error-validation {
                              color: #c4244c; font-weight: normal;
                        }"))),
                    
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        HTML("<h4><b>Upload Micronix Samples</b></h4><p>Please fill out the sections below to upload micronix samples.</p>"),
                        hr(),
                        #upload data
                        fileInput("UploadMicronixDataSet", "Micronix Data File", width = '47%', multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        fluidRow(column(width = 6, radioButtons("UploadFileType", label = NULL, choices = c("VisionMate" = "visionmate", "Traxcer" = "traxcer", "NA" = "na"), inline = T),
                                                   tags$a(href='micronix_format_info.html', target='blank', 'More Info'))),
                        shinyjs::hidden(textInput("ActionUploadMatrix", label = NULL)),
                        textOutput("WarningMicronixUploadSampleID"),
                        textOutput("WarningMicronixUploadBarcodeRepeats"),
                        textOutput("WarningMicronixUploadColnames"),
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
                                 HTML("<h4>Sample Storage Information</h4>"),
                                 HTML("<p>Plate position and barcode.</p>"),
                                 tableOutput("LogisticsItems")),
                          column(width = 6,
                                 HTML("<h4>Sample Meta Data Information</h4>"),
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
                      ))),
           
           tabPanel("Cryovial",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        HTML("<h4><b>Upload Cryovial Samples Form</b></h4>"),
                        hr(),
                        fileInput("UploadCryoDataSet", "UploadCryoCSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        textOutput("WarningCryoUploadSampleID"),
                        textOutput("WarningCryoUploadColnames"),
                        textOutput("WarningUploadCryoSpecimenTypes"),
                        textOutput("WarningCryoUploadDateFormat"),
                        textOutput("WarningUploadCryoStudyShortCodes"),
                        textOutput("WarningCryoSpecimenExists"),
                        textInput("UploadCryoPlateID", label = "Box Name"),
                        textOutput("WarningCryoUploadContainer"),
                        selectInput("UploadLocationCryoFreezerName", label = "Storage Location", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        fluidRow(column(width = 1), column(width = 11, selectInput("UploadLocationCryoLevelI", label = HTML("<h5>Storage Location: Level I</h5>"), width = '100%', choices = NULL))),
                        fluidRow(column(width = 1), column(width = 11, selectInput("UploadLocationCryoLevelII", label = HTML("<h5>Storage Location: Level II</h5>"), width = '100%', choices = NULL))),
                        hr(),
                        fluidRow(column(width = 12,
                                        actionButton("UploadCryoAction", width = '49%', label = "Upload Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton("ClearCryoUploadForm", width = '49%', label = "Clear Form"))),
                        br(),
                        span(verbatimTextOutput("UploadCryoReturnMessage1"), style="font-size: 28px"),
                        span(verbatimTextOutput("UploadCryoReturnMessage2"), style="font-size: 28px")
                      ),
                      mainPanel(
                        width = 9,
                        HTML("<h4><center>This is an <b>Example SampleDB UploadCryoCSV</b>.</center></h4>"),
                        fluidRow(
                          column(width = 2),
                          column(width = 8,
                                 span(verbatimTextOutput("ExampleUploadCryoCSVNoDate"), style ="max-width: 75px; text-align: center"),
                          )),
                        fluidPage(
                          column(width = 1),
                          column(width = 10,
                        HTML("<h4><b>Creating an UploadMicronixCSV</b></h4>
                        <h5>The combination of <code>study_subject</code>, <code>specimen_type</code> and <code>study_code</code> must be unique.
                             Consider adding a collection_date for the sample</h5><h4><b>Longitudinal Data</b></h4>
                             <h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                             longitudinal information to the samples. (Date format is YMD.)</h5>
                             <center><h4><b>Example SampleDB UploadCryoCSV</b> with <code>collection_date</code> Column</h4></center>"),
                          ),
                        column(width = 1)
                        ),
                        fluidRow(
                          column(width = 2),
                          column(width = 8,
                                 span(verbatimTextOutput("ExampleUploadCryoCSVDate"), style ="max-width: 75px; text-align: center"),
                          )),
                        # HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code>, <code>study_code</code> and <code>collection_date</code> must be unique</h5>")
                      ))),
           
           tabPanel("RDT",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        HTML("<h4><b>Upload RDT Samples Form</b></h4>"),
                        hr(),
                        fileInput("UploadRDTDataSet", "UploadRDTCSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        textOutput("WarningRDTUploadSampleID"),
                        textOutput("WarningRDTUploadColnames"),
                        textOutput("WarningUploadRDTSpecimenTypes"),
                        textOutput("WarningRDTUploadDateFormat"),
                        textOutput("WarningUploadRDTStudyShortCodes"),
                        textOutput("WarningRDTSpecimenExists"),
                        textInput("UploadRDTPlateID", label = "Bag Name"),
                        textOutput("WarningRDTUploadContainer"),
                        selectInput("UploadLocationRDTFreezerName", label = "Storage Location", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        fluidRow(column(width = 1), column(width = 11, selectInput("UploadLocationRDTLevelI", label = HTML("<h5>Storage Location: Level I</h5>"), width = '100%', choices = NULL))),
                        fluidRow(column(width = 1), column(width = 11, selectInput("UploadLocationRDTLevelII", label = HTML("<h5>Storage Location: Level II</h5>"), width = '100%', choices = NULL))),
                        hr(),
                        fluidRow(column(width = 12,
                                        actionButton("UploadRDTAction", width = '49%', label = "Upload Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton("ClearRDTUploadForm", width = '49%', label = "Clear Form"))),
                        br(),
                        span(verbatimTextOutput("UploadRDTReturnMessage1"), style="font-size: 28px"),
                        span(verbatimTextOutput("UploadRDTReturnMessage2"), style="font-size: 28px")
                      ),
                      mainPanel(
                        width = 9,
                        HTML("<h4><center>This is an <b>Example SampleDB UploadRDTCSV</b>.</center></h4>"),
                        fluidRow(
                          column(width = 2),
                          column(width = 8,
                                 span(verbatimTextOutput("ExampleUploadRDTCSVNoDate"), style ="max-width: 75px; text-align: center"),
                          )),
                        fluidPage(
                          column(width = 1),
                          column(width = 10,
                                 HTML("<h4><b>Creating an UploadMicronixCSV</b></h4>
                                 <h5>The combination of <code>study_subject</code>, <code>specimen_type</code> and <code>study_code</code> must be unique.
                             Consider adding a collection_date for the sample</h5>
                             <h4><b>Longitudinal Data</b></h4>
                             <h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                             longitudinal information to the samples. (Date format is YMD.)</h5>
                             <center><h4><b>Example SampleDB UploadRDTCSV</b> with <code>collection_date</code> Column</h4></center>"),
                          ),
                          column(width = 1)
                        ),
                        fluidRow(
                          column(width = 2),
                          column(width = 8,
                                 span(verbatimTextOutput("ExampleUploadRDTCSVDate"), style ="max-width: 75px; text-align: center"),
                          )),
                        # HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code>, <code>study_code</code> and <code>collection_date</code> must be unique</h5>")
                      ))),
           tabPanel("Dried Blood Spot",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        HTML("<h4><b>Upload Paper Samples Form</b></h4>"),
                        hr(),
                        fileInput("UploadPaperDataSet", "UploadPaperCSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        textOutput("WarningPaperUploadSampleID"),
                        textOutput("WarningPaperUploadColnames"),
                        textOutput("WarningUploadPaperSpecimenTypes"),
                        textOutput("WarningPaperUploadDateFormat"),
                        textOutput("WarningUploadPaperStudyShortCodes"),
                        textOutput("WarningPaperSpecimenExists"),
                        textInput("UploadPaperPlateID", label = "Bag Name"),
                        textOutput("WarningPaperUploadContainer"),
                        selectInput("UploadLocationPaperFreezerName", label = "Storage Location", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        fluidRow(column(width = 1), column(width = 11, selectInput("UploadLocationPaperLevelI", label = HTML("<h5>Storage Location: Level I</h5>"), width = '100%', choices = NULL))),
                        fluidRow(column(width = 1), column(width = 11, selectInput("UploadLocationPaperLevelII", label = HTML("<h5>Storage Location: Level II</h5>"), width = '100%', choices = NULL))),
                        hr(),
                        fluidRow(column(width = 12,
                                        actionButton("UploadActionPaperSamples", width = '49%', label = "Upload Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton("ClearUploadFormPaperSamples", width = '49%', label = "Clear Form"))),
                        br(),
                        span(verbatimTextOutput("UploadPaperReturnMessage1"), style="font-size: 28px"),
                        span(verbatimTextOutput("UploadPaperReturnMessage2"), style="font-size: 28px")
                      ),
                      mainPanel(
                        width = 9,
                        HTML("<h4><center>This is an <b>Example SampleDB UploadPaperCSV</b>.</center></h4>"),
                        fluidRow(
                          column(width = 2),
                          column(width = 8,
                                 span(verbatimTextOutput("ExampleUploadPaperCSVNoDate"), style ="max-width: 75px; text-align: center"),
                          )),
                        fluidPage(
                          column(width = 1),
                          column(width = 10,
                                 HTML("<h4><b>Creating an UploadMicronixCSV</b></h4>
                                 <h5>The combination of <code>study_subject</code>, <code>specimen_type</code> and <code>study_code</code> must be unique.
                             Consider adding a collection_date for the sample</h5>
                             <h4><b>Longitudinal Data</b></h4>
                             <h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                             longitudinal information to the samples. (Date format is YMD.)</h5>
                             <center><h4><b>Example SampleDB UploadPaperCSV</b> with <code>collection_date</code> Column</h4></center>"),
                          ),
                          column(width = 1)
                        ),
                        fluidRow(
                          column(width = 2),
                          column(width = 8,
                                 span(verbatimTextOutput("ExampleUploadPaperCSVDate"), style ="max-width: 75px; text-align: center"),
                          )),
                        # HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code>, <code>study_code</code> and <code>collection_date</code> must be unique</h5>")
                        ))),
           ),

           tabPanel("Search Samples",
                    sidebarLayout(
                      sidebarPanel(
                        width = 2,
                        HTML("<h4>Search Samples</h4>"),
                        hr(),
                        # fileInput("SearchByLabel", label = HTML("Barcode <h6>Single column named \"barcode\"</h6>")), actionButton("ClearSearchBarcodes", label = "Clear Barcodes"), textOutput("WarnSubjectBarcodeFileColnames"), textOutput("WarnSubjectBarcodeFileColnames2"),
                        radioButtons("SearchBySampleType","Sample Type", c("All" = "all", "Micronix" = "micronix", "Cryovial" = "cryo", "RDT" = "rdt", "Paper" = "paper"), inline = T),
                        conditionalPanel(condition = "input.SearchBySampleType == \"micronix\"",
                                         fileInput("SearchByBarcode", label = "Sample Barcodes"),
                                         selectInput("SearchByPlate", label = "Plate Name", choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name))),
                        conditionalPanel(condition = "input.SearchBySampleType == \"cryo\"",
                                         fileInput("SearchByCryovialLabels", label = "Sample Labels"),
                                         selectInput("SearchByBox", label = "Box Name", choices = c("", sampleDB::CheckTable(database = database, "box")$box_name))),
                        conditionalPanel(condition = "input.SearchBySampleType == \"rdt\"",
                                         fileInput("SearchByRDTLabels", label = "Sample Labels"),
                                         selectInput("SearchByRDTBag", label = "Bag Name", choices = c("", sampleDB::CheckTable(database = database, "bag")$bag_name))),
                        conditionalPanel(condition = "input.SearchBySampleType == \"paper\"",
                                         fileInput("SearchByPaperLabels", label = "Sample Labels"),
                                         selectInput("SearchByPaperBag", label = "Bag Name", choices = c("", sampleDB::CheckTable(database = database, "bag")$bag_name))),
                        hr(),
                        selectizeInput("SearchByStudy", "Study", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code)),
                        radioButtons("SubjectUIDSearchType", label = "Study Subject Search Method", choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"), selected = "individual"),
                        conditionalPanel(condition = "input.SubjectUIDSearchType == \"individual\"",
                                         selectizeInput("SearchBySubjectUID", label = "Study Subject", choices = c())),
                        conditionalPanel(condition = "input.SubjectUIDSearchType == \"multiple\"",
                                         fileInput("SearchBySubjectUIDFile", label = HTML("Study Subjects <h6>Single column named \"subject_uid\"</h6>")), actionButton("ClearSearchUIDFile", label = "Clear Study Subjects")),
                        selectizeInput("SearchBySpecimenType", "Specimen Type", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label)),
                        dateRangeInput("dateRange", label = "Collection Dates", start = NA, end = NA) %>% suppressWarnings(),
                        # selectizeInput("SearchByBarcode", "Plate Name", choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name)),
                        selectizeInput("SearchByLocation", "Storage Location", choices = c("", sampleDB::CheckTable("location")$location_name)),
                        selectizeInput("SearchByLevelI", "Storage Location: Level I", choices = c("")),
                        selectizeInput("SearchByLevelII", "Storage Location: Level II", choices = c("")),
                        selectizeInput("SearchByExhausted", "Archived", choices = c("", TRUE, FALSE)),
                        # selectizeInput("SearchBySampleType", "Sample Type", choices = c("", "Micronix", "Cryovile", "RDT", "Paper")),
                        textOutput("WarnSubjectUIDFileColnames"),
                        textOutput("WarnSubjectUIDFileColnames2")
                      ),
                    mainPanel(
                      width = 10,
                        DT::dataTableOutput("SearchResultsTable"),
                        downloadButton("downloadData", "Download")
                    ))),
           navbarMenu("Move Samples",
             tabPanel("Move Samples",
  
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          HTML("<h4><b>Move Samples</b></h4>"),
                          hr(),
                          radioButtons("MoveSampleType","Sample Storage Type", c("Micronix" = "micronix", "Cryovial" = "cryovial", "RDT" = "rdt", "Paper" = "paper"), inline = T),
                          fileInput("MoveDataSet", "Move Samples File(s)", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                          fluidRow(column(width = 12,
                                          actionButton("MoveAction", label = "Move Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                          actionButton("ClearMoveForm", label = "Clear Form"))),
                          br(),
                          textOutput("WarningMoveColnames"),
                          verbatimTextOutput("MoveReturnMessage1"),
                          verbatimTextOutput("MoveReturnMessage2"),
                        ),
                        mainPanel(
                          width = 9,
                          HTML("<h4><center>This is an <b>Example SampleDB MoveSamplesCSV</b> from VisionMate.</center></h4>"),
                          fluidRow(
                            column(width = 2),
                            column(width = 8,
                                   span(verbatimTextOutput("ExampleMoveSamplesCSV"), style ="max-width: 75px; text-align: center"),
                            )),
                          fluidPage(
                            column(width = 3),
                            column(width = 6,
                                   HTML("<h5>This format is essentially just the CSV that any micronix instrument creates.
                               No CSV reformating is required.</h5>
                               <h5>To perform moves:</h5>
                               <ol> <li>All plates involved in the move need to be scanned</li> <li>The Micronix .csv file needs to be given the plate name</li> <li>All the scans must be uploaded together</li> </ol>")
                            ),
                            column(width = 3)
                          ),
                          )),
                      ),
             tabPanel("Move Container of Samples",
                      
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          #shouldnt need sample type but bc it is unknown at this point if container names are unique, sample type specifies the db table to use to find the container name
                          radioButtons("MoveContainerSampleType","Move Container Sample Type", c("Micronix" = "micronix", "Cryovile" = "cryovile", "RDT" = "rdt", "Paper" = "paper"), inline = T),
                          selectizeInput("MoveContainerName", label = "Container Name", choices = NULL),
                          #should print to user the current location of the container
                          selectInput("MoveContainerLocation", label = "Storage Location", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                          fluidRow(column(width = 1), column(width = 11, selectInput("MoveContainerLocationLevelI", label = HTML("<h5>Storage Location: Level I</h5>"), width = '100%', choices = NULL))),
                          fluidRow(column(width = 1), column(width = 11, selectInput("MoveContainerLocationLevelII", label = HTML("<h5>Storage Location: Level II</h5>"), width = '100%', choices = NULL))),
                          fluidRow(column(width = 12,
                                          actionButton("MoveContainerAction", width = '49%', label = "Move Container", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                          br(),
                          verbatimTextOutput("MoveContainerMessage"),
                        ),
                        mainPanel()
           ))),
           navbarMenu("Delete & Archive Samples",
             tabPanel("Delete & Archive Samples",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          shinyjs::hidden(textInput("delarch_toggle1", label = NULL)),
                          # actionButton("verify_delarch", label = NULL),
                          HTML("<h4>Delete and Archive Samples</h4>"),
                          hr(),
                          # fileInput("SearchByLabel", label = HTML("Barcode <h6>Single column named \"barcode\"</h6>")), actionButton("ClearSearchBarcodes", label = "Clear Barcodes"), textOutput("WarnSubjectBarcodeFileColnames"), textOutput("WarnSubjectBarcodeFileColnames2"),
                          radioButtons("DelArchSearchBySampleType","Sample Type", c("All" = "all", "Micronix" = "micronix", "Cryovial" = "cryo", "RDT" = "rdt", "Paper" = "paper"), inline = T),
                          conditionalPanel(condition = "input.DelArchSearchBySampleType == \"micronix\"",
                                           fileInput("DelArchSearchByBarcode", label = "Sample Barcodes"),
                                           selectInput("DelArchSearchByPlate", label = "Plate Name", choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name))),
                          conditionalPanel(condition = "input.DelArchSearchBySampleType == \"cryo\"",
                                           fileInput("DelArchSearchByCryovialLabels", label = "Sample Labels"),
                                           selectInput("DelArchSearchByBox", label = "Box Name", choices = c("", sampleDB::CheckTable(database = database, "box")$box_name))),
                          conditionalPanel(condition = "input.DelArchSearchBySampleType == \"rdt\"",
                                           fileInput("DelArchSearchByRDTLabels", label = "Sample Labels"),
                                           selectInput("DelArchSearchByRDTBag", label = "Bag Name", choices = c("", sampleDB::CheckTable(database = database, "bag")$bag_name))),
                          conditionalPanel(condition = "input.DelArchSearchBySampleType == \"paper\"",
                                           fileInput("DelArchSearchByPaperLabels", label = "Sample Labels"),
                                           selectInput("DelArchSearchByPaperBag", label = "Bag Name", choices = c("", sampleDB::CheckTable(database = database, "bag")$bag_name))),
                          hr(),
                          selectizeInput("DelArchSearchByStudy", "Study", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code)),
                          radioButtons("DelArchSubjectUIDSearchType", label = "Study Subject Search Method", choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"), selected = "individual"),
                          conditionalPanel(condition = "input.DelArchSubjectUIDSearchType == \"individual\"",
                                           selectizeInput("DelArchSearchBySubjectUID", label = "Study Subject", choices = c())),
                          conditionalPanel(condition = "input.DelArchSubjectUIDSearchType == \"multiple\"",
                                           fileInput("DelArchSearchBySubjectUIDFile", label = HTML("Study Subjects <h6>Single column named \"subject_uid\"</h6>")), actionButton("ClearSearchUIDFile", label = "Clear Study Subjects")),
                          selectizeInput("DelArchSearchBySpecimenType", "Specimen Type", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label)),
                          dateRangeInput("DelArchdateRange", label = "Collection Dates", start = NA, end = NA) %>% suppressWarnings(),
                          selectizeInput("DelArchSearchByLocation", "Storage Location", choices = c("", sampleDB::CheckTable("location")$location_name)),
                          selectizeInput("DelArchSearchByLevelI", "Storage Location: Level I", choices = c("")),
                          selectizeInput("DelArchSearchByLevelII", "Storage Location: Level II", choices = c("")),
                          selectizeInput("DelArchSearchByExhausted", "Archived", choices = c("", TRUE, FALSE)),
                          # textOutput("WarnSubjectUIDFileColnames"),
                          # textOutput("WarnSubjectUIDFileColnames2")
                        ),
                        mainPanel(
                          width = 10,
                          DT::dataTableOutput("DelArchSearchResultsTable"),
                          downloadButton("DelArchdownloadData", "Download"),
                          hr(),
                          # verbatimTextOutput("ShowSelectedSamples"),
                          textInput(label = "Sample ID", inputId = "delarch_id"),
                          # verbatimTextOutput("show_delarch_id"),
                          fluidRow(column(width = 12,
                                          actionButton("ArchiveAction", label = "Archive Samples"),
                                          actionButton("DeleteAction", label = "Delete Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                          verbatimTextOutput("arch_del_completed_usr_msg"),
                          # conditionalPanel(condition = "input.RenameStudyDescription == \"xxx\"",
                          #                  br(),
                          #                  HTML("Type \"Yes\" if you would like to archive these samples."),
                          #                  textInput("zzz", label = NULL),
                          #                  actionButton("yes1", label = "Enter"),
                          #                  verbatimTextOutput("yesout")),
                        ))),
           tabPanel("Delete Empty Container of Samples",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        #shouldnt need sample type but bc it is unknown at this point if container names are unique, sample type specifies the db table to use to find the container name
                        radioButtons("DeleteContainerSampleType","Delete Container Sample Type", c("Micronix" = "micronix", "Cryovial" = "cryovial", "RDT" = "rdt", "Paper" = "paper"), inline = T),
                        selectInput("DelteContainerName", label = "Container Name", choices = c("")),
                        fluidRow(column(width = 12,
                                        actionButton("DeleteContainerAction", width = '49%', label = "Delete Container", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                      ),
                      mainPanel()))),
           
           navbarMenu("Update References",
                      tabPanel("Freezers",
                               sidebarLayout(
                                 sidebarPanel(
                                   width = 3,
                                   HTML("<h4><b>Add a Freezer</b></h4>"),
                                   textInput("AddFreezerName", label = NULL, placeholder = "New Name"),
                                   textInput("AddFreezerType", label = NULL, placeholder = "New Type"),
                                   textInput("AddFreezerLevel_I", label = NULL, placeholder = "New Level I"),
                                   textInput("AddFreezerLevel_II", label = NULL, placeholder = "New Level II"),
                                   actionButton("AddFreezerAction", label = "Add", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningFreezerNameAddUnique"),
                                   hr(),
                                   HTML("<h4><b>Rename a Freezer</b></h4>"),
                                   selectInput("RenameFreezerName1", label = NULL, choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                                   selectInput("RenameFreezerLevelI1", label = NULL, choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                                   selectInput("RenameFreezerLevelII1", label = NULL, choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                                   textInput("RenameFreezerName2", label = NULL, placeholder = "New Name"),
                                   textInput("RenameFreezerType2", label = NULL, placeholder = "New Type"),
                                   textInput("RenameFreezerLevelI2", label = NULL, placeholder = "New Level I"),
                                   textInput("RenameFreezerLevelII2", label = NULL, placeholder = "New Level II"),
                                   actionButton("RenameFreezerAction", label = "Rename", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningFreezerNameChangeUnique"),
                                   hr(),
                                   HTML("<h4><b>Remove a Freezer</b></h4>"),
                                   selectInput("DeleteFreezerName", label = NULL, choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                                   selectInput("DeleteFreezerLevelI", label = NULL, choices = NULL),
                                   selectInput("DeleteFreezerLevelII", label = NULL, choices = NULL),
                                   textOutput("WarningFreezerDeletion"),
                                   actionButton("DeleteFreezerAction", label = "Delete", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   HTML("<br></br>"),
                                   span(verbatimTextOutput("FreezerReturnMessage"), style="font-size: 28px")
                                 ),
                                 mainPanel(
                                   width = 9,
                                   HTML("<h4><b>Freezers</b></h4>"),
                                   DT::dataTableOutput("TableFreezer"))),
                               ),

                      tabPanel("Specimen Types",
                               sidebarLayout(
                                 sidebarPanel(
                                   width = 3,
                                   HTML("<h4><b>Add a Specimen Type</b></h4>"),
                                   textInput("AddSpecimenType", label = NULL, placeholder = "New Name"),
                                   actionButton("AddSpecimenTypeAction", label = "Add", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WaringAddSpecimenTypeUnique"),
                                   hr(),
                                   HTML("<h4><b>Rename a Specimen Type</b></h4>"),
                                   selectInput("RenameSpecimenType1", label = "Current Specimen Type Name", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label), selected = 1),
                                   textInput("RenameSpecimenType2", label = NULL, placeholder = "New Name"),
                                   actionButton("RenameSpecimenTypeAction", label = "Rename", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningChangeSpecimenTypeUnique"),
                                   HTML("<h4><b>Remove a Specimen Type</b></h4>"),
                                   selectInput("DeleteSpecimenType", label = NULL, choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label), selected = 1),
                                   textOutput("WarningSpecimenTypeDeletion"),
                                   actionButton("DeleteSpecimenTypeAction", label = "Delete", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   HTML("<br></br>"),
                                   span(verbatimTextOutput("SpecimenReturnMessage"), style="font-size: 28px"),
                                 ),
                                 mainPanel(
                                   HTML("<h4><b>Specimen Types</b></h4>"),
                                   DT::dataTableOutput("TableSpecimenType"),
                                 ))),

                      tabPanel("Studies",
                               sidebarLayout(
                                 sidebarPanel(
                                   width = 4,
                                  HTML("<h4><b>Add a Study</b></h4>"),
                                   fluidRow(column(width = 6, textInput("AddStudyTitle", label = "New Title")),
                                            column(width = 6, textInput("AddStudyDescription", label = "New Description"))),
                                   fluidRow(column(width = 6, textInput("AddStudyLeadPerson", label = "New Lead Person")),
                                            column(width = 6, textInput("AddStudyShortCode", label = "New Short Code"))),
                                   fluidRow(column(width = 6, checkboxInput("AddStudyIsLongitudinal", label = "Londitudinal", value = FALSE))), actionButton("AddStudyAction", label = "Add", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningStudyAddTitleUnique"),
                                   textOutput("WarningStudyAddShortCodeUnique"),
                                   hr(),
                                   HTML("<h4><b>Rename a Study</b></h4>"),
                                   selectInput("ChangeStudyShortCode", width = "47.5%", label = "Study to be modified", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code)),
                                   fluidRow(column(width = 6, textInput("RenameStudyTitle", label = "Title", placeholder = "New Title")),
                                            column(width = 6, textInput("RenameStudyDescription", label = "Description", placeholder = "New Description"))),
                                   fluidRow(column(width = 6, textInput("RenameStudyLeadPerson", label = "Lead Person", placeholder = "New Lead Person")),
                                            column(width = 6,textInput("RenameStudyShortCode", label = "Short Code", placeholder = "New Short Code"))),
                                   fluidRow(column(width = 6, checkboxInput("RenameStudyIsLongitudinal", label = "Londitudinal", value = FALSE))),
                                   actionButton("RenameStudyAction", label = "Rename", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningStudyChangeTitleUnique"),
                                   textOutput("WarningStudyChangeShortCodeUnique"),
                                   hr(),
                                   HTML("<h4><b>Remove a Study</b></h4>"),
                                   selectInput("DeleteStudyShortCode", width = "47.5%", label = "Study to be deleted", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code)),
                                   textOutput("WarnStudyDeletion"),
                                   actionButton("DeleteStudyAction", label = "Delete", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   HTML("<br></br>"),
                                   span(verbatimTextOutput("StudyReturnMessage"), style="font-size: 28px")
                                 ),
                                 mainPanel(
                                   width = 8,
                                   HTML("<h4><b>Studies</b></h4>"),
                                   DT::dataTableOutput("TableStudy")))
                               )
                      ),
           tabPanel("About",
                    fluidPage(uiOutput("report_issues"),
                              uiOutput("source_code")),
           ),

)
