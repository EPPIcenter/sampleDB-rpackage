library(dplyr)
library(sampleDB)
library(shinyFeedback)
library(shiny)
library(markdown)
library(DT)
library(shinyWidgets)


#SET PATH TO SQLITE DATABASE
# database <- "example_19-Oct-21.sample_db.sqlite"
# database <- Sys.getenv("SAMPLEDB_DATABASE") #use the aragorn env var set at boot
database <- "/databases/new.sampleDB.db"

navbarPage("SampleDB",
           navbarMenu("Upload New Wetlab Samples",
           tabPanel("Micronix Samples",

                    #GENERAL APP CSS
                    shinyFeedback::useShinyFeedback(),
                    shinyjs::useShinyjs(),
                    includeCSS("app.css"),
                    tags$head(
                      tags$style(HTML("
                        h5 {
                              line-height: 150%;
                            }
                        .shiny-output-error-validation {
                              color: #FCA211; font-weight: bold;
                            }"))),
                    
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        HTML("<h4><b>Upload Micronix Samples Form</b></h4>"),
                        hr(),
                        fileInput("UploadMicronixDataSet", "Upload a UploadMicronixCSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        textOutput("WarningMicronixUploadSampleID"),
                        textOutput("WarningMicronixUploadColnames"),
                        textOutput("WarningUploadMicronixSpecimenTypes"),
                        textOutput("WarningMicronixUploadDateFormat"),
                        textOutput("WarningUploadMicronixStudyShortCodes"),
                        textOutput("WarningMicronixSpecimenExists"),
                        textInput("UploadMicronixPlateID", label = "Unique Plate Name"),
                        textOutput("WarningMicronixUploadContainer"),
                        selectInput("UploadMicronixLocation", label = "Storage Location", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        selectInput("UploadLocationMicronixLevelI", label = HTML("<h5>Level I</h5>"), width = '25%', choices = c("", sampleDB::CheckTable(database = database, "location")$level_I)),
                        selectInput("UploadLocationMicronixLevelII", label = HTML("<h5>Level II</h5>"), width = '25%', choices = c("", sampleDB::CheckTable(database = database, "location")$level_II)),
                        fluidRow(column(width = 12,
                                        actionButton("UploadMicronixAction", label = "Upload Dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton("ClearMicronixUploadForm", label = "Clear Form"))),
                        br(),
                        span(verbatimTextOutput("UploadMicronixReturnMessage1"), style="font-size: 28px"),
                        span(verbatimTextOutput("UploadMicronixReturnMessage2"), style="font-size: 28px")
                      ),
                      mainPanel(
                        width = 9,
                        HTML("<h4>This is an <b>Example SampleDB UploadMicronixCSV</b> from VisionMate.</h4>"),
                        verbatimTextOutput("ExampleMicronixUploadCSVNoDate"),
                        HTML("<h5>To create this table join the <code>LocationRow</code>,
                             <code>LocationColumn</code>, and <code>TubeCode</code> from the VisionMate
                             CSV with <code>study_subject_id</code> and <code>specimen_type</code> information,
                             where the <code>study_subject_id</code> and <code>specimen_type</code>
                             corresponds to the study subject id and specimen type associated with each sample</h5>"),
                        HTML("<h4><b>Alternatives to VisionMate Format</b></h4>"),
                        HTML("<h5>If your data is was formatted using a different micronix instrument the process for
                             creating a UploadMicronixCSV is the same. Simply add the <code>study_subject_id</code>
                             and <code>specimen_type</code> columns to your existing columns and the CSV is ready for upload.
                             In other words uploading is agnostic with respect to the format of the micronix information</h5>"),
                        HTML("<h4><b>Longitudinal Data</b></h4>"),
                        HTML("<h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                             longitudinal information to the samples. (Date format is YMD.)</h5>"),
                        br(),
                        HTML("<center><h4><b>Example SampleDB UploadMicronixCSV</b> with <code>collection_date</code> Column</h4></center>"),
                        verbatimTextOutput("ExampleMicronixUploadCSVDate"),
                      ))),
           
           tabPanel("Cryo Samples",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        HTML("<h4><b>Upload Cryo Samples Form</b></h4>"),
                        hr(),
                        fileInput("UploadCryoDataSet", "Upload a UploadCryoCSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        textOutput("WarningCryoUploadSampleID"),
                        textOutput("WarningCryoUploadColnames"),
                        textOutput("WarningUploadCryoSpecimenTypes"),
                        textOutput("WarningCryoUploadDateFormat"),
                        textOutput("WarningUploadCryoStudyShortCodes"),
                        textOutput("WarningCryoSpecimenExists"),
                        textInput("UploadCryoPlateID", label = "Unique Plate Name"),
                        textOutput("WarningCryoUploadContainer"),
                        selectInput("UploadLocationCryoFreezerName", label = "Freezer Name", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        selectInput("UploadLocationCryoLevelI", label = HTML("<h5>Level I</h5>"), width = '25%', choices = c("", sampleDB::CheckTable(database = database, "location")$level_I)),
                        selectInput("UploadLocationCryoLevelII", label = HTML("<h5>Level II</h5>"), width = '25%', choices = c("", sampleDB::CheckTable(database = database, "location")$level_II)),
                        fluidRow(column(width = 12, 
                                        actionButton("UploadCryoAction", label = "Upload Dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton("ClearCryoUploadForm", label = "Clear Form"))),
                        span(verbatimTextOutput("UploadCryoReturnMessage1"), style="font-size: 28px"),
                        span(verbatimTextOutput("UploadCryoReturnMessage2"), style="font-size: 28px")
                      ),
                      mainPanel(
                        width = 9,
                        HTML("<h4>This is an <b>Example SampleDB UploadCryoCSV</b>.</h4>"),
                        verbatimTextOutput("ExampleUploadCryoCSVNoDate"),
                        HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code> and <code>study_code</code> must be unique.
                             Consider adding a collection_date for the sample</h5>"),
                        HTML("<h4><b>Longitudinal Data</b></h4>"),
                        HTML("<h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                             longitudinal information to the samples. (Date format is YMD.)</h5>"),
                        HTML("<center><h4><b>Example SampleDB UploadCryoCSV</b> with <code>collection_date</code> Column</h4></center>"),
                        verbatimTextOutput("ExampleUploadCryoCSVDate"),
                        HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code>, <code>study_code</code> and <code>collection_date</code> must be unique</h5>")))),
           
           tabPanel("RDT Samples",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        HTML("<h4><b>Upload RDT Samples Form</b></h4>"),
                        hr(),
                        fileInput("UploadRDTDataSet", "Upload a UploadRDTCSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        textOutput("WarningRDTUploadSampleID"),
                        textOutput("WarningRDTUploadColnames"),
                        textOutput("WarningUploadRDTSpecimenTypes"),
                        textOutput("WarningRDTUploadDateFormat"),
                        textOutput("WarningUploadRDTStudyShortCodes"),
                        textOutput("WarningRDTSpecimenExists"),
                        textInput("UploadRDTPlateID", label = "Unique Plate Name"),
                        textOutput("WarningRDTUploadContainer"),
                        selectInput("UploadLocationRDTFreezerName", label = "Freezer Name", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        selectInput("UploadLocationRDTLevelI", label = HTML("<h5>Level I</h5>"), width = '25%', choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        selectInput("UploadLocationRDTLevelII", label = HTML("<h5>Level II</h5>"), width = '25%', choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        fluidRow(column(width = 12,
                                        actionButton("UploadRDTAction", label = "Upload Dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton("ClearRDTUploadForm", label = "Clear Form"))),
                        span(verbatimTextOutput("UploadRDTReturnMessage1"), style="font-size: 28px"),
                        span(verbatimTextOutput("UploadRDTReturnMessage2"), style="font-size: 28px")
                      ),
                      mainPanel(
                        width = 9,
                        HTML("<h4>This is an <b>Example SampleDB UploadRDTCSV</b>.</h4>"),
                        verbatimTextOutput("ExampleUploadRDTCSVNoDate"),
                        HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code> and <code>study_code</code> must be unique.
                             Consider adding a collection_date for the sample</h5>"),
                        HTML("<h4><b>Longitudinal Data</b></h4>"),
                        HTML("<h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                             longitudinal information to the samples. (Date format is YMD.)</h5>"),
                        HTML("<center><h4><b>Example SampleDB UploadCryoCSV</b> with <code>collection_date</code> Column</h4></center>"),
                        verbatimTextOutput("ExampleUploadRDTCSVNoDate"),
                        HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code>, <code>study_code</code> and <code>collection_date</code> must be unique</h5>")))),
           tabPanel("Paper Samples",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        HTML("<h4><b>Upload Paper Samples Form</b></h4>"),
                        hr(),
                        fileInput("UploadPaperDataSet", "Upload a UploadPaperCSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        textOutput("WarningPaperUploadSampleID"),
                        textOutput("WarningPaperUploadColnames"),
                        textOutput("WarningUploadPaperSpecimenTypes"),
                        textOutput("WarningPaperUploadDateFormat"),
                        textOutput("WarningUploadPaperStudyShortCodes"),
                        textOutput("WarningPaperSpecimenExists"),
                        textInput("UploadPaperPlateID", label = "Unique Bag Name"),
                        textOutput("WarningPaperUploadContainer"),
                        selectInput("UploadLocationPaperFreezerName", label = "Freezer Name", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        selectInput("UploadLocationPaperLevelI", label = HTML("<h5>Level I</h5>"), width = '25%', choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        selectInput("UploadLocationPaperLevelII", label = HTML("<h5>Level II</h5>"), width = '25%', choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        fluidRow(column(width = 12,
                                        actionButton("UploadActionPaperSamples", label = "Upload Dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton("ClearUploadFormPaperSamples", label = "Clear Form"))),
                        span(verbatimTextOutput("UploadPaperReturnMessage1"), style="font-size: 28px"),
                        span(verbatimTextOutput("UploadPaperReturnMessage2"), style="font-size: 28px")
                      ),
                      mainPanel(
                        width = 9,
                        HTML("<h4>This is an <b>Example SampleDB UploadPaperCSV</b>.</h4>"),
                        verbatimTextOutput("ExampleUploadPaperCSVNoDate"),
                        HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code> and <code>study_code</code> must be unique.
                             Consider adding a collection_date for the sample</h5>"),
                        HTML("<h4><b>Longitudinal Data</b></h4>"),
                        HTML("<h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                             longitudinal information to the samples. (Date format is YMD.)</h5>"),
                        HTML("<center><h4><b>Example SampleDB UploadCryoCSV</b> with <code>collection_date</code> Column</h4></center>"),
                        verbatimTextOutput("ExampleUploadPaperCSVDate"),
                        HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code>, <code>study_code</code> and <code>collection_date</code> must be unique</h5>")))),
           ),

           tabPanel("Search Existing Samples",
                    sidebarLayout(
                      sidebarPanel(
                        width = 2,
                        HTML("<h4>Search Filters</h4>"),
                        hr(),
                        fileInput("SearchByBarcode", label = HTML("Barcode <h6>Single column named \"barcode\"</h6>")), actionButton("ClearSearchBarcodes", label = "Clear Barcodes"), textOutput("WarnSubjectBarcodeFileColnames"), textOutput("WarnSubjectBarcodeFileColnames2"),
                        br(),
                        selectizeInput("SearchByPlateID", "Plate Name", choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name)),
                        selectizeInput("SearchByStudy", "Study", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code)),
                        selectizeInput("SearchByLocation", "Storage Location", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
                        selectizeInput("SearchBySpecimenType", "Specimen Type", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label)),
                        radioButtons("SubjectUIDSearchType", label = "Study Subject Search Method", choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"), selected = "individual"),
                        conditionalPanel(condition = "input.SubjectUIDSearchType == \"individual\"",
                                         selectizeInput("SearchBySubjectUID", label = "Study Subject", choices = NULL)),
                        conditionalPanel(condition = "input.SubjectUIDSearchType == \"multiple\"",
                                         fileInput("SearchBySubjectUIDFile", label = HTML("Study Subjects <h6>Single column named \"subject_uid\"</h6>")), actionButton("ClearSearchUIDFile", label = "Clear Study Subjects")), textOutput("WarnSubjectUIDFileColnames"), textOutput("WarnSubjectUIDFileColnames2")
                      ),
                    mainPanel(
                      width = 10,
                          DT::dataTableOutput("SearchResultsTable"),
                          downloadButton("downloadData", "Download")
                    ))
           ),

           tabPanel("Move Samples",

                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        HTML("<h4><b>Move Samples Form</b></h4>"),
                        fileInput("MoveDataSet", "SampleDB MoveSamplesCSV", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
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
                        HTML("<h4>This is an <b>Example SampleDB MoveSamplesCSV</b> from VisionMate.</h4>"),
                        verbatimTextOutput("ExampleMoveSamplesCSV"),
                        HTML("<h5>This format is essentially just the CSV that any micronix instrument creates.
                             No CSV reformating is required.</h5>"),
                        HTML("<h5>To perform moves:</h5>"),
                        HTML("<ol> <li>All plates involved in the move need to be scanned</li> <li>The Micronix .csv file needs to be given the plate name</li> <li>All the scans must be uploaded together</li> </ol>"))),
                        # HTML("<h6>*Note that no new samples can be added during moves. This is because no
                        #       study subject nor specimen type information is processed during moves.</h6>")
                    ),
           
           tabPanel("Delete Empty Plates",
                    fluidRow(
                      column(
                        width = 4,
                        
                        fluidRow(
                          column(
                            width = 12,
                            HTML("<h4><b>Delete Empty Plate</b></h4>"),
                            selectizeInput("DeletePlateName",
                                           "Plate Name",
                                           choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name)),
                            actionButton("DeletePlateAction", "Delete Plate")),
                          column(
                            width = 6,
                            br(),
                            textOutput("WarningDeletePlateMessage"),
                            br(),
                            verbatimTextOutput("DeletePlateMessage"),
                          ))),
                        
                      column(
                        width = 4,
                        HTML("<code>ARCHIVE SAMPLES</code>")),
                      column(
                        width = 4,
                        HTML("<code>DELETE SAMPLES</code>")),
                        )),
           
           navbarMenu("References",

                      tabPanel("Freezers",
                               sidebarLayout(
                                 sidebarPanel(
                                   width = 3,
                                   HTML("<h4><b>Add a Freezer</b></h4>"),
                                   textInput("AddFreezer", label = NULL, placeholder = "New Name"),
                                   actionButton("AddFreezerAction", label = "Add", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningFreezerNameAddUnique"),
                                   hr(),
                                   HTML("<h4><b>Rename a Freezer</b></h4>"),
                                   selectInput("RenameFreezer1", label = "Current Name", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name), selected = 1),
                                   textInput("RenameFreezer2", label = NULL, placeholder = "New Name"),
                                   actionButton("RenameFreezerAction", label = "Rename", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningFreezerNameChangeUnique"),
                                   hr(),
                                   HTML("<h4><b>Remove a Freezer</b></h4>"),
                                   selectInput("DeleteFreezer", label = NULL, choices = c("", sampleDB::CheckTable(database = database, "location")$location_name), selected = 1),
                                   textOutput("WarningFreezerDeletion"),
                                   actionButton("DeleteFreezerAction", label = "Delete", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
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
                                   fluidRow(column(width = 6, checkboxInput("AddStudyIsLongitudinal", label = "Londitudinal", value = FALSE)),
                                            column(width = 6, checkboxInput("AddStudyIsHidden", label = "Hidden", value = FALSE))), actionButton("AddStudyAction", label = "Add", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningStudyAddTitleUnique"),
                                   textOutput("WarningStudyAddShortCodeUnique"),
                                   hr(),
                                   HTML("<h4><b>Rename a Study</b></h4>"),
                                   selectInput("ChangeStudyShortCode", width = "47.5%", label = "Current Study", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code), selected = 1),
                                   fluidRow(column(width = 6, textInput("RenameStudyTitle", label = "Title", placeholder = "New Title")),
                                            column(width = 6, textInput("RenameStudyDescription", label = "Description", placeholder = "New Description"))),
                                   fluidRow(column(width = 6, textInput("RenameStudyLeadPerson", label = "Lead Person", placeholder = "New Lead Person")),
                                            column(width = 6,textInput("RenameStudyShortCode", label = "Short Code", placeholder = "New Short Code"))),
                                   fluidRow(column(width = 6, checkboxInput("RenameStudyIsLongitudinal", label = "Londitudinal", value = FALSE)),
                                            column( width = 6, checkboxInput("RenameStudyIsHidden", label = "Hidden", value = FALSE))),
                                   actionButton("RenameStudyAction", label = "Rename", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   textOutput("WarningStudyChangeTitleUnique"),
                                   textOutput("WarningStudyChangeShortCodeUnique"),
                                   hr(),
                                   HTML("<h4><b>Remove a Study</b></h4>"),
                                   selectInput("DeleteStudyShortCode", width = "47.5%", label = "Current Study", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code), selected = 1),
                                   textOutput("WarnStudyDeletion"),
                                   actionButton("DeleteStudyAction", label = "Delete", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   br(), 
                                   span(verbatimTextOutput("StudyReturnMessage"), style="font-size: 28px")
                                 ),
                                 mainPanel(
                                   width = 8,
                                   HTML("<h4><b>Studies</b></h4>"),
                                   DT::dataTableOutput("TableStudy")))
                               )
                      ),
           tabPanel("About",

                    fluidRow(column(width = 4))

           )

)
