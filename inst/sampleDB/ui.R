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
database <- "/databases/sampledb_database.sqlite"

navbarPage("SampleDB",
           navbarMenu("Upload New Wetlab Samples",
           tabPanel("Upload New Micronix Tube Samples",

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

                    fluidRow(
                      column(4,

                        HTML("<h4><b>Upload Micronix Samples Form</b></h4>"),
                        br(),
                        
                        fluidRow(
                          column(
                            width = 12,
                            fileInput("UploadDataSet",
                                      "Upload a UploadMicronixCSV CSV",
                                      multiple = TRUE,
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                            )),
                        
                        textOutput("WarningUploadBarcodeA"),
                        textOutput("WarningUploadBarcode"),
                        textOutput("WarningUploadColnames"),
                        textOutput("WarningUploadSpecimenTypes"),
                        textOutput("WarningUploadStudyShortCodes"),
                        textOutput("WarningUploadDateFormat"),
                        textOutput("WarningStudySubjectLongitudinal"),

                    fluidRow(
                      column(
                        width = 12,
                        textInput("UploadPlateID",
                                  label = "Unique Plate Name"))),
                    textOutput("WarningUploadPlate"),

                    fluidRow(
                      column(
                        width = 12,
                        selectInput("UploadLocation",
                                    label = "Storage Location",
                                    choices = c("", sampleDB::CheckTable(database = database, "location")$description)))),

                    fluidRow(
                      column(
                        width = 12,
                            actionButton("UploadAction",
                                         label = "Upload Dataset",
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            actionButton("ClearUploadForm",
                                         label = "Clear Form"),
                          )),

                    br(),

                    fluidRow(
                      column(
                        width = 6,
                        span(verbatimTextOutput("UploadReturnMessage1"), style="font-size: 28px"))),
                    
                    fluidRow(
                      column(
                        width = 6,
                        span(verbatimTextOutput("UploadReturnMessage2"), style="font-size: 28px"))),
                    ),

                      column(8,
                        fluidRow(
                          br(),
                          HTML("<h4>This is an <b>Example SampleDB UploadMicronixCSV</b> from VisionMate.</h4>"),
                          column(12,
                                 verbatimTextOutput("ExampleUploadCSVNoDate")),
                          HTML("<h5>To create this table join the <code>LocationRow</code>,
                               <code>LocationColumn</code>, and <code>TubeCode</code> from the VisionMate
                               CSV with <code>study_subject_id</code> and <code>specimen_type</code> information,
                               where the <code>study_subject_id</code> and <code>specimen_type</code>
                               corresponds to the study subject id and specimen type associated with each sample</h5>")),

                          fluidRow(
                             HTML("<h4><b>Alternatives to VisionMate Format</b></h4>"),
                             HTML("<h5>If your data is was formatted using a different micronix instrument the process for
                                  creating a UploadMicronixCSV is the same. Simply add the <code>study_subject_id</code>
                                  and <code>specimen_type</code> columns to your existing columns and the CSV is ready for upload.
                                  In other words uploading is agnostic with respect to the format of the micronix information</h5>")),

                          fluidRow(
                             HTML("<h4><b>Longitudinal Data</b></h4>"),
                             HTML("<h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                                  longitudinal information to the samples. (Date format is YMD.)</h5>"),
                             br(),
                             HTML("<center><h4><b>Example SampleDB UploadMicronixCSV</b> with <code>collection_date</code> Column</h4></center>"),
                             column(12,
                                    verbatimTextOutput("ExampleUploadCSVDate")
                             ))))),
           
           tabPanel("Upload New Cryo Samples",
                    fluidRow(
                      column(4,
                             
                             HTML("<h4><b>Upload Cryo Samples Form</b></h4>"),
                             br(),
                             
                             fluidRow(
                               column(
                                 width = 12,
                                 fileInput("UploadCryoSamples",
                                           "Upload a UploadCryoCSV CSV",
                                           multiple = TRUE,
                                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                               )),
                             fluidRow(
                               column(
                                 width = 12,
                                 textInput("UploadBoxID",
                                           label = "Unique Box Name"))),
                             fluidRow(
                               column(
                                 width = 6,
                                 hr()
                                 )),
                             
                             fluidRow(
                               column(
                                 width = 12,
                                 selectInput("UploadLocationCryoFreezerName",
                                             label = "Freezer Name",
                                             choices = c("", sampleDB::CheckTable(database = database, "location")$description)),
                                 selectInput("UploadLocationCryoLevelI",
                                             label = HTML("<h5>Level I</h5>"),
                                             width = '25%',
                                             choices = c("", sampleDB::CheckTable(database = database, "location")$description)),
                                 selectInput("UploadLocationCryoLevelII",
                                             label = HTML("<h5>Level I</h5>"),
                                             width = '25%',
                                             choices = c("", sampleDB::CheckTable(database = database, "location")$description)))),
                             fluidRow(
                               column(
                                 width = 12,
                                 actionButton("UploadActionCryoSamples",
                                              label = "Upload Dataset",
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 actionButton("ClearUploadFormCryoSamples",
                                              label = "Clear Form"),
                               )),
                      ),
                      column(8,
                             fluidRow(
                               br(),
                               HTML("<h4>This is an <b>Example SampleDB UploadCryoCSV</b>.</h4>"),
                               column(12,
                                      verbatimTextOutput("ExampleUploadCryoCSVNoDate")
                                      ),
                               HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code> and <code>study_code</code> must be unique.
                                    Consider adding a collection_date for the sample</h5>")
                             ),
                             fluidRow(
                               HTML("<h4><b>Longitudinal Data</b></h4>"),
                               HTML("<h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                                  longitudinal information to the samples. (Date format is YMD.)</h5>"),
                               br(),
                               HTML("<center><h4><b>Example SampleDB UploadCryoCSV</b> with <code>collection_date</code> Column</h4></center>"),
                               column(12,
                                      verbatimTextOutput("ExampleUploadCryoCSVDate")
                               ),
                               HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code>, <code>study_code</code> and <code>collection_date</code> must be unique</h5>"))
                             )
                    )
                    
                    ),
           ),

           tabPanel("Search Existing Samples",

                    h4("Use Filters Below to Populate the Search Results Table", align = "center"),
                    br(),
                    fluidRow(
                      column(
                        width = 4,
                        fileInput("SearchByBarcode",
                                  HTML("Barcode - single column named \"barcode\""),
                                  multiple = FALSE),
                        actionButton("ClearSearchBarcodes", label = "Clear Barcodes")),
                        textOutput("WarnSubjectBarcodeFileColnames"),
                        textOutput("WarnSubjectBarcodeFileColnames2"),
                      
                      column(
                        width = 4,
                        selectizeInput("SearchByPlateID",
                                       "Plate Name",
                                       choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid))),

                      column(
                        width = 4,
                        radioButtons("SubjectUIDSearchType", label = "Study-Subject Search Method",
                                     choices = list("Single UID" = "one_at_a_time", "Multiple UIDs -- column named \"subject_uid\"" = "multiple"),
                                     selected = "one_at_a_time"),

                        conditionalPanel(
                          condition = "input.SubjectUIDSearchType == \"one_at_a_time\"",
                          selectizeInput("SearchBySubjectUID",
                                    label = "Study-Subject ID (UID)",
                                    choices = NULL)),

                        conditionalPanel(
                          condition = "input.SubjectUIDSearchType == \"multiple\"",
                          fileInput("SearchBySubjectUIDFile",
                                    label = "Study-Subject ID (UID)"),
                          actionButton("ClearSearchUIDFile", label = "Clear Subject IDs")))),
                    textOutput("WarnSubjectUIDFileColnames"),
                    textOutput("WarnSubjectUIDFileColnames2"),

                    fluidRow(
                      column(
                        width = 4,
                        selectizeInput("SearchByStudy",
                                       "Study",
                                       choices = c("", sampleDB::CheckTable(database = database, "study")$short_code))),
                      column(
                        width = 4,
                        selectizeInput("SearchByLocation",
                                       "Storage Location",
                                       choices = c("", sampleDB::CheckTable(database = database, "location")$description))),
                      column(
                        width = 4,
                        selectizeInput("SearchBySpecimenType",
                                       "Specimen Type",
                                       choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label)))),

                    hr(),
                    fluidRow(
                      column(
                        width = 12,
                        HTML("<center><h3><b>Search Results</b></h3></center>"),
                        DT::dataTableOutput("SearchResultsTable"))),

                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        downloadButton("downloadData", "Download")))),

           tabPanel("Move Samples",

                    fluidRow(
                      column(
                        width = 4,
                        
                        HTML("<h4><b>Move Samples Form</b></h4>"),
                        br(),
                        
                        fluidRow(
                          column(
                            
                            width = 12,
                            fileInput("MoveDataSet",
                                      "SampleDB MoveSamplesCSV",
                                      multiple = TRUE,
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))),

                          fluidRow(
                            column(
                              width = 12,
                              actionButton("MoveAction",
                                           label = "Move Samples",
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                              actionButton("ClearMoveForm",
                                           label = "Clear Form"))),
                        textOutput("WarningMoveColnames"),

                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              verbatimTextOutput("MoveReturnMessage1"))),
                        
                          fluidRow(
                            column(
                              width = 6,
                              verbatimTextOutput("MoveReturnMessage2")))),
                      
                        column(
                          width = 8,
                          HTML("<h4>This is an <b>Example SampleDB MoveSamplesCSV</b> from VisionMate.</h4>"),
                          verbatimTextOutput("ExampleMoveSamplesCSV"),
                          HTML("<h5>This format is essentially just the CSV that any micronix instrument creates.
                                No CSV reformating is required.</h5>"),
                          br(),
                          HTML("<h5>To perform moves:</h5>"),
                          HTML("<ol>
                                  <li>All plates involved in the move need to be scanned</li>
                                  <li>The Micronix .csv file needs to be given the plate name</li>
                                  <li>All the scans must be uploaded together</li>
                              </ol>"),
                          # HTML("<h6>*Note that no new samples can be added during moves. This is because no
                          # study subject nor specimen type information is processed during moves.</h6>"),
                        ))),
           
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
                                           choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid)),
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
                        HTML("<code>BLANK</code>")),
                      column(
                        width = 4,
                        HTML("<code>BLANK</code>")),
                        )),
           
           navbarMenu("References",

                      tabPanel("Freezers",

                               fluidRow(
                                 column(4,
                                        h3("Add a Freezer"),

                                        fluidRow(
                                          column(width = 12,
                                                 textInput("AddFreezer",
                                                           label = NULL,
                                                           placeholder = "New Name"))),
                                        fluidRow(
                                          column(width = 1,
                                                 actionButton("AddFreezerAction",
                                                              label = "Add",
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                        textOutput("WarningFreezerNameUnique"),

                                        HTML("<h3>Rename a Freezer</h3>"),
                                        fluidRow(
                                          column(
                                            width = 12,
                                            selectInput("RenameFreezer1",
                                                        label = "Current Name",
                                                        choices = c("", sampleDB::CheckTable(database = database, "location")$description),
                                                        selected = 1))),

                                        fluidRow(
                                          column(
                                            width = 12,
                                            textInput("RenameFreezer2",
                                                      label = NULL,
                                                      placeholder = "New Name"))),

                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton("RenameFreezerAction",
                                                         label = "Rename",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                          textOutput("modify_freezer_warning"),
                                          # textOutput("modify_freezer_warning2"),
                                          ),

                                        h3("Remove a Freezer"),
                                        fluidRow(
                                          column(
                                            width = 12,
                                            selectInput("DeleteFreezer",
                                                        label = NULL,
                                                        choices = c("", sampleDB::CheckTable(database = database, "location")$description),
                                                        selected = 1))),
                                        textOutput("delete_freezer_delete_warning"),

                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton("DeleteFreezerAction",
                                                         label = "Delete",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                                        br(),
                                        fluidRow(
                                          column(
                                            width = 6,
                                            span(verbatimTextOutput("FreezerReturnMessage"), style="font-size: 28px"))),

                                        ),

                                 column(8,
                                   h3("Freezers"),
                                   DT::dataTableOutput("TableFreezer")))),

                      tabPanel("Specimen Types",

                               fluidRow(
                                 column(4,

                                        h3("Add a Specimen Type"),
                                        fluidRow(
                                          column(width = 12,
                                                 textInput("AddSpecimenType",
                                                           label = NULL,
                                                           placeholder = "New Name"))),

                                        fluidRow(
                                          column(width = 1,
                                                 actionButton("AddSpecimenTypeAction",
                                                              label = "Add",
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                          textOutput("WaringAddSpecimenTypeUnique"),

                                        HTML("<h3>Rename a Specimen Type</h3>"),
                                        fluidRow(
                                          column(
                                            width = 12,
                                            selectInput("RenameSpecimenType1",
                                                        label = "Current Specimen Type Name",
                                                        choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label),
                                                        selected = 1))),

                                        fluidRow(
                                          column(
                                            width = 12,
                                            textInput("RenameSpecimenType2",
                                                      label = NULL,
                                                      placeholder = "New Name"))),
                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton("RenameSpecimenTypeAction",
                                                         label = "Rename",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                          textOutput("WarningChangeSpecimenTypeUnique")),

                                        h3("Remove a Specimen Type"),
                                        fluidRow(
                                          column(
                                            width = 12,
                                            selectInput("DeleteSpecimenType",
                                                        label = NULL,
                                                        choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label),
                                                        selected = 1))),
                                        textOutput("WarningSpecimenTypeDeletion"),

                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton("DeleteSpecimenTypeAction",
                                                         label = "Delete",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                                        br(),
                                        
                                        fluidRow(
                                          column(
                                            width = 6,
                                            span(verbatimTextOutput("SpecimenReturnMessage"), style="font-size: 28px"))),
                                        ),

                                 column(8,
                                   h3("Specimen Types"),
                                   DT::dataTableOutput("TableSpecimenType"))),),

                      tabPanel("Studies",

                               fluidRow(
                                 column(4,

                                        h3("Add a Study"),
                                        fluidRow(

                                          column(
                                            width = 6,
                                            textInput("AddStudyTitle",
                                                      label = "Title",
                                                      placeholder = "New Title")),
                                          column(
                                            width = 6,
                                            textInput("AddStudyDescription",
                                                      label = "Description",
                                                      placeholder = "New Description"))),

                                        fluidRow(
                                          column(
                                            width = 6,
                                            textInput("AddStudyLeadPerson",
                                                      label = "Lead Person",
                                                      placeholder = "New Lead Person")
                                          ),
                                          column(
                                            width = 6,
                                            textInput("AddStudyShortCode",
                                                      label = "Short Code",
                                                      placeholder = "New Short Code"))),

                                        fluidRow(
                                          column(
                                            width = 6,
                                            checkboxInput("AddStudyIsLongitudinal",
                                                          label = "Londitudinal",
                                                          value = FALSE)),
                                          column(
                                            width = 6,
                                            checkboxInput("AddStudyIsHidden",
                                                          label = "Hidden",
                                                          value = FALSE))),
                                        fluidRow(
                                          column(width = 1,
                                                 actionButton("AddStudyAction",
                                                              label = "Add",
                                                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                        textOutput("WarningStudyAddTitleUnique"),
                                        textOutput("WarningStudyAddShortCodeUnique"),

                                        h3("Rename a Study"),
                                        fluidRow(
                                          column(
                                            width = 3,
                                            DT::dataTableOutput("RenamePreview"))),

                                        fluidRow(
                                          column(
                                            width = 6,
                                            textInput("RenameStudyTitle",
                                                      label = "Title",
                                                      placeholder = "New Title")),
                                          column(
                                            width = 6,
                                            textInput("RenameStudyDescription",
                                                      label = "Description",
                                                      placeholder = "New Description"))),

                                        fluidRow(
                                          column(
                                            width = 6,
                                            textInput("RenameStudyLeadPerson",
                                                      label = "Lead Person",
                                                      placeholder = "New Lead Person")),
                                          column(
                                            width = 6,
                                            textInput("RenameStudyShortCode",
                                                      label = "Short Code",
                                                      placeholder = "New Short Code"))),

                                        fluidRow(
                                          column(
                                            width = 6,
                                            checkboxInput("RenameStudyIsLongitudinal",
                                                          label = "Londitudinal",
                                                          value = FALSE)),
                                          column(
                                            width = 6,
                                            checkboxInput("RenameStudyIsHidden",
                                                          label = "Hidden",
                                                          value = FALSE))),

                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton("RenameStudyAction",
                                                         label = "Rename",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                        textOutput("WarningStudyChangeTitleUnique"),
                                        textOutput("WarningStudyChangeShortCodeUnique"),

                                        h3("Remove a Study"),
                                        
                                        fluidRow(
                                          column(
                                            width = 8,
                                            textOutput("WarnStudyDeletion"),
                                        
                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton("DeleteStudyAction",
                                                         label = "Delete",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                        
                                        br(),
                                        
                                        fluidRow(
                                          column(
                                            width = 8,
                                            span(verbatimTextOutput("StudyReturnMessage"), style="font-size: 28px")))

                                          ))),

                                 column(8,
                                        h3("Studies"),
                                        DT::dataTableOutput("TableStudy"))))),
           tabPanel("About",

                    fluidRow(column(width = 4))

           )

)
