library(dplyr)
library(sampleDB)
library(DT)
library(shinyFeedback)
library(shiny)
library(markdown)


#SET PATH TO SQLITE DATABASE
database <- "example_19-Oct-21.sample_db.sqlite"

navbarPage("SampleDB",

           tabPanel("Upload New Samples",

                    shinyFeedback::useShinyFeedback(),
                    includeCSS("app.css"),

                    tags$head(
                      tags$style(HTML("
                        h5 {
                              line-height: 150%;
                            }"))
                    ),

                    fluidRow(
                      column(4,

                    HTML("<h4><b>Upload Samples Form</b></h4>"),
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        fileInput("UploadDataSet",
                                  "SampleDB UploadCSV",
                                  multiple = TRUE,
                                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))),

                    fluidRow(
                      column(
                        width = 12,
                        selectizeInput("UploadStudyShortCode",
                                       choices = c("", sampleDB::CheckTable(database = database, "study")$short_code),
                                       label = "Study Name"))),

                    fluidRow(
                      column(
                        width = 12,
                        textInput("UploadPlateID",
                                  label = "Unique Plate Name"))),
                    textOutput("upload_plate_dup_warning"),

                    fluidRow(
                      column(
                        width = 12,
                        selectInput("UploadLocation",
                                    label = "Storage Location",
                                    choices = sampleDB::CheckTable(database = database, "location")$description))),

                    fluidRow(
                      column(
                        width = 12,
                        actionButton(".UploadAction",
                                     label = "Upload Dataset",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                    br(),

                    fluidRow(
                      column(
                        width = 12,
                        span(textOutput("UploadReturnMessage"), style="font-size: 28px")))),

                    column(8,

                           fluidRow(
                             br(),
                             HTML("<h4>This is an <b>Example SampleDB UploadCSV</b> from VisionMate.</h4>"),
                             column(12,
                                    verbatimTextOutput("uploadcsv_nodate_example")),
                             HTML("<h5>To create this table join the <code>LocationRow</code>,
                                   <code>LocationColumn</code>, and <code>TubeCodes</code> from the VisionMate
                                   CSV with <code>study_subject_id</code> and <code>specimen_type</code> information,
                                   where the <code>study_subject_id</code> and <code>specimen_type</code>
                                   corresponds to the study subject id and specimen type associated with each sample</h5>")),

                           fluidRow(
                             HTML("<h4><b>Alternatives to VisionMate Format</b></h4>"),
                             HTML("<h5>If your data is was formatted using a different micronix instrument the process for
                                  creating a SampleDB UploadCSV is the same. Simply add the <code>study_subject_id</code>
                                  and <code>specimen_type</code> columns to your existing columns and the CSV is ready for upload.
                                  In other words uploading is agnostic with respect to the format of the micronix information</h5>")),

                           fluidRow(
                             HTML("<h4><b>Longitudinal Data</b></h4>"),
                             HTML("<h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                                  longitudinal information to the samples. (Date format is YMD.)</h5>"),
                             br(),
                             HTML("<center><h4><b>Example SampleDB UploadCSV</b> with <code>collection_date</code> Column</h4></center>"),
                             column(12,
                                    verbatimTextOutput("uploadcsv_date_example")
                             ))))),

           tabPanel("Search Existing Samples",

                    h4("Use Filters Below to Populate the Search Results Table", align = "center"),
                    br(),
                    fluidRow(
                      column(
                        width = 4,
                        fileInput("SearchByBarcode",
                                  HTML("Search By Barcode - single column named \"barcode\""),
                                   multiple = FALSE)),
                      column(
                        width = 4,
                        selectizeInput("SearchByPlateID",
                                       "Search By Plate ID",
                                       choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid))),
                      column(
                        width = 4,
                        selectizeInput("SearchBySubjectUID",
                                       "Search By Subject (UID)",
                                       choices = NULL))),

                    fluidRow(
                      column(
                        width = 4,
                        selectizeInput("SearchByStudy",
                                       "Search By Study",
                                       choices = c("", sampleDB::CheckTable(database = database, "study")$short_code))),
                      column(
                        width = 4,
                        selectizeInput("SearchByLocation",
                                       "Search By Location",
                                       choices = c("", sampleDB::CheckTable(database = database, "location")$description))),
                      column(
                        width = 4,
                        selectizeInput("SearchBySpecimenType",
                                       "Search By Specimen Type",
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
                        fluidRow(
                          column(
                            width = 12,
                            fileInput("MoveDataSet",
                                      "SampleDB MoveSamplesCSV",
                                      multiple = TRUE,
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))),

                        radioButtons("move_plate_type", label = "Move Samples to:",
                                     choices = list("New Plate" = "new_plate", "Existing Plate" = "existing_plate"),
                                     selected = "new_plate"),


                        conditionalPanel(
                          condition = "input.move_plate_type == \"new_plate\"",
                          textInput("MovePlateID",
                                    label = "New Plate Name"),
                          textOutput("move_plate_dup_warning"),
                          selectInput("MoveLocation",
                                      label = "Storage Location",
                                      choices = sampleDB::CheckTable(database = database, "location")$description),
                          ),

                        conditionalPanel(
                          condition = "input.move_plate_type == \"existing_plate\"",
                          selectizeInput("MoveExistingPlateID",
                                         choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid),
                                         label = "Existing Plate Name")),

                        fluidRow(
                          column(
                            width = 12,
                            actionButton(".MoveAction",
                                         label = "Move Samples",
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                        br(),
                        fluidRow(
                          column(
                            width = 12,
                            verbatimTextOutput("MoveReturnMessage")
                          )
                        ),
                      ),
                      column(
                        width = 8,
                        HTML("<h4>This is an <b>Example SampleDB MoveSamplesCSV</b> from VisionMate.</h4>"),
                        verbatimTextOutput("movetubescsv_example"),
                        HTML("<h5>This format is essentially just the CSV that any micronix instrument creates.
                              No CSV reformating is required.</h5>"),
                        HTML("<h6>*Note that no new samples can be added during moves. This is because no
                        study subject nor specimen type information is processed during moves.</h6>"),
                      )
                    ),



                    ),

           #navbarMenu allows for drop downs
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
                                                 actionButton(".AddFreezerAction",
                                                              label = "Add",
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                        textOutput("add_freezer_warning"),

                                        fluidRow(
                                          column(
                                            width = 12,
                                            selectInput(".RenameFreezer1",
                                                        label = h3("Rename a Freezer"),
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
                                            actionButton(".RenameFreezerAction",
                                                         label = "Rename",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                          textOutput("modify_freezer_warning")),

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
                                            actionButton(".DeleteFreezerAction",
                                                         label = "Delete",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                                        fluidRow(
                                          column(
                                            width = 4,
                                            span(textOutput("FreezerReturnMessage"), style="font-size: 28px"))),

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
                                                 actionButton(".AddSpecimenTypeAction",
                                                              label = "Add",
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                          textOutput("add_specimen_type_warning"),

                                        fluidRow(
                                          column(
                                            width = 12,
                                            selectInput(".RenameSpecimenType1",
                                                        label = h3("Rename a Specimen Type"),
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
                                            actionButton(".RenameSpecimenTypeAction",
                                                         label = "Rename",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                          textOutput("modify_specimen_type_warning")),

                                        h3("Remove a Specimen Type"),
                                        fluidRow(
                                          column(
                                            width = 12,
                                            selectInput("DeleteSpecimenType",
                                                        label = NULL,
                                                        choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label),
                                                        selected = 1))),
                                        textOutput("delete_specimen_delete_warning"),

                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton(".DeleteSpecimenTypeAction",
                                                         label = "Delete",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                                        fluidRow(
                                          column(
                                            width = 4,
                                            span(textOutput("SpecimenReturnMessage"), style="font-size: 28px"))),
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
                                                 actionButton(".AddStudyAction",
                                                              label = "Add",
                                                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                        textOutput("add_study_title_warning"),
                                        textOutput("add_study_short_code_warning"),

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
                                            actionButton(".RenameStudyAction",
                                                         label = "Rename",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                        textOutput("rename_study_title_warning"),
                                        textOutput("rename_study_short_code_warning"),

                                        h3("Remove a Study"),
                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton("DeleteStudyAction",
                                                         label = "Delete",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                                        fluidRow(
                                          column(
                                            width = 4,
                                            span(textOutput("StudyReturnMessage"), style="font-size: 28px"))),

                                        h3(""),
                                        fluidRow(
                                          column(
                                            width = 8,
                                            verbatimTextOutput("DeleteStudy")
                                          ))),

                                 column(8,
                                        h3("Studies"),
                                        DT::dataTableOutput("TableStudy"))))),
           tabPanel("About",

                    fluidRow(column(width = 4))

           )

)
