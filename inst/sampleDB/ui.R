#make an app that allows user to modify the freezer table
library(markdown)
library(sampleDB)
library(DT)

#NOTE make sure these globals are updated when referrence changes are made
#bc upload info is selected using dropdown for referrences
# freezer_names <- sampleDB::CheckTable("location") %>%
#   dplyr::select(description) %>%
#   dplyr::pull()
#
# specimen_type_names <- sampleDB::CheckTable("specimen_type") %>%
#   dplyr::select(label) %>%
#   dplyr::pull()

navbarPage("SampleDB",

           tabPanel("Upload New Samples",

                    shinyFeedback::useShinyFeedback(),
                    includeCSS("app.css"),

                    fluidRow(
                      column(
                        width = 4,
                        fileInput("UploadDataSet",
                                  "Choose CSV File",
                                  multiple = TRUE,
                                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
                      column(
                        width = 2,
                        radioButtons("CSVUploadType",
                                     "Instrument",
                                     choices = c("Traxer" = "traxer",
                                                 "Vision Mate" = "vision_mate"),
                                     selected = "vision_mate")),
                      column(
                        width = 2,
                        radioButtons("LongitudinalUpload",
                                     "Longitudinal",
                                     choices = c("True" = "true_longitudinal",
                                                 "False" = "false_longitudinal"),
                                     selected = "false_longitudinal"))),

                    fluidRow(
                      column(
                        width = 4,
                        textInput("UploadPlateID",
                                  label = "Unique Plate ID")),
                      column(
                        width = 4,
                        selectInput("UploadLocation",
                                  label = "Location",
                                  choices = sampleDB::CheckTable("location")$description))),
                    textOutput("upload_plate_dup_warning"),

                    fluidRow(
                      column(
                        width = 4,
                        selectizeInput("UploadStudyShortCode",
                                       choices = c("", CheckTable("study")$short_code),
                                       label = "Study Short Code"))),

                    fluidRow(
                      column(
                        width = 12,
                        actionButton(".UploadAction",
                                     label = "Upload Dataset",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                    h3(""),

                    fluidRow(
                      column(
                        width = 4,
                        verbatimTextOutput("UploadReturnMessage")))),

           tabPanel("Search Existing Samples",

                    fluidRow(
                      column(
                        width = 4,
                        fileInput("SearchByBarcode",
                                  "Search By Barcode",
                                   multiple = FALSE)),
                      column(
                        width = 4,
                        selectizeInput("SearchByPlateID",
                                       "Search By Plate ID",
                                       choices = c("", CheckTable("matrix_plate")$uid))),
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
                                       choices = c("", CheckTable("study")$short_code))),
                      column(
                        width = 4,
                        selectizeInput("SearchByLocation",
                                       "Search By Location",
                                       choices = c("", CheckTable("location")$description))),
                      column(
                        width = 4,
                        selectizeInput("SearchBySpecimenType",
                                       "Search By Specimen Type",
                                       choices = c("", CheckTable("specimen_type")$label)))),

                    fluidRow(
                      column(
                        width = 12,
                        actionButton(".SearchAction",
                                     label = "Search Dataset",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                    h3(""),
                    fluidRow(
                      column(
                        width = 12,
                        DT::dataTableOutput("SearchResultsTable"))),

                    fluidRow(
                      column(
                        width = 12,
                        downloadButton("downloadData", "Download")))),

           tabPanel("Move Samples",

                    fluidRow(
                      column(
                        width = 4,
                        fileInput("MoveDataSet",
                                  "Choose CSV File",
                                  multiple = TRUE,
                                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
                      column(
                        width = 2,
                        radioButtons("CSVMoveType",
                                     "Instrument",
                                     choices = c("Traxer" = "traxer",
                                                 "Vision Mate" = "vision_mate"),
                                     selected = "vision_mate")),
                      column(
                        width = 2,
                        radioButtons("LongitudinalMove",
                                     "Longitudinal",
                                     choices = c("True" = "true_longitudinal",
                                                 "False" = "false_longitudinal"),
                                     selected = "false_longitudinal"))),

                    fluidRow(
                      column(
                        width = 4,
                        textInput("MovePlateIDNew",
                                  label = "Unique Plate ID")),

                      column(
                        width = 4,
                        selectizeInput("MovePlateIDExisting",
                                       c("", CheckTable("matrix_plate")$uid),
                                       label = "Existing Plate ID")),

                      column(
                        width = 4,
                        selectInput("MoveLocation",
                                    label = "Location",
                                    choices = sampleDB::CheckTable("location")$description))),
                    textOutput("move_plate_dup_warning"),

                    fluidRow(
                      column(
                        width = 4,
                        selectizeInput("MoveStudyShortCode",
                                       choices = c("", CheckTable("study")$short_code),
                                       label = "Study Short Code"))),

                    fluidRow(
                      column(
                        width = 12,
                        actionButton(".MoveAction",
                                     label = "Upload Dataset",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),

                    h3(""),
                    fluidRow(
                      column(
                        width = 4,
                        verbatimTextOutput("MoveReturnMessage"))),

                    ),

           #navbarMenu allows for drop downs
           navbarMenu("Referrences",

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
                                                        choices = c("", CheckTable("location")$description),
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
                                                        choices = c("", CheckTable("location")$description),
                                                        selected = 1))),
                                        textOutput("delete_freezer_delete_warning"),

                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton(".DeleteFreezerAction",
                                                         label = "Delete",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))

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
                                                        choices = c("", sampleDB::CheckTable("specimen_type")$label),
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
                                                        choices = c("", sampleDB::CheckTable("specimen_type")$label),
                                                        selected = 1))),
                                        textOutput("delete_specimen_delete_warning"),

                                        fluidRow(
                                          column(
                                            width = 1,
                                            actionButton(".DeleteSpecimenTypeAction",
                                                         label = "Delete",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),

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
