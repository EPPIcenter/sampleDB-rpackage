UIStudiesReference <- function(){
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
      selectInput("ChangeStudyShortCode", width = "47.5%", label = "Study to be modified", choices = NULL),
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
      selectInput("DeleteStudyShortCode", width = "47.5%", label = "Study to be deleted", choices = NULL),
      textOutput("WarnStudyDeletion"),
      actionButton("DeleteStudyAction", label = "Delete", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      HTML("<br></br>"),
      span(verbatimTextOutput("StudyReturnMessage"), style="font-size: 28px")
    ),
    mainPanel(
      width = 8,
      HTML("<h4><b>Studies</b></h4>"),
      DT::dataTableOutput("TableStudy")))
}