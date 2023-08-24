library(shiny)
library(bslib)

UISearchDelArchSamples <- function() {
  ui <- page_sidebar(
    title = "Search, Delete, and Archive",
    sidebar = sidebar(
      title = "Controls",
      hr(),
      actionButton("DelArchSearchReset", label = "Reset Search Criteria", width = '100%'),
      hr(),
      tabsetPanel(
        id = "DelArchSearchTypes",
        tabPanel("Samples",
          radioButtons("DelArchSearchBySampleType", "Sample Type", choices = c("Type1", "Type2")),
          fileInput("DelArchSearchByBarcode", label = "Sample Barcodes"),
          selectizeInput("DelArchSearchByManifest", label = "Manifest", choices = c())
        ),
        tabPanel("Controls",
          radioButtons("DelArchSearchByControlType", "Control Type", choices = c("Control1", "Control2"))
        )
      ),
      bslib::accordion(
        bslib::accordion_panel("Locations",
          selectizeInput("DelArchSearchByLocation", "Storage Location", choices = c("", "Location 1", "Location 2")),
          selectizeInput("DelArchSearchByLevelI", "Storage Location: Level I", choices = c("")),
          selectizeInput("DelArchSearchByLevelII", "Storage Location: Level II", choices = c(""))
        ),
        bslib::accordion_panel("State & Status",
          selectizeInput("DelArchSearchByState", "State", choices = c("Active", "Inactive")),
          selectizeInput("DelArchSearchByStatus", "Status", choices = c("In Use", "Not In Use"))
        ),
        bslib::accordion_panel("Specimens",
          selectizeInput("DelArchSearchBySpecimenType", "Specimen Type", choices = c("", "Type 1", "Type 2")),
          dateRangeInput("DelArchdateRange", label = "Collection Dates", start = NA, end = NA)
        ),
        bslib::accordion_panel("Study & Subjects",
          selectizeInput("DelArchSearchByStudy", "Study", choices = c("", "Study 1", "Study 2")),
          radioButtons("DelArchSubjectUIDSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"), selected = "individual"),
          conditionalPanel(
            condition = "input.DelArchSubjectUIDSearchType == 'individual'",
            selectizeInput("DelArchSearchBySubjectUID", label = "Study Subject", choices = c())
          ),
          conditionalPanel(
            condition = "input.DelArchSubjectUIDSearchType == 'multiple'",
            fileInput("DelArchSearchBySubjectUIDFile", label = NULL)
          )
        )
      )
    ),
    mainPanel(
      hr(),
      tags$em("Use the filters in the panel to the left to find samples.", style = "color: grey; font-size: 18px;"),
      fluidRow(
        column(
          width = 6,
          downloadButton("DelArchDownloadSearchData", "Download")
        )
      ),
      tags$h3("Sample Archival & Deletion Workflows"),
      hr(),
      tags$em("Select samples to get started. In all workflows, users will be asked to confirm that they have selected the correct samples.", style = "color: grey; font-size: 18px;"),
      fluidRow(
        column(
          width = 6,
          tags$h4("Sample Archival"),
          hr(),
          tags$p("Select samples above that you wish to archive. This will remove samples from plates in the database so that they may be replaced with", tags$em("In Use"), "samples."),
          actionButton("ArchiveAction", label = "Archive Samples", width = '25%')
        ),
        column(
          width = 6,
          tags$h4("Sample Deletion"),
          hr(),
          tags$p("Select samples above that you wish to delete. Samples that are", tags$em("deleted"), "are removed", tags$strong("permanently", style = "color: red"), ". Use caution when deleting samples - in most cases, archival should be used to retain sample history."),
          actionButton("DeleteAction", label = "Delete Samples", width = '25%')
        )
      ),
      verbatimTextOutput("DelArchMessage")
    )
  )
  return(ui)
}
