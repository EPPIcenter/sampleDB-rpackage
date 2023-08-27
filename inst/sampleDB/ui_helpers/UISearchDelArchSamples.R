library(shiny)
library(bslib)

UISearchDelArchSamples <- function() {
  ui <- layout_sidebar(
    sidebar = sidebar(
      title = "Search Criteria",
      actionButton("DelArchSearchReset", label = "Reset Search Criteria", width = '100%'),
      radioButtons("DelArchSearchType", "Search Type", choices = c("Samples" = "samples", "Controls" = "controls"), selected = "samples"),
      conditionalPanel(
        condition = "input.DelArchSearchType == 'samples'",
        radioButtons("DelArchSearchBySampleType", "Sample Type", choices = get_sample_types()),
        fileInput("DelArchSearchByBarcode", label = "Sample Barcodes"),
        selectizeInput("DelArchSearchByManifest", label = "Container", choices = c())
      ),
      conditionalPanel(
        condition = "input.DelArchSearchType == 'controls'",
        radioButtons("DelArchSearchByControlType", "Control Type", choices = get_control_types())
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          id = "DelArchSubjectsPanel",
          title = "Study & Subjects",
          selectizeInput("DelArchSearchByStudy", "Study", choices = c()),
          radioButtons("DelArchSubjectUIDSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"), selected = "individual"),
          conditionalPanel(
            condition = "input.DelArchSubjectUIDSearchType == 'individual'",
            selectizeInput("DelArchSearchBySubjectUID", label = "Study Subject", choices = c())
          ),
          conditionalPanel(
            condition = "input.DelArchSubjectUIDSearchType == 'multiple'",
            fileInput("DelArchSearchBySubjectUIDFile", label = NULL)
          ),
          conditionalPanel(
            condition = "input.DelArchSearchType == 'controls'",
            selectizeInput("DelArchCompositionTypes", label = "Composition Type", choices = c()),
            selectizeInput("DelArchSearchByStrains", label = "Strains", choices = c())
          )
        ),
        conditionalPanel(
          condition = "input.DelArchSearchType == 'samples'",
          bslib::accordion_panel(
            id = "DelArchSpecimensPanel",
            title = "Specimens",
            selectizeInput("DelArchSearchBySpecimenType", "Specimen Type", choices = c("")),
            dateRangeInput("DelArchdateRange", label = "Collection Dates", start = NA, end = NA)
          )
        ),
        bslib::accordion_panel("Locations",
          selectizeInput("DelArchSearchByLocation", "Storage Location", choices = c("")),
          selectizeInput("DelArchSearchByLevelI", "Storage Location: Level I", choices = c("")),
          selectizeInput("DelArchSearchByLevelII", "Storage Location: Level II", choices = c(""))
        ),
        bslib::accordion_panel("State & Status",
          selectizeInput("DelArchSearchByState", "State", choices = c()),
          selectizeInput("DelArchSearchByStatus", "Status", choices = c())
        )
      )
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Track your data"
      ),
      card_body(
        reactableOutput("DelArchSearchResultsTable"),
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
        )
      ),
      verbatimTextOutput("DelArchMessage")
    )
  )
  return(ui)
}
