library(shiny)
library(bslib)

UISearchDelArchSamples <- function() {
  ui <- layout_sidebar(
    tags$script(HTML("
      var firstSelectedRow = null;
      var shiftKeyEnabled = false;
      $(document).on('click', '.rt-tbody .rt-tr-group', function(evt) {
        evt.preventDefault()
        var row = $(this).closest('.rt-tr-group').index();
        if (evt.shiftKey && firstSelectedRow !== null) {
          var start = Math.min(firstSelectedRow, row);
          var end = Math.max(firstSelectedRow, row);
          $('body').addClass('no-select');
          Shiny.setInputValue('selectedRange', [start, end], {priority: 'event'});
        } else {
          $('body').removeClass('no-select');
          firstSelectedRow = row;
        }
        shiftKeyEnabled = evt.shiftKey;
        Shiny.setInputValue('shiftKeyEnabled', shiftKeyEnabled);
      });
      ")
    ),
    sidebar = sidebar(
      title = "Search Criteria",
      actionButton("DelArchSearchReset", label = "Reset Search Criteria", width = '100%'),
      radioButtons("DelArchSearchType", "Search Type", choices = c("Samples" = "samples", "Controls" = "controls"), selected = "samples"),
      conditionalPanel(
        condition = "input.DelArchSearchType == 'samples'",
        radioButtons("DelArchSearchBySampleType", "Sample Type", choices = get_sample_types()),
        conditionalPanel(
          condition = "input.DelArchSearchType == 'samples' && input.DelArchSearchBySampleType == 'dbs_sample'",
          radioButtons("SearchDBSSampleManifest", "Select a DBS Container", inline = TRUE, choices = c("All" = "all", "Box" = "box", "Bag" = "bag"), selected = "all")
        ),
        conditionalPanel(
          condition = "input.DelArchSearchBySampleType == 'micronix' || input.DelArchSearchBySampleType == 'cryovial'",
          fileInput("DelArchSearchByBarcode", label = "Sample Barcodes")
        ),
        selectizeInput("DelArchSearchByManifest", label = "Container", choices = c())
      ),
      conditionalPanel(
        condition = "input.DelArchSearchType == 'samples' && input.DelArchSearchBySampleType == 'micronix'",
        actionLink("DelArchAdvancedSearchLink", "Advanced...")
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
            selectizeInput("DelArchCompositionTypes", label = "Composition Types", choices = c()),
            selectizeInput("DelArchSearchByStrains", label = "Strains", choices = c()),
            selectizeInput("DelArchSearchByPercentages", label = "Percentages", choices = c()),
            selectizeInput("DelArchSearchByDensity", label = "Density", choices = c())
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
          selectizeInput("DelArchSearchByState", "State", choices = c("")),
          selectizeInput("DelArchSearchByStatus", "Status", choices = c(""))
        )
      ),      
      verbatimTextOutput("DelArchMessage")
    ),
    layout_sidebar(
      sidebar = sidebar(
        title = "Sample Archival & Deletion Workflows", 
        position = "right",
        open = FALSE,
        border = FALSE,
        tags$p("Select samples to get started. In all workflows, users will be asked to confirm that they have selected the correct samples."),
        tags$h4("Sample Archival"),
        tags$p("Select samples above that you wish to archive. This will remove samples from plates in the database so that they may be replaced with", tags$em("In Use"), "samples."),
        actionButton("ArchiveAction", label = "Archive Samples", width = '100%'),
        tags$h4("Sample Deletion"),
        tags$p("Select samples above that you wish to delete. Samples that are", tags$em("deleted"), "are removed", tags$strong("permanently", style = "color: red"), ". Use caution when deleting samples - in most cases, archival should be used to retain sample history."),
        actionButton("DeleteAction", label = "Delete Samples", width = '100%')
      ),
      tags$style(HTML("
        .btn-group {
          display: inline-flex;
          align-items: center;
          position: relative;
        }
        .btn-group > .btn {
          margin-right: -4px; /* Overlap the buttons slightly */
          border-radius: 0; /* Remove border-radius */
          border: none; /* Remove border */
          box-shadow: none; /* Remove any box shadow */
        }
        .btn-group > .btn:first-child {
          border-top-left-radius: 4px;
          border-bottom-left-radius: 4px;
        }
        .btn-group > .btn:last-child {
          border-top-right-radius: 4px;
          border-bottom-right-radius: 4px;
          padding-left: 8px;
          padding-right: 8px;
        }
        .dropdown-menu {
          min-width: 100px;
          left: 0; /* Align the dropdown with the button */
          top: 100%; /* Position it below the button */
          margin-top: 0.25rem; /* Small margin between button and dropdown */
        }
      ")),
      
      card(
        card_header("Search Results"),
        border = FALSE,
        full_screen = TRUE,
        reactableOutput("DelArchSearchResultsTable"),
        fluidRow(
          column(
            width = 6,
            div(
              class = "btn-group",
              downloadButton("DelArchDownloadSearchData", "Download", class = "btn btn-primary"),
              tags$button(
                class = "btn btn-primary dropdown-toggle",
                type = "button",
                `data-bs-toggle` = "dropdown",
                `aria-expanded` = "false"
              ),
              tags$ul(
                class = "dropdown-menu",
                tags$li(actionButton("download_qpcr_quantstudio", "Download qPCR Format (QuantStudio)", class = "dropdown-item")),
                tags$li(actionButton("download_qpcr_biorad", "Download qPCR Format (BioRad)", class = "dropdown-item"))
              )
            ),
            actionButton("editComment", "Edit Comment", style = "margin-left: 10px;")  
          )
        )
      )
    )
  )
  return(ui)
}
