library(RSQLite)
UISearchDelArchSamples <- function(){
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  ui <- sidebarLayout(
    sidebarPanel(
      width = 2,
      # actionButton("verify_delarch", label = NULL),
      HTML("<h4>Search, Delete and Archive Samples</h4>"),
      hr(),
      # fileInput("SearchByLabel", label = HTML("Barcode <h6>Single column named \"barcode\"</h6>")), actionButton("ClearSearchBarcodes", label = "Clear Barcodes"), textOutput("WarnSubjectBarcodeFileColnames"), textOutput("WarnSubjectBarcodeFileColnames2"),

      radioButtons("DelArchSearchBySampleType","Sample Type", choices = c("All" = "all", DBI::dbReadTable(con, "sample_type") %>% filter(is.null(parent_id)) %>% pull(id, name = "name")), selected = "all", inline = T),
      hr(),
      actionButton("DelArchSearchReset", width = '100%', label = "Reset Search Criteria", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      hr(),
      fileInput("DelArchSearchByBarcode", label = "Sample Barcodes"),
      selectizeInput("DelArchSearchByManifest", label = NULL, choices = c()),
      hr(),
      selectizeInput("DelArchSearchByStudy", "Study", choices = c("", CheckTable(database = database, "study")$short_code)),
      conditionalPanel(condition = "input.DelArchSubjectUIDSearchType == \"individual\"",
                       selectizeInput("DelArchSearchBySubjectUID", label = "Study Subject", choices = c())),
      conditionalPanel(condition = "input.DelArchSubjectUIDSearchType == \"multiple\"",
                       fileInput("DelArchSearchBySubjectUIDFile", label = NULL)),
      radioButtons("DelArchSubjectUIDSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"), selected = "individual"),
      selectizeInput("DelArchSearchBySpecimenType", "Specimen Type", choices = c("", CheckTable(database = database, "specimen_type")$name)),
      dateRangeInput("DelArchdateRange", label = "Collection Dates", start = NA, end = NA) %>% suppressWarnings(),
      selectizeInput("DelArchSearchByLocation", "Storage Location", choices = c("", CheckTable("location")$name)),
      selectizeInput("DelArchSearchByLevelI", "Storage Location: Level I", choices = c("")),
      selectizeInput("DelArchSearchByLevelII", "Storage Location: Level II", choices = c("")),
      selectizeInput("DelArchSearchByState", "State", choices = DBI::dbReadTable(con, "state")$name, selected = "Active"),
      selectizeInput("DelArchSearchByStatus", "Status", choices = c("In Use"), selected = "In Use")
    ),
    mainPanel(
      width = 10,
      reactableOutput("DelArchSearchResultsTable"),
      hr(),
      tags$em("Use the filters in the panel to the left to find samples that you wish to download. You may optionally select samples from the table to narrow down the samples that should be included. When you are finished, press the button below.", style = "color: grey;font-size: 18px;"),
      fluidRow(
        width = 3,
        column(
          width = 6, 
          downloadButton("DelArchDownloadSearchData", "Download")
        )
      ),
      tags$h3("Sample Archival & Deletion Workflows"),
      tags$hr(),
      tags$em("Select samples above to get started. In all workflows, users will be asked to confirm that they have selected the correct samples.", style = "color: grey;font-size: 18px;"),
      fluidRow(
        width = 3,
        column(
          width = 6, 
          tagList(
            tags$h4("Sample Archival"), 
            tags$hr(),
            tags$p("Select sample above that you wish to archive. This will remove samples from plates in the database so that they may be replaced with", tags$em("In Use"), "samples."),
            actionButton("ArchiveAction", width = '25%', label = "Archive Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        column(
          width = 6, 
          tagList(
            tags$h4("Sample Deletion"), 
            tags$hr(),
            tags$p("Select sample above that you wish to delete. Samples that are", tags$em("deleted"), "are removed", tags$strong("permanently", style = "color:red"),". Use caution when deleting samples - in most cases, archival should be used to retain sample history. An example of when to delete a sample is if a sample has been uploaded by mistake."),
            actionButton("DeleteAction", width = '25%', label = "Delete Samples", style="color:#c4244c; background-color: #fff4f4; border-color: #c4244c")
          )
        )
      ),
      verbatimTextOutput("DelArchMessage"),
      # conditionalPanel(condition = "input.RenameStudyDescription == \"xxx\"",
      #                  br(),
      #                  HTML("Type \"Yes\" if you would like to archive these samples."),
      #                  textInput("zzz", label = NULL),
      #                  actionButton("yes1", label = "Enter"),
      #                  verbatimTextOutput("yesout")),
    ))

  DBI::dbDisconnect(con)

  return (ui)
}