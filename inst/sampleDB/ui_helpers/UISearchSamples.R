library(RSQLite)
UISearchSamples <- function(){
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 2,
      HTML("<h4>Search Samples</h4>"),
      hr(),
      # fileInput("SearchByLabel", label = HTML("Barcode <h6>Single column named \"barcode\"</h6>")), actionButton("ClearSearchBarcodes", label = "Clear Barcodes"), textOutput("WarnSubjectBarcodeFileColnames"), textOutput("WarnSubjectBarcodeFileColnames2"),
      radioButtons("SearchBySampleType","Sample Type", choices = c("All" = "all", DBI::dbReadTable(con, "sample_type") %>% pull(id, name = "name")), selected = "all", inline = T),
      hr(),
      actionButton("SearchReset", width = '100%', label = "Reset Search Criteria", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      hr(),
      fileInput("SearchByBarcode", label = "Sample Barcodes"),
      selectInput("SearchByManifest", label = NULL, choices = c()),
      hr(),
      selectizeInput("SearchByStudy", "Study", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code)),
      conditionalPanel(condition = "input.SubjectUIDSearchType == \"individual\"",
                       selectizeInput("SearchBySubjectUID", label = "Study Subject", choices = c())),
      conditionalPanel(condition = "input.SubjectUIDSearchType == \"multiple\"",
                       fileInput("SearchBySubjectUIDFile", label = "Study Subject")),
      radioButtons("SubjectUIDSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"), selected = "individual"),
      selectizeInput("SearchBySpecimenType", "Specimen Type", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$name)),
      dateRangeInput("dateRange", label = "Collection Dates", start = NA, end = NA) %>% suppressWarnings(),
      selectizeInput("SearchByLocation", "Storage Location", choices = c("", sampleDB::CheckTable("location")$name)),
      selectizeInput("SearchByLevelI", "Storage Location: Level I", choices = c("")),
      selectizeInput("SearchByLevelII", "Storage Location: Level II", choices = c("")),
      selectizeInput("SearchByState", "State", choices = c(Global$DefaultStateSearchTerm)),
      selectizeInput("SearchByStatus", "Status", choices = c(Global$DefaultStatusSearchTerm)),
      textOutput("WarnSubjectUIDFileColnames"),
      textOutput("WarnSubjectUIDFileColnames2")
    ),
    mainPanel(
      width = 10,
      DT::dataTableOutput("SearchResultsTable"),
      downloadButton("downloadData", "Download")
    ))

    DBI::dbDisconnect(con)

    return(ui)
}