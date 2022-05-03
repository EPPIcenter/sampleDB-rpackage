UISearchSamples <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 2,
      HTML("<h4>Search Samples</h4>"),
      hr(),
      # fileInput("SearchByLabel", label = HTML("Barcode <h6>Single column named \"barcode\"</h6>")), actionButton("ClearSearchBarcodes", label = "Clear Barcodes"), textOutput("WarnSubjectBarcodeFileColnames"), textOutput("WarnSubjectBarcodeFileColnames2"),
      radioButtons("SearchBySampleType","Sample Type", c("All" = "all", "Micronix" = "micronix", "Cryovial" = "cryo", "RDT" = "rdt", "Paper" = "paper"), selected = "micronix", inline = T),
      conditionalPanel(condition = "input.SearchBySampleType == \"micronix\"",
                       fileInput("SearchByBarcode", label = "Sample Barcodes"),
                       selectInput("SearchByPlate", label = "Plate Name", choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name))),
      conditionalPanel(condition = "input.SearchBySampleType == \"cryo\"",
                       fileInput("SearchByCryovialLabels", label = "Sample Labels"),
                       selectInput("SearchByBox", label = "Box Name", choices = c("", sampleDB::CheckTable(database = database, "box")$box_name))),
      conditionalPanel(condition = "input.SearchBySampleType == \"rdt\"",
                       fileInput("SearchByRDTLabels", label = "Sample Labels"),
                       selectInput("SearchByRDTBag", label = "Bag Name", choices = c("", sampleDB::CheckTable(database = database, "bag")$bag_name))),
      conditionalPanel(condition = "input.SearchBySampleType == \"paper\"",
                       fileInput("SearchByPaperLabels", label = "Sample Labels"),
                       selectInput("SearchByPaperBag", label = "Bag Name", choices = c("", sampleDB::CheckTable(database = database, "bag")$bag_name))),
      hr(),
      selectizeInput("SearchByStudy", "Study", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code)),
      radioButtons("SubjectUIDSearchType", label = "Study Subject Search Method", choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"), selected = "individual"),
      conditionalPanel(condition = "input.SubjectUIDSearchType == \"individual\"",
                       selectizeInput("SearchBySubjectUID", label = "Study Subject", choices = c())),
      conditionalPanel(condition = "input.SubjectUIDSearchType == \"multiple\"",
                       fileInput("SearchBySubjectUIDFile", label = HTML("Study Subjects <h6>Single column named \"subject_uid\"</h6>")), actionButton("ClearSearchUIDFile", label = "Clear Study Subjects")),
      selectizeInput("SearchBySpecimenType", "Specimen Type", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label)),
      dateRangeInput("dateRange", label = "Collection Dates", start = NA, end = NA) %>% suppressWarnings(),
      selectizeInput("SearchByLocation", "Storage Location", choices = c("", sampleDB::CheckTable("location")$location_name)),
      selectizeInput("SearchByLevelI", "Storage Location: Level I", choices = c("")),
      selectizeInput("SearchByLevelII", "Storage Location: Level II", choices = c("")),
      selectizeInput("SearchByExhausted", "Archived", choices = c("", TRUE, FALSE)),
      textOutput("WarnSubjectUIDFileColnames"),
      textOutput("WarnSubjectUIDFileColnames2")
    ),
    mainPanel(
      width = 10,
      DT::dataTableOutput("SearchResultsTable"),
      downloadButton("downloadData", "Download")
    ))
}