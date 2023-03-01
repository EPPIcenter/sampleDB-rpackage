library(DBI)
library(shinyBS)

UIMoveSamples <- function(){

  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  file_specs_json <- rjson::fromJSON(file = system.file(
    "extdata", "file_specifications.json", package = .sampleDB$pkgname))

  file_type_ids <- lapply(file_specs_json$file_types, function(x) x$id)
  names(file_type_ids) <- lapply(file_specs_json$file_types, function(x) x$name)

  ui <- sidebarLayout(
    sidebarPanel(
      width = 4,
      HTML("<h4><b>Move Samples</b></h4>"),
      HTML("<p>To move samples please select a storage type and fill out the sections below.</p>"),
      radioButtons("MoveSampleType","1. Sample Storage Type", DBI::dbReadTable(con, "sample_type") %>% pull(id, name = "name"), inline = T),
      hr(),
      radioButtons("MoveFileType", label = "2. Move File Type", choices = file_type_ids, inline = T),
      fileInput("MoveDataSet", "3. Move Samples File", width = '47%', multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("MoveAction", width = '100%', label = "Move Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
               column(width = 6, actionButton("ClearMoveForm", width = '100%', label = "Clear Form", style="color:#c4244c; background-color: #fff4f4; border-color: #c4244c"))),
      
      br(),
      actionButton("CreateNewManifest", label = "New Manifest"),
      #output messages
      verbatimTextOutput("MoveOutputConsole"),
    ),
    mainPanel(
      width = 7,
      HTML("<h2>Guide to Moving Samples</h2>"),
      br(),
      HTML("<h4><code>1. Choose Sample Storage Type</code></h4>"),
      hr(),
      HTML("<p>Use the <b>Sample Storage Type</b> section to select the sample storage type being moved.</p>"),
      br(),
      HTML("<h4><code>2. Create a Move File for Each Plate</code></h4>"),
      hr(),
      # HTML("<h4>Currently in the database...</h4>"),
      # fluidRow(
      #   column(width = 6,
      #          tableOutput("InDatabasePlateOne")),
      #   column(width = 6,
      #          tableOutput("InDatabasePlateTwo")),
      # ),
      HTML("<h4>A Real World Example:</h4>"),
      br(),
      HTML("<h4>Move samples amoungst Plate1 & Plate2</h4>"),
      HTML("Plate1 and Plate2 move files reflects what the database should contain once the sample storage items have been moved."),
      br(),
      br(),
      fluidRow(
        column(width = 6,
               HTML("<b>Plate1 Move File</b>"),
               tableOutput("PlateOneMove")
        ),
        column(width = 6,
               HTML("<b>Plate2 Move File</b>"),
               tableOutput("PlateTwoMove")
        ),
      ),
      br(),
      HTML("<h4>Save these files in a .csv format, after the plate associated with the data.</h4>"),
      br(),
      HTML("<b>Plate1 Move File -> </b><i>plate1.csv</i>"),
      br(),
      HTML("<b>Plate2 Move File -> </b><i>plate2.csv</i>")
    ))

  dbDisconnect(con)

  return(ui)
}