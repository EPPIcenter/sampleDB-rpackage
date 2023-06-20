UIControlsReference <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 4,
      tags$h4("Create a new strain"),
      textInput("InputControlNewStrain", "Strain", placeholder = "Add new strain here..."),
      textInput("InputControlStrainDesc", "Description", placeholder = "Optionally add description here..."),
      actionButton("InputCreateStrain", label = "Create"),
      fileInput("InputUploadStrains", label = "Upload Strains", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      actionButton("InputUploadStrainAction", label = "Upload"),
      hr(),
      tags$h4("Create a Control Batch"), # control batch enter here
      textInput("InputControlNewStudy", "Batch", placeholder = "Add new batch here..."),
      dateInput("InputControlDate", "Date"),
      textInput("InputControlStudyDesc", "Description", placeholder = "Optionally add description here..."),
      textInput("InputControlUrl", "URL", placeholder = "Add protocol resource locator here..."),
      textInput("InputControlBatchPerson", "Person", placeholder = "Add person who created the batch..."),
      actionButton("InputControlStudyAction", label = "Upload")
    ),
    mainPanel(
      fluidRow(
        column(width = 3, selectizeInput("InputControlSearchBatch", width = '100%', label = "Batch", choices = tbl(con, "study") %>% filter(!is.na(control_collection_id)) %>% pull(short_code), selected=FALSE)),
        column(width = 3, selectizeInput("InputControlSearchStrain", width = '100%', label = "Strain", choices = tbl(con, "strain") %>% pull(id, name = "name"), selected=FALSE)),
        column(width = 3, selectizeInput("InputControlSearchDensity", width = '100%', label = "Density", choices = tbl(con, "control") %>% pull(density) %>% unique(.), selected=FALSE)),
        column(width = 3, selectizeInput("InputControlSearchPercentage", width = '100%', label = "Percentage", choices = tbl(con, "control_strain") %>% pull(percentage) %>% unique(.), selected=FALSE)),
      ),
      reactableOutput("ControlTableOutput"),
      downloadButton("ControlsDownload", "Download")
    )
  )

  DBI::dbDisconnect(con) 
  return(ui)
}