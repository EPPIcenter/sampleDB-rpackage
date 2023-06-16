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
      reactableOutput("ControlTableOutput"),
      downloadButton("ControlsDownload", "Download")
    )
  )

  DBI::dbDisconnect(con) 
  return(ui)
}