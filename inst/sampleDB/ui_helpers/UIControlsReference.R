UIControlsReference <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 4,
      tags$h4("Search for control keys"),
      tags$hr(),
      selectizeInput("InputControlKey", "Control Keys", selected = FALSE, choices = DBI::dbReadTable(con, "control_combination_key") %>% pull(id, name="name")),
      selectizeInput("InputControlStrain", "Strains", selected = FALSE, choices = DBI::dbReadTable(con, "strain") %>% pull(id, name="name")),
      tags$hr(),
      tags$h4("Create a new strain"),
      textInput("InputControlNewStrain", "Strain", placeholder = "Add new strain here..."),
      textInput("InputControlStrainDesc", "Description", placeholder = "Optionally add description here..."),
      actionButton("InputCreateStrain", label = "Create"),
      tags$hr(),
      tags$h4("Upload control keys"),
      fileInput("InputUploadControls", label = "Upload Allowed Control Combinations", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      actionButton("InputUploadControlAction", label = "Upload")
    ),
    mainPanel(
      reactableOutput("ControlTableOutput"),
      downloadButton("ControlsDownload", "Download")
    )
  )

  DBI::dbDisconnect(con) 
  return(ui)
}