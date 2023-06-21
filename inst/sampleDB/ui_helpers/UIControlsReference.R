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
        column(width = 2, selectizeInput("InputControlSearchBatch", width = '100%', label = "Batch", choices = tbl(con, "study") %>% filter(!is.na(control_collection_id)) %>% pull(short_code), selected=FALSE)),
        column(width = 2, selectizeInput("InputControlSearchStrain", width = '100%', label = "Strain", choices = tbl(con, "strain") %>% pull(name), selected=FALSE)),
        column(width = 2, selectizeInput("InputControlSearchDensity", width = '100%', label = "Density", choices = tbl(con, "control") %>% pull(density) %>% unique(.), selected=FALSE)),
        column(width = 2, selectizeInput("InputControlSearchPercentage", width = '100%', label = "Percentage", choices = tbl(con, "control_strain") %>% pull(percentage) %>% unique(.), selected=FALSE)),
        column(width = 2, dateRangeInput("InputControlSearchDateRange", label = "Dates", start = NA, end = NA)),
      ),
      fluidRow(
        column(width = 2, selectInput("InputControlLocationRoot", label = "Freezer", choices = tbl(con, "location") %>% pull(name) %>% unique(.))),        
        column(width = 2, selectInput("InputControlLocationLevelI", label = "Shelf Name", choices = c())),
        column(width = 2, selectInput("InputControlLocationLevelII", label = "Basket Name", choices = c()))
      ),
      reactableOutput("ControlTableOutput"),
      downloadButton("DownloadControlData", "Download"),

      tags$h3("Control Archival & Deletion Workflows"),
      tags$hr(),
      tags$em("Select controls above to get started. In all workflows, users will be asked to confirm that they have selected the correct controls", style = "color: grey;font-size: 18px;"),
      fluidRow(
        width = 3,
        column(
          width = 6, 
          tagList(
            tags$h4("Control Archival"), 
            tags$hr(),
            tags$p("Select controls above that you wish to archive. This will remove controls from plates in the database so that they may be replaced with", tags$em("In Use"), "controls"),
            actionButton("InputControlArchiveAction", width = '25%', label = "Archive Controls", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        column(
          width = 6, 
          tagList(
            tags$h4("Control Deletion"), 
            tags$hr(),
            tags$p("Select controls above that you wish to delete. Controls that are", tags$em("deleted"), "are removed", tags$strong("permanently", style = "color:red"),". Use caution when deleting controls - in most cases, archival should be used to retain sample history. An example of when to delete a sample is if a sample has been uploaded by mistake."),
            actionButton("InputControlDeleteAction", width = '25%', label = "Delete Controls", style="color:#c4244c; background-color: #fff4f4; border-color: #c4244c")
          )
        )
      ),
    )
  )

  DBI::dbDisconnect(con) 
  return(ui)
}