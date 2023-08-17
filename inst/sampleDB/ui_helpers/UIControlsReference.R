UIControlsReference <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 4,
      tabsetPanel(
        # tags$em("Select 'Individual' to upload one strain, or 'Multiple' to upload a csv containing strains", style = "color: grey;font-size: 14px;"),
        tabPanel("Create a New Strain",
          br(),
          radioButtons("InputControlStrainUploadType", "Upload Type", choices=c("Individual" = "individual", "Multiple"="multiple"), selected="individual", inline=TRUE), 
          conditionalPanel(condition = "input.InputControlStrainUploadType == \"individual\"",
            textInput("InputControlNewStrain", "Strain", placeholder = "Add new strain here..."),
            actionButton("InputCreateStrain", label = "Create")
          ),
          conditionalPanel(condition = "input.InputControlStrainUploadType == \"multiple\"",
            fileInput("InputUploadStrains", label = "Upload Strains", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            actionButton("InputUploadStrainAction", label = "Upload")
          ),
        ),
        # New tabPanel for "Batch IDs"
        tabPanel("Batch IDs",
          br(),
          dateInput("InputCreateBatchID", label = "Batch ID", format = "yyyy-mm-dd"),
          textInput("InputCreateBatchDescription", label = "Description", placeholder = "Description"),
          textInput("InputCreateBatchLeadPerson", label = "Lead Person"),
          actionButton("InputBatchIDUploadAction", label = "Upload")
        ),
        # New tabPanel for "Composition IDs"
        tabPanel("Composition IDs",
          br(),
          fileInput("InputUploadCompositionIDs", label = "Upload Compositions", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          actionButton("InputCompositionIDUploadAction", label = "Upload")
        )
      )
    ),
    mainPanel(
      radioButtons("InputControlPanelType", "Control Storage Type", choices=get_control_types(), selected = "dbs_collection", inline=TRUE),
      fluidRow(
        column(width = 2, selectizeInput("InputControlSearchBatch", width = '100%', label = "Batch", choices = c("", tbl(con, "study") %>% pull(short_code)), selected="")),
        column(width = 2, selectizeInput("InputControlSearchStrain", width = '100%', label = "Strain", choices = c("", tbl(con, "strain") %>% pull(name)), selected="")),
        column(width = 2, selectizeInput("InputControlSearchDensity", width = '100%', label = "Density", choices = c("", tbl(con, "malaria_blood_control") %>% pull(density) %>% unique(.)), selected="")),
        column(width = 2, selectizeInput("InputControlSearchPercentage", width = '100%', label = "Percentage", choices = c("", tbl(con, "composition_strain") %>% pull(percentage) %>% unique(.)), selected="")),
      ),
      fluidRow(
        column(width = 2, dateRangeInput("InputControlSearchDateRange", label = "Dates", start = NA, end = NA)),
        column(width = 2, selectInput("InputControlLocationRoot", label = "Freezer", choices = c("", tbl(con, "location") %>% pull(location_root) %>% unique(.)), selected="")),        
        column(width = 2, selectInput("InputControlLocationLevelI", label = "Shelf Name", choices = c())),
        column(width = 2, selectInput("InputControlLocationLevelII", label = "Basket Name", choices = c()))
      ),
      conditionalPanel(condition = "input.InputControlPanelType == \"dbs_collection\"",
        reactableOutput("OutputDBSCollectionMainTable"),
        reactableOutput("OutputDBSCollectionCompositionTable")
      ),
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