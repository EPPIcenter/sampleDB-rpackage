# Function for the "Create a New Strain" tab
createStrainTab <- function() {
  tabPanel("Create a New Strain",
    br(),
    radioButtons("InputControlStrainUploadType", "Upload Type", choices=c("Individual" = "individual", "Multiple"="multiple"), selected="individual", inline=TRUE), 
    conditionalPanel(condition = "input.InputControlStrainUploadType == 'individual'",
      textInput("InputControlNewStrain", "Strain", placeholder = "Add new strain here..."),
      actionButton("InputCreateStrain", label = "Create")
    ),
    conditionalPanel(condition = "input.InputControlStrainUploadType == 'multiple'",
      fileInput("InputUploadStrains", label = "Upload Strains", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      actionButton("InputUploadStrainAction", label = "Upload")
    )
  )
}

# Function for the "Batch IDs" tab
batchIDsTab <- function() {
  tabPanel("Batch IDs",
    br(),
    dateInput("InputCreateBatchID", label = "Batch ID", format = "yyyy-mm-dd"),
    textInput("InputCreateBatchDescription", label = "Description", placeholder = "Description"),
    textInput("InputCreateBatchLeadPerson", label = "Lead Person"),
    actionButton("InputBatchIDUploadAction", label = "Upload")
  )
}

# Function for the "Composition IDs" tab
compositionIDsTab <- function() {
  tabPanel("Composition IDs",
    br(),
    fileInput("InputUploadCompositionIDs", label = "Upload Compositions", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    actionButton("InputCompositionIDUploadAction", label = "Upload")
  )
}

# Function for sidebar panel with tabs
sidebarWithTabs <- function() {
  sidebarPanel(
    shinyjs::useShinyjs(),
    width = 4,
    tabsetPanel(
      createStrainTab(),
      batchIDsTab(),
      compositionIDsTab()
    )
  )
}

# Function for the main panel content
mainPanelContent <- function(con) {
  mainPanel(
    # Control Storage Type Radio Buttons
    radioButtons("InputControlPanelType", "Control Storage Type", choices=get_control_types(), selected = "dbs_collection", inline=TRUE),
    
    # Filter controls
    tags$h3("Filters"),
    fluidRow(
      column(width = 2, selectizeInput("InputControlSearchBatch", width = '100%', label = "Batch", choices = c("", tbl(con, "study") %>% pull(short_code)), selected="")),
      column(width = 2, selectizeInput("InputControlSearchStrain", width = '100%', label = "Strain", choices = c("", tbl(con, "strain") %>% pull(name)), selected="")),
      column(width = 2, selectizeInput("InputControlSearchDensity", width = '100%', label = "Density", choices = c("", tbl(con, "malaria_blood_control") %>% pull(density) %>% unique(.)), selected="")),
      column(width = 2, selectizeInput("InputControlSearchPercentage", width = '100%', label = "Percentage", choices = c("", tbl(con, "composition_strain") %>% pull(percentage) %>% unique(.)), selected=""))
    ),
    fluidRow(
      column(width = 2, dateRangeInput("InputControlSearchDateRange", label = "Dates", start = NA, end = NA)),
      column(width = 2, selectInput("InputControlLocationRoot", label = "Freezer", choices = c("", tbl(con, "location") %>% pull(location_root) %>% unique(.)), selected="")),
      column(width = 2, selectInput("InputControlLocationLevelI", label = "Shelf Name", choices = c())),
      column(width = 2, selectInput("InputControlLocationLevelII", label = "Basket Name", choices = c()))
    ),
    
    # Main Table
    reactableOutput("OutputControlTable"),
    downloadButton("DownloadControlData", "Download")
    # ... (other UI elements)
  )
}

# Main UI function
UIControlsReference <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  
  ui <- sidebarLayout(
    sidebarWithTabs(),
    mainPanelContent(con)
  )
  
  DBI::dbDisconnect(con)
  
  return(ui)
}
