library(DBI)
library(RSQLite)
library(shinyjs)

UIMoveContainerOfSamples <- function(){

  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 4,
      
      # Section Title
      tags$h4("Modify Containers (Samples and Controls)"),
      
      # Radio button to select between sample and control container types
      radioButtons("ContainerType", "1. Type", choices = c("Samples" = "samples", "Controls" = "controls"), inline = TRUE),
      
      # Conditional panels based on ContainerType selection
      conditionalPanel(
        condition = "input.ContainerType == 'samples'",
        # For samples: Sample storage type (micronix, cryovial, etc.)
        radioButtons("ContainerSampleType", "2. Sample Storage Type", choices = get_sample_types(), inline = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.ContainerType == 'controls'",
        # For controls: Control storage type (e.g., whole blood, dbs sheet)
        radioButtons("ContainerControlType", "2. Control Storage Type", choices = get_all_control_types(), inline = TRUE)
      ),
      
      # SelectizeInput for containers
      selectizeInput("ContainerManifestID", label = "3. Select Container", choices = c()),
      
      # Radio buttons for container actions (Move, Rename, Delete)
      radioButtons("ContainerAction", "4. Container Action", c("Move" = "move", "Rename" = "rename", "Delete" = "delete"), inline = TRUE),
      
      hr(),
      
      # Location Inputs (Only show when moving the container)
      shinyjs::hidden(selectInput("ContainerLocationRoot", label = "Move Location", choices = c())),
      shinyjs::hidden(selectInput("ContainerLocationLevelI", label = "Shelf Name", choices = c())),
      shinyjs::hidden(selectInput("ContainerLocationLevelII", label = "Basket Name", choices = c())),
      
      # Text input for renaming container
      textInput("ContainerManifestNewID", label = "Human Readable Name", placeholder = "PRISM-2022-001"),
      
      # UI output for checking if the container ID is valid
      uiOutput("ContainerManifestIDCheck"),
      
      hr(),
      
      # Container actions (Move/Rename/Delete)
      uiOutput("ContainerAction"),
      
      hr(),
      
      # Console for displaying output
      verbatimTextOutput("ContainerOutputConsole")
    ),
    
    mainPanel()
  )
  
  return(ui)
}
