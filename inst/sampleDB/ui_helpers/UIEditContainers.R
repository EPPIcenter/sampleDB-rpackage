library(DBI)
library(RSQLite)
library(shinyjs)

UIMoveContainerOfSamples <- function(){

  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      width = 4,
      #shouldnt need sample type but bc it is unknown at this point if container names are unique, sample type specifies the db table to use to find the container name
      tags$h4("Modify Containers"),
      radioButtons("ContainerSampleType","1. Sample Storage Type", choices = global_sample_names_ids_list, inline = TRUE),
      selectInput("ContainerManifestID", label = "2. Select Container", choices = c()),
      radioButtons("ContainerAction","2. Container Action", c("Move" = "move", "Rename" = "rename", "Delete" = "delete"), inline = T),
      hr(),
      shinyjs::hidden(selectInput("ContainerLocationRoot", label = "Move Location", choices = c())),        
      shinyjs::hidden(selectInput("ContainerLocationLevelI", label = "Shelf Name", choices = c())),
      shinyjs::hidden(selectInput("ContainerLocationLevelII", label = "Basket Name", choices = c())),

      textInput("ContainerManifestNewID", label = "Human Readable Name", placeholder = "PRISM-2022-001"),
      uiOutput("ContainerManifestIDCheck"),
      hr(),
      uiOutput("ContainerAction"),
      hr(),
      verbatimTextOutput("ContainerOutputConsole")
    ),
    mainPanel()
  )

  DBI::dbDisconnect(con)

  return(ui)
}
