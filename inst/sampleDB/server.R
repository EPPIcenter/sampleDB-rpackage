library(dplyr)
library(sampleDB)
library(shinyFeedback)
library(shiny)
library(readr)
library(markdown)
library(lubridate)
library(emojifont)
# library(shinyjs)
library(DT)
library(purrr)
for(helper in list.files(path = "helpers", full.names = T, recursive = T)){source(helper, local = TRUE)}

function(input, output, session) {
  
    # SET PATH TO SQLITE DATABASE - WOULD PREFER DATABASE TO BE AT Sys.getenv("SAMPLEDB_DATABASE")
    database <- Sys.getenv("SDB_PATH")

    # reactiveValues object for storing current data set.
    vals <- reactiveValues(data = NULL)
    
    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal <- function(failed = FALSE){
      modalDialog(
        textInput("dataset", "It looks like SampleDB has not yet been setup on your computer",
                  placeholder = 'password'
        ),
        span('In order to setup SampleDB on this computer please provide the server admin password'),
        if (failed)
          div(tags$b("Invalid name of data object", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      )
    }
    
    showModal(dataModal())
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
      print(input$dataset)
      # Check that data object exists and is data frame.
      if (!is.null(input$dataset) && nzchar(input$dataset) &&
          exists(input$dataset) && is.data.frame(get(input$dataset))) {
        vals$data <- get(input$dataset)
        removeModal()
      } else {
        showModal(dataModal(failed = TRUE))
      }
    })

    
    # --------- Upload Samples -------------

    # Upload Micronix Samples
    MatrixUpload(session, output, input, database, ref.clear_action = "ClearMicronixUploadForm")
    
    # Upload Cryo Samples
    # CryoUpload(session, output, input, database, ref.clear_action = "ClearCryoUploadForm")

    # Upload RDT Samples
    # RDTUpload(session, output, input, database, ref.clear_action = "ClearRDTUploadForm")

    # Upload Paper Samples
    # PaperUpload(session, output, input, database, ref.clear_action = "ClearPaperUploadForm")
    
    # -------- Search Samples -------------
    
    SearchWetlabSamples(session, input, database, output)
    
    # -------- Move Samples -------------

    MoveWetlabSamples(session, input, database, output)

    # -------- Move Container --------
    
    MoveWetlabContainers(session, input, database, output)
    
    # -------- Archive and Delete Samples --------
    
    # does not work at the moment
    # SearchWetlabSamples(session, input, database, output, DelArch = TRUE)

    # -------- Delete Empty Container --------
    
    DeleteEmptyWetlabContainers(session, input, database, output)
    
    # -------- Update References ---------------
    
    # Update Freezers
    UpdateLabFreezers(session, input, output, database)
    
    # Update Specimen Types
    UpdateSpecimenTypes(session, input, output, database)
    
    # Update EPPIcenter Lab Studies
    UpdateLabStudies(session, input, output, database)

    # --------------- About ------------
  
    url <- a("here", href="https://github.com/EPPIcenter/sampleDB-rpackage/issues/")
    output$report_issues <- renderUI({
      tagList(HTML("Please report issues"), url)
    })
    
    url <- a("here", href="https://github.com/EPPIcenter/sampleDB-rpackage/")
    output$source_code <- renderUI({
      tagList("Source code can be found ", url)
    })
}
