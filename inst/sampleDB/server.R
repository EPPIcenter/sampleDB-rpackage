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
    system("if test -f \"/bin/sampleDB_backup_generator.sh\"; then bash /bin/sampleDB_backup_generator.sh; fi")
  
    # SET PATH TO SQLITE DATABASE - WOULD PREFER DATABASE TO BE AT Sys.getenv("SAMPLEDB_DATABASE")
    database <- Sys.getenv("SDB_PATH")

    
    ######
    # reactiveValues object for storing current data set.
    vals <- reactiveValues(data = NULL)
    
    # Show the UI for a modal dialog with admin password input if a sampledb db does not exist on the server.
    # If 'failed' is TRUE, then display a message that the previous value was invalid.
    dataModal <- function(failed = FALSE){
      modalDialog(
        textInput("password", "It looks like SampleDB has not yet been setup on your computer",
                  placeholder = 'password'
        ),
        span('In order to setup SampleDB on this computer please provide the server admin password'),
        if (failed)
          div(tags$b("Password is incorrect", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      )
    }
    
    # if(!file.exists("/databases/sampledb/v0.0.2/sampledb_database.sqlite")){
    #   showModal(dataModal()) 
    # }
    
    # When OK button is pressed, attempt to use sudo. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
      print(input$password)
      #a will equal NULL if the password is correct, otherwise it will be not null
      password_verification <- system("sudo -kS sudo -l", input = input$password, intern = TRUE) %>% attributes() %>% pluck("status")
      print(password_verification)
      # Check that data object exists and is data frame.
      if(is.null(password_verification)){
        # vals$data <- get(input$password)
        removeModal()
      } else {
        showModal(dataModal(failed = TRUE))
      }
    })
    
    # Display information about selected data
    output$SDBSetup <- renderPrint({
      if(is.null(vals$data)){
        print("Password has not been provided" )
      }
      else{
        #write sampledb file to
        path <- "var/lib/sampleDB/"
        sqlite_file <- system.file("extdata", "sampledb_database.sqlite", package = "sampleDB")
        system(paste("sudo -kS cp", sqlite_file, path), input = "Gr33nhouse")
        # Sys.chmod(paste0(path, "/sampledb_database.sqlite"), mode = "0777") # may need to reformat cmd to include sudo
        
        #add variable to .Renviron-site
        #check that "/etc/R/Renviron.site" exists
        system(paste("sudo -kS bash -c \"echo SDB_PATH='\"'\"/databases/sampledb/v0.0.2/sampledb_database.sqlite\"'\"' >> /etc/R/Renviron.site\""), input = "Gr33nhouse")
        # system("sudo -kS bash -c \"echo SDB_PATH='\"/databases/sampledb/v0.0.2/sampledb_database.sqlite\"' >> /etc/R/Renviron.site\"", input = "Gr33nhouse")
        
      }
    })
    #######

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
