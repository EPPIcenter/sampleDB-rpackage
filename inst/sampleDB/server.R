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
