library(dplyr)
library(sampleDB)
library(shinyFeedback)
library(shiny)
library(readr)
library(markdown)
library(lubridate)
library(emojifont)
library(shinyjs)
library(DT)
library(purrr)
for(helper in list.files(path = "helpers", full.names = T, recursive = T)){source(helper, local = TRUE)}

function(input, output, session) {
  
    # SET PATH TO SQLITE DATABASE - WOULD PREFER DATABASE TO BE AT Sys.getenv("SAMPLEDB_DATABASE")
    database <- "/databases/new.sampleDB.db"

    # --------- Upload Samples -------------

    # Upload Micronix Samples
    MatrixUpload(session, output, input, database, ref.clear_action = "ClearMicronixUploadForm")
    
    # Upload Cryo Samples
    CryoUpload(session, output, input, database, ref.clear_action = "ClearCryoUploadForm")

    # Upload RDT Samples
    RDTUpload(session, output, input, database, ref.clear_action = "ClearRDTUploadForm")

    # Upload Paper Samples
    PaperUpload(session, output, input, database, ref.clear_action = "ClearPaperUploadForm")
    
    # -------- Search Samples -------------

    # SERVER-SIDE DROPDOWN -- SAVES LOADING TIME
    updateSelectizeInput(session, 'SearchBySubjectUID', choices = c("", sampleDB::CheckTable(database = database, "study_subject")$subject %>% unique()), server = TRUE)
    
    SearchWetlabSamples(session, input, database, output,
                        inputs = list(SearchByLocation = "SearchByLocation",
                                      SearchByLevelI = "SearchByLevelI",
                                      SearchByLevelII = "SearchByLevelII",
                                      dateRange = "dateRange",
                                      SearchByExhausted = "SearchByExhausted",
                                      SearchBySpecimenType = "SearchBySpecimenType",
                                      SearchByStudy = "SearchByStudy",
                                      SearchBySampleType = "SearchBySampleType",
                                      SubjectUIDSearchType = "SubjectUIDSearchType",
                                      SearchBySubjectUID = "SearchBySubjectUID"),
                        outputs = list(SearchResultsTable = "SearchResultsTable",
                                       downloadData = "downloadData"))
    
    # -------- Move Samples -------------

    MoveWetlabSamples(session, input, database, output)

    # -------- Move Container --------
    
    observe({
      if(input$MoveContainerSampleType == "Micronix"){
        updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("matrix_plate")$plate_name))
      }
      else if(input$MoveContainerSampleType == "Cryovile"){
        updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("box")$box_name))
      }
      else if(input$MoveContainerSampleType == "RDT"){
        updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
      }else{
        updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
      }
    })
    
    observe({
      if(input$MoveContainerLocation != ""){
        tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input$MoveContainerLocation)
        updateSelectizeInput(session, "MoveContainerLocationLevelI", label = NULL, choices = c(tmp_table.location$level_I))
        updateSelectizeInput(session, "MoveContainerLocationLevelII", label = NULL, choices = c(tmp_table.location$level_II))
      }else{
        updateSelectizeInput(session, "MoveContainerLocationLevelI", label = NULL, choices = c(""))
        updateSelectizeInput(session, "MoveContainerLocationLevelII", label = NULL, choices = c(""))
      }
    })
    
    observeEvent(
      input$MoveContainerAction,({
        container.type <- input$MoveContainerSampleType
        container.name <- input$MoveContainerName
        freezer.name <- input$MoveContainerLocation
        freezer.levelI <- input$MoveContainerLocationLevelI
        freezer.levelII <- input$MoveContainerLocationLevelII
        sampleDB::MoveContainers(type = container.type,
                                 container_name = container.name,
                                 location = list(freezer.name,
                                                 freezer.levelI,
                                                 freezer.levelII))
    }))
    
    # -------- Archive Samples --------
    # -------- Delete Samples --------
    
    # SERVER-SIDE DROPDOWN -- SAVES LOADING TIME
    updateSelectizeInput(session, 'DelArchSearchBySubjectUID', choices = c("", sampleDB::CheckTable(database = database, "study_subject")$subject %>% unique()), server = TRUE)
    
    SearchWetlabSamples(session, input, database, output,
                        inputs = list(SearchByLocation = "DelArchSearchByLocation",
                                      SearchByLevelI = "DelArchSearchByLevelI",
                                      SearchByLevelII = "DelArchSearchByLevelII",
                                      dateRange = "DelArchdateRange",
                                      SearchByExhausted = "DelArchSearchByExhausted",
                                      SearchBySpecimenType = "DelArchSearchBySpecimenType",
                                      SearchByStudy = "DelArchSearchByStudy",
                                      SearchBySampleType = "DelArchSearchBySampleType",
                                      SubjectUIDSearchType = "DelArchSubjectUIDSearchType",
                                      SearchBySubjectUID = "DelArchSearchBySubjectUID"),
                        outputs = list(SearchResultsTable = "DelArchSearchResultsTable",
                                       downloadData = "DelArchdownloadData"),
                        DelArch = TRUE)

    # -------- Delete Empty Container --------
    
    observe({
      if(input$DeleteContainerSampleType == "Micronix"){
        updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("matrix_plate")$plate_name))
      }
      else if(input$DeleteContainerSampleType == "Cryovile"){
        updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("box")$box_name))
      }
      else if(input$DeleteContainerSampleType == "RDT"){
        updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
      }else{
        updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
      }
    })
    
    # DeletePlateChecks(input, database, output)
    
    observeEvent(
      input$DeleteContainerAction,
      ({

        # # SET REQUIREMENT FOR DELETEING PLATE
        # DeleteEmptyPlateRequirement(input, database)

        # DELETE PLATE
        type.container <- input$DeleteContainerSampleType
        name.container <- input$DelteContainerName
        print(type.container)
        print(name.container)
        # DeleteEmptyPlates(type.container, name.container)
        
        # RESET PLATE NAMES DROPDOWN
        # DeleteEmptyPlateReset(session, database)
      }))

    
    
    # -------- Update References ---------------
    
    # Update Freezers
    UpdateLabFreezers(session, input, output, database)
    
    # Update Specimen Types
    UpdateSpecimenTypes(session, input, output, database)
    
    # Update EPPIlab Studies
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