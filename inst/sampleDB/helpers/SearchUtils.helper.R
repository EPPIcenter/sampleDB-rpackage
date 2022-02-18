

SearchWetlabSamples <- function(session, input, database, output, inputs, outputs, DelArch = FALSE){
  
  # SEARCH CHECKS... CHECK THAT SEARCH FILES ARE NOT MALFORMED
  # .SearchChecks(input, database, output)
  
  observe({
    if(input[[inputs$SearchByLocation]] != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input[[inputs$SearchByLocation]])
      updateSelectizeInput(session, inputs$SearchByLevelI, label = NULL, choices = c(tmp_table.location$level_I))
      updateSelectizeInput(session, inputs$SearchByLevelII, label = NULL, choices = c(tmp_table.location$level_II))
    }else{
      updateSelectizeInput(session, inputs$SearchByLevelI, label = NULL, choices = c(""))
      updateSelectizeInput(session, inputs$SearchByLevelII, label = NULL, choices = c(""))
    }
  })
  
  # ACTIVELY USE UI FILTERS TO RENDER A TABLE WITH SEARCH RESULTS
  observe({
    
    if(!is.na(input[[inputs$dateRange]][1]) & !is.na(input[[inputs$dateRange]][2])){
      eval.search.date <- list(date.from = input[[inputs$dateRange]][1], date.to = input[[inputs$dateRange]][2])
    }else{
      eval.search.date <- ""
    }
    
    filters <- list(
      search.date = eval.search.date,
      search.exhausted = input[[inputs$SearchByExhausted]],
      search.location = list(location_name = input[[inputs$SearchByLocation]], level_I = input[[inputs$SearchByLevelI]], level_II = input[[inputs$SearchByLevelII]]),
      search.specimen_type = input[[inputs$SearchBySpecimenType]],
      search.study = input[[inputs$SearchByStudy]],
      search.type = input[[inputs$SearchBySampleType]])
    
    # RETRIEVE SEARCH RESULTS
    if(input[[inputs$SubjectUIDSearchType]] == "individual"){
      filters$name.study_subject <- input[[inputs$SearchBySubjectUID]]
      list.search_results <- sampleDB::SearchSamples(discard(filters, function(x) "" %in% x), study_subject.file = F)
      search_results <- list.search_results$tbl.usr_results
      storage_container_ids <- list.search_results$storage_container_ids
    }else{
      filters$name.study_subject <- input[[inputs$SearchBySubjectUIDFile]]$datapath
      list.search_results <- sampleDB::SearchSamples(discard(filters, function(x) "" %in% x), study_subject.file = T)
      search_results <- list.search_results$tbl.usr_results
      storage_container_ids <- list.search_results$storage_container_ids
    }
    
    # PRINT SEARCH RESULTS
    output[[outputs$SearchResultsTable]] <- DT::renderDataTable({
      if(!is.null(search_results)){
        search_results
      }else{
        tibble(a = c(1)) %>% filter(a == 2)
      }
    },
    options = list(
      searching = T,
      server = F,
      paging = T,
      pageLength = 20,
      lengthMenu = c(10, 20, 50, 100),
      language = list(zeroRecords = "There are no EPPIcenter Wetlab Samples that match this search.")))
    
    # DOWNLOAD SEARCH RESULTS
    output[[outputs$downloadData]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(search_results, con)
      }
    )
    
    if(DelArch == TRUE){
      updateTextInput(session = session, "RenameStudyLeadPerson", value = storage_container_ids) 
    }
    
  })
  
  if(DelArch == TRUE){
    observe({
      selected <- input$"DelArchSearchResultsTable_rows_selected"
      if(length(selected) > 0){
        a <- input$"RenameStudyLeadPerson"
        sc_ids <- strsplit(a, ",")[[1]]
        output$ShowSelectedSamples <- renderPrint({paste(length(sc_ids[selected]),"samples selected")})
      }
      observeEvent(
        input$DeleteAction,({
          ArchiveAndDeleteSamples("delete", storage_container_ids = sc_ids[selected])
        }))
      observeEvent(
        input$ArchiveAction,({
          ArchiveAndDeleteSamples("archive", storage_container_ids = sc_ids[selected])
        }))
      observeEvent(
        input$UnArchiveAction,({
          ArchiveAndDeleteSamples("unarchive", storage_container_ids = sc_ids[selected])
        }))
      
      # RESET UI VALUE
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "")
      })
  }
  
  # CLEAR FILES
  # .SearchReset(input)
}

.SearchReset <- function(input){
  observeEvent(input$ClearSearchBarcodes, ({reset("SearchByBarcode")}))
  observeEvent(input$ClearSearchUIDFile, ({reset("SearchBySubjectUIDFile")})) 
}
.SearchChecks <- function(input, database, output){
  #CHECK THAT UID FILE IS PROPERLY FORMED
  CheckSubjectBarcodeFileColnames <- reactive({helper.CheckSubjectBarcodeFileColnames(input, database)})
  output$WarnSubjectBarcodeFileColnames <- renderText(CheckSubjectBarcodeFileColnames())
  
  #CHECK IF UID FILE IS PROPERLY FORMED - FILEINPUT
  CheckSubjectUIDFileColnames2 <- reactive({CheckSubjectUIDFileColnames2(input, database)})
  output$WarningSubjectUIDFileColnames2 <- renderText(CheckSubjectUIDFileColnames2())
  
  #CHECK THAT UID FILE IS PROPERLY FORMED
  CheckSubjectUIDFileColnames <- reactive({helper.CheckSubjectUIDFileColnames(input, database)})
  output$WarnSubjectUIDFileColnames <- renderText(CheckSubjectUIDFileColnames())
  
  #CHECK IF UID FILE IS PROPERLY FORMED - FILEINPUT
  CheckSubjectBarcodeFileColnames2 <- reactive({helper.CheckSubjectUIDFileColnames2(input, database)})
  output$WarnSubjectBarcodeFileColnames2 <- renderText(CheckSubjectBarcodeFileColnames2())
}

###############################################################################
#need to verify checks in this file
helper.CheckSubjectBarcodeFileColnames <- function(input, database){
  if(!is.null(input$SearchByBarcode$datapath)){
    
    names.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% colnames()
    ncols.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% ncol()
    
    toggle <- ncols.barcode_file == 1 & names.barcode_file == "barcode"
    
    out <- validate(need(toggle, "Failed: Barcode File is Malformed"))
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckSubjectBarcodeFileColnames2 <- function(input, database){
  if(!is.null(input$SearchByBarcode$datapath)){
    
    names.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% colnames()
    ncols.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% ncol()
    
    toggle <- !(ncols.barcode_file == 1 & names.barcode_file == "barcode")
    
    out <- shinyFeedback::feedbackWarning("SearchByBarcode", toggle, "Failed: Barcode File is Malformed")
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckSubjectUIDFileColnames <- function(input, database){
  
  if(!is.null(input$SearchBySubjectUIDFile$datapath)){
    
    names.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% colnames()
    ncols.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% ncol()
    
    toggle <- ncols.subject_uid_file == 1 & names.subject_uid_file == "subject_uid"
    out <- validate(need(toggle, "Failed: Subject UID File is Malformed"))
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckSubjectUIDFileColnames2 <- function(input, database){
  if(!is.null(input$SearchBySubjectUIDFile$datapath)){
    
    names.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% colnames()
    ncols.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% ncol()
    
    toggle <- !(ncols.subject_uid_file == 1 & names.subject_uid_file == "subject_uid")
    
    out <- shinyFeedback::feedbackWarning("SearchBySubjectUIDFile", toggle, "Failed: Subject UID File is Malformed")
  }else{
    out <- NULL
  }
  return(out)
}


# STUFF BELOW SHOULD REALLY BE PUT ELSEWHERE
helper.SubsetPlateNames <- function(input, database){
   study_ref_id <- filter(sampleDB::CheckTable(database = database, "study"), short_code %in% input$SearchByStudy)$id
   study_subject_ref_id <- filter(sampleDB::CheckTable(database = database, "study_subject"), study_id %in% study_ref_id)$id
   specimen_ref_id <- filter(sampleDB::CheckTable(database = database, "specimen"), study_subject_id %in% study_subject_ref_id)$id
   storage_container_id <- filter(sampleDB::CheckTable(database = database, "storage_container"), specimen_id %in% specimen_ref_id)$id
   matrix_tube_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), id %in% storage_container_id)$id
   
   plate_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), id %in% matrix_tube_ids)$plate_id %>% unique()
   plate_names <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), id %in% plate_ids)$uid
   return(plate_names)
 }


