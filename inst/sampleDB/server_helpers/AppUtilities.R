library(yaml)

# Utility Functions for Main Shiny Functions (e.g. MicronixUpload, MoveWetlabSamples, etc.)

# Get UI Elements
GetUIUploadElements <- function(sample_type, msg = NULL){
  
  if(sample_type == "micronix"){
    ui.input <- list(UploadPlateID = "UploadMicronixPlateID",
                     UploadDataSet = "UploadMicronixDataSet",
                     MicronixFileType = "MicronixFileType",
                     ClearForm = "ClearMicronixUploadForm",
                     UploadFreezerName = "UploadMicronixLocation",
                     UploadFreezerNameLevelI = "UploadLocationMicronixLevelI",
                     UploadFreezerNameLevelII = "UploadLocationMicronixLevelII",
                     UploadReturnMessage1 = "UploadMicronixReturnMessage1",
                     UploadReturnMessage2 = "UploadMicronixReturnMessage2")
    ui.output = list(WarningUploadSampleID = "WarningMicronixUploadSampleID",
                     WarningLogisticalColnames = "WarningMicronixUploadLogisticalColnames",
                     WarningMetadataColnames = "WarningMicronixUploadMetadataColnames",
                     WarningUploadSpecimenTypes = "WarningUploadMicronixSpecimenTypes",
                     WarningUploadInvalidData = "WarningUploadInvalidData",
                     WarningUploadDateFormat = "WarningMicronixUploadDateFormat",
                     WarningUploadStudyShortCodes = "WarningUploadMicronixStudyShortCodes",
                     WarningSpecimenExists = "WarningMicronixSpecimenExists",
                     WarningUploadContainerName = "WarningMicronixUploadContainerName",
                     WarningUploadBarcodeRepeats = "WarningMicronixUploadBarcodeRepeats")
  }

  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUISearchElements <- function(){
  
  ui.input <- list(SearchByLocation = "SearchByLocation",
                   SearchByLevelI = "SearchByLevelI",
                   SearchByLevelII = "SearchByLevelII",
                   dateRange = "dateRange",
                   SearchByStatus = "SearchByStatus",
                   SearchByState = "SearchByState",
                   SearchBySpecimenType = "SearchBySpecimenType",
                   SearchByStudy = "SearchByStudy",
                   SearchBySampleType = "SearchBySampleType",
                   SubjectUIDSearchType = "SubjectUIDSearchType",
                   SearchBySubjectUID = "SearchBySubjectUID",
                   SearchBySubjectUIDFile = "SearchBySubjectUIDFile",
                   SearchByBarcode = "SearchByBarcode",
                   SearchByCryovialLabels = "SearchByCryovialLabels",
                   SearchByRDTLabels = "SearchByRDTLabels",
                   SearchByPaperLabels = "SearchByPaperLabels",
                   SearchByPlate = "SearchByPlate",
                   SearchByBox = "SearchByBox",
                   SearchByRDTBag = "SearchByRDTBag",
                   SearchByPaperBag = "SearchByPaperBag",
                   SearchBySingleBarcode = "SearchBySingleBarcode",
                   SearchByBarcodeType = "SearchByBarcodeType")
  ui.output <- list(SearchResultsTable = "SearchResultsTable",
                    downloadData = "downloadData")
  
  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIMoveElements <- function(sample_type, msg = NULL){
  
  if(sample_type == "micronix"){
    ui.input <-  list(MicronixFileType = "MoveFileType",
                      MoveDataSet = "MoveDataSet",
                      CreateEmptyMicronixPlateLocation = "CreateEmptyMicronixPlateLocation",
                      CreateEmptyMicronixPlateLevelI = "CreateEmptyMicronixPlateLevelI",
                      CreateEmptyMicronixPlateLevelII = "CreateEmptyMicronixPlateLevelII")
    ui.output <- list(WarningLogisticalColnames = "WarningMoveLogisticalColnames",
                      WarningMoveBarcodesExist = "WarningMoveBarcodesExist")
  }
  
  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIFreezerElements <- function(msg = NULL){
  
  ui.input <- list(AddFreezerName = "AddFreezerName",
                   AddFreezerType = "AddFreezerType",
                   AddFreezerLevel_I = "AddFreezerLevel_I",
                   AddFreezerLevel_II = "AddFreezerLevel_II",
                   AddFreezerAction = "AddFreezerAction",
                   RenameFreezerName1 = "RenameFreezerName1",
                   RenameFreezerLevelI1 = "RenameFreezerLevelI1",
                   RenameFreezerLevelII1 = "RenameFreezerLevelII1",
                   RenameFreezerName2 = "RenameFreezerName2",
                   RenameFreezerType2 = "RenameFreezerType2",
                   RenameFreezerLevelI2 = "RenameFreezerLevelI2",
                   RenameFreezerLevelII2 = "RenameFreezerLevelII2",
                   RenameFreezerAction = "RenameFreezerAction",
                   DeleteFreezerName = "DeleteFreezerName",
                   DeleteFreezerLevelI = "DeleteFreezerLevelI",
                   DeleteFreezerLevelII = "DeleteFreezerLevelII",
                   DeleteFreezerAction = "DeleteFreezerAction")
  ui.output = list(WarningFreezerNameAddUnique = "WarningFreezerNameAddUnique",
                   WarningFreezerNameChangeUnique = "WarningFreezerNameChangeUnique",
                   WarningFreezerDeletion = "WarningFreezerDeletion",
                   FreezerReturnMessage = "FreezerReturnMessage")
  
  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUISpecimenTypeElements <- function(msg = NULL){
  
  ui.input <- list(AddSpecimenType = "AddSpecimenType",
                   AddSpecimenTypeAction = "AddSpecimenTypeAction",
                   RenameSpecimenType1 = "RenameSpecimenType1",
                   RenameSpecimenType2 = "RenameSpecimenType2",
                   RenameSpecimenTypeAction = "RenameSpecimenTypeAction",
                   DeleteSpecimenType = "DeleteSpecimenType",
                   DeleteSpecimenTypeAction = "DeleteSpecimenTypeAction")
  ui.output = list(WaringAddSpecimenTypeUnique = "WaringAddSpecimenTypeUnique",
                   WarningChangeSpecimenTypeUnique = "WarningChangeSpecimenTypeUnique",
                   WarningSpecimenTypeDeletion = "WarningSpecimenTypeDeletion",
                   SpecimenReturnMessage = "SpecimenReturnMessage")
  
  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIStudiesElements <- function(msg = NULL){
  
  ui.input <-  list(AddStudyTitle = "AddStudyTitle",
                    AddStudyDescription = "AddStudyDescription",
                    AddStudyLeadPerson = "AddStudyLeadPerson",
                    AddStudyShortCode = "AddStudyShortCode",
                    AddStudyIsLongitudinal = "AddStudyIsLongitudinal",
                    AddStudyAction = "AddStudyAction",
                    ChangeStudyShortCode = "ChangeStudyShortCode",
                    RenameStudyTitle = "RenameStudyTitle",
                    RenameStudyDescription = "RenameStudyDescription",
                    RenameStudyLeadPerson = "RenameStudyLeadPerson",
                    RenameStudyShortCode = "RenameStudyShortCode",
                    RenameStudyIsLongitudinal = "RenameStudyIsLongitudinal",
                    RenameStudyAction = "RenameStudyAction",
                    DeleteStudyShortCode = "DeleteStudyShortCode",
                    DeleteStudyAction = "DeleteStudyAction")
  ui.output <- list(WarningStudyAddTitleUnique = "WarningStudyAddTitleUnique",
                    WarningStudyAddShortCodeUnique = "WarningStudyAddShortCodeUnique",
                    WarningStudyChangeTitleUnique = "WarningStudyChangeTitleUnique",
                    WarningStudyChangeShortCodeUnique = "WarningStudyChangeShortCodeUnique",
                    WarnStudyDeletion = "WarnStudyDeletion",
                    StudyReturnMessage = "StudyReturnMessage")
  
  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIDelArchElements <- function(){
  
  ui.input <- list(SearchByLocation = "DelArchSearchByLocation",
                   SearchByLevelI = "DelArchSearchByLevelI",
                   SearchByLevelII = "DelArchSearchByLevelII",
                   dateRange = "DelArchdateRange",
                   SearchByStatus = "DelArchSearchByStatus",
                   SearchByState = "DelArchSearchByState",
                   SearchBySpecimenType = "DelArchSearchBySpecimenType",
                   SearchByStudy = "DelArchSearchByStudy",
                   SearchBySampleType = "DelArchSearchBySampleType",
                   SubjectUIDSearchType = "DelArchSubjectUIDSearchType",
                   SearchBySubjectUID = "DelArchSearchBySubjectUID",
                   SearchBySubjectUIDFile = "DelArchSearchBySubjectUIDFile",
                   SearchByBarcode = "DelArchSearchByBarcode",
                   SearchByCryovialLabels = "DelArchSearchByCryovialLabels",
                   SearchByRDTLabels = "DelArchSearchByRDTLabels",
                   SearchByPaperLabels = "DelArchSearchByPaperLabels",
                   SearchByPlate = "DelArchSearchByPlate",
                   SearchByBox = "DelArchSearchByBox",
                   SearchByRDTBag = "DelArchSearchByRDTBag",
                   SearchByPaperBag = "DelArchSearchByPaperBag",
                   ArchiveAction = "ArchiveAction",
                   DeleteAction = "DeleteAction",
                   DelArchComment = "DelArchComment",
                   DelArchStatus = "DelArchStatus",
                   DelArchVerification = "DelArchVerification",
                   SearchBySingleBarcode = "DelArchSearchBySingleBarcode",
                   SearchByBarcodeType = "DelArchSearchByBarcodeType")
  ui.output <- list(SearchResultsTable = "DelArchSearchResultsTable",
                    SelectedRowsTable = "DelArchSearchResultsTable",
                    DelArchMessage = "DelArchMessage")
  
  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

SearchFunction <- function(input, output, ui_elements){
  
  #set default list.search_results
  list.search_results <- NULL
  
  # get user ui input
  if(!is.na(input[[ui_elements$ui.input$dateRange]][1]) & !is.na(input[[ui_elements$ui.input$dateRange]][2])){
    eval.search.date <- list(date.from = input[[ui_elements$ui.input$dateRange]][1], date.to = input[[ui_elements$ui.input$dateRange]][2])
  }else{
    eval.search.date <- ""
  }
  
  search.type <- input[[ui_elements$ui.input$SearchBySampleType]]
  barcode.search_method <- input[[ui_elements$ui.input$SearchByBarcodeType]]
  if(barcode.search_method == "multiple_barcodes"){
    search.label <- list(input[[ui_elements$ui.input$SearchByBarcode]]$datapath, 
                         input[[ui_elements$ui.input$SearchByCryovialLabels]]$datapath,
                         input[[ui_elements$ui.input$SearchByRDTLabels]]$datapath,
                         input[[ui_elements$ui.input$SearchByPaperLabels]]$datapath) %>% 
      discard(., function(x) is.null(x) | "" %in% x) %>%
      map(., function(x){if(length(x > 0)){read.csv(x)$Barcode}}) %>%
      unlist() 
  }else{
    individual_barcode <- NULL
    if(ui_elements$ui.input$SearchBySingleBarcode != ""){
      individual_barcode <- input[[ui_elements$ui.input$SearchBySingleBarcode]] 
    }
    search.label <- list(individual_barcode)
  }
  
  # search.container <- list(micronix.container_name = input[[ui_elements$ui.input$SearchByPlate]],
  #                          cryovial.container_name = input[[ui_elements$ui.input$SearchByBox]],
  #                          rdt.container_name = input[[ui_elements$ui.input$SearchByRDTBag]],
  #                          paper.container_name = input[[ui_elements$ui.input$SearchByPaperBag]]) %>% 
  #   discard(., function(x) is.null(x) | "" %in% x)


  # TODO
  search.container <- input[[ui_elements$ui.input$SearchByPlate]]

  search.date <- eval.search.date
  search.status <- input[[ui_elements$ui.input$SearchByStatus]]
  search.state <- input[[ui_elements$ui.input$SearchByState]]
  search.location <- list(location_name = input[[ui_elements$ui.input$SearchByLocation]],
                          level_I = input[[ui_elements$ui.input$SearchByLevelI]],
                          level_II = input[[ui_elements$ui.input$SearchByLevelII]])
  search.specimen_type <- input[[ui_elements$ui.input$SearchBySpecimenType]]
  search.study <- input[[ui_elements$ui.input$SearchByStudy]]
  
  tryCatch(
    
    # search (if a single StudySubject is searched for)
    if(input[[ui_elements$ui.input$SubjectUIDSearchType]] == "individual"){
      search.study_subject <- input[[ui_elements$ui.input$SearchBySubjectUID]]
      list.search_results <- sampleDB::SearchSamples(sample_type = search.type, sample_label = search.label, container_name = search.container, study_subject = search.study_subject,
                                                     specimen_type = search.specimen_type, study = search.study, collection_dates = search.date, status = search.status,
                                                     state = search.state, freezer = search.location, return_sample_ids = T) %>% suppressWarnings()
    }else{
      
      # search (if a file of StudySubjects is searched for)
      search_multiple_file <- read.csv(input[[ui_elements$ui.input$SearchBySubjectUIDFile]]$datapath)

      # remove empty columns
      if (typeof(search_multiple_file) != "list") {
        empty_columns <- colSums(is.na(search_multiple_file) | search_multiple_file == "") == nrow(search_multiple_file)
        search_multiple_file <- search_multiple_file[, !empty_columns]
        search_multiple_file <- search_multiple_file[!apply(search_multiple_file, 1, function(row) all(row == "")), ] 
      }

      search.study_subject <- search_multiple_file$StudySubject
      search.study <- search_multiple_file$StudyCode
      search.specimen_type <- search_multiple_file$SpecimenType
      if (!is.null(search_multiple_file$CollectionDate)) {
        search.date <- list(date.from = search_multiple_file$CollectionDate, date.to = search_multiple_file$CollectionDate)
      } else {
        search.date <- NULL
      }


      list.search_results <- sampleDB::SearchSamples(sample_type = search.type, sample_label = search.label, container_name = search.container, study_subject = search.study_subject,
                                                     specimen_type = search.specimen_type, study = search.study, collection_dates = search.date, status = search.status,
                                                     state = search.state, freezer = search.location, return_sample_ids = T) %>% suppressWarnings()
      
    },
    error=function(e){}
  )

  return(list.search_results)
}

CheckPlates <- function(database, sample_type, input, output){
  
  #get ui elements
  ui_elements <- GetUIUploadElements(sample_type)
  message("Checking user provided plate names...")
  
  # check unique plate names
  # output[[ui_elements$ui.output$WarningUploadContainerName]] <- renderText({
  #   plate_name <- input[[ui_elements$ui.input$UploadPlateID]]
  #   out <- sampleDB:::.CheckUploadContainerNameDuplication(database = database,plate_name = plate_name, only_active = T)
  #   validate(need(out, "ERROR:\nPlate name is not unique"))
  # })
}

FreezerChangesChecks <- function(input, database, output, ui_elements){
  # warn if freezer name is redundant
  output[[ui_elements$ui.output$WarningFreezerNameAddUnique]] <- renderText({
    out <- sampleDB:::.CheckFreezerNameIsUnique(input, database, 
                                     freezer_address = list(freezer_name = input[[ui_elements$ui.input$AddFreezerName]],
                                                            freezer_levelI = input[[ui_elements$ui.input$AddFreezerLevel_I]],
                                                            freezer_levelII = input[[ui_elements$ui.input$AddFreezerLevel_II]]))
    validate(need(out, "ERROR:\nFreezer address must be unique"))
  })
  
  # warn if freezer name is redundant
  output[[ui_elements$ui.output$WarningFreezerNameChangeUnique]] <- renderText({
    out <- sampleDB:::.CheckFreezerNameIsUnique(input, database,
                              freezer_address = list(freezer_name = input[[ui_elements$ui.input$RenameFreezerName2]],
                                                        freezer_levelI = input[[ui_elements$ui.input$RenameFreezerLevelI2]],
                                                        freezer_levelII = input[[ui_elements$ui.input$RenameFreezerLevelII2]]))
    validate(need(out, "ERROR:\nFreezer address must be unique"))
  })
  
  #warn deletion of freezer in use
  output$WarningFreezerDeletion <- renderText({
    out <- sampleDB:::.CheckFreezerDeletion(input, database,
                          freezer_address = list(freezer_name = input[[ui_elements$ui.input$DeleteFreezerName]],
                                                    freezer_levelI = input[[ui_elements$ui.input$DeleteFreezerLevelI]],
                                                    freezer_levelII = input[[ui_elements$ui.input$DeleteFreezerLevelII]]))
    validate(need(out, "ERROR:\nFreezer is currently in use"))
  })
}

SpecimenTypeChangesChecks <- function(input, database, output, ui_elements){
  # warn if specimen type is redundant
  output[[ui_elements$ui.output$WaringAddSpecimenTypeUnique]] <- renderText({
    out <- sampleDB:::.CheckSpecimenTypeUnique(input, database, specimen_type = input[[ui_elements$ui.input$AddSpecimenType]])
    validate(need(out, "ERROR:\nSpecimen type is not unique"))
  })
  
  # warn if specimen type is redundant
  output[[ui_elements$ui.output$WarningChangeSpecimenTypeUnique]] <- renderText({
    out <- sampleDB:::.CheckSpecimenTypeUnique(input, database, specimen_type = input[[ui_elements$ui.input$RenameSpecimenType2]])
    validate(need(out, "ERROR:\nSpecimen type is not unique"))
  })
  
  #warn deletion of specimen type in use
  output[[ui_elements$ui.output$WarningSpecimenTypeDeletion]] <- renderText({
    out <- sampleDB:::.CheckSpecimenTypeDeletion(input, database , specimen_type = input[[ui_elements$ui.input$DeleteSpecimenType]])
    validate(need(out, "ERROR:\nSpecimen type is currently in use"))
  })
}

StudyChangesChecks <- function(input, database, output, ui_elements) {
  #warn against study title duplication
  output[[ui_elements$ui.output$WarningStudyAddTitleUnique]] <- renderText({
    out <- sampleDB:::.CheckStudyTitleIsUnique(study_title = input[[ui_elements$ui.input$AddStudyTitle]], input = input, database = database)
    validate(need(out, "ERROR:\nStudy title is not unique"))
  })
  
  #warn against study title duplication
  output[[ui_elements$ui.output$WarningStudyChangeTitleUnique]] <- renderText({
    out <- sampleDB:::.CheckStudyTitleIsUnique(study_title = input[[ui_elements$ui.input$RenameStudyTitle]], input = input, database = database)
    validate(need(out, "ERROR:\nStudy title is not unique"))
  })
  
  #warn against study code duplication
  output[[ui_elements$ui.output$WarningStudyAddShortCodeUnique]] <- renderText({
    out <- sampleDB:::.CheckStudyShortCodeIsUnique(study_short_code = input[[ui_elements$ui.input$AddStudyShortCode]], input = input, database = database)
    validate(need(out, "ERROR:\nStudy code is not unique"))
  })
  
  #warn against study code duplication
  output[[ui_elements$ui.output$WarningStudyChangeShortCodeUnique]] <- renderText({
    out <- sampleDB:::.CheckStudyShortCodeIsUnique(study_short_code = input[[ui_elements$ui.input$RenameStudyShortCode]], input = input, database = database)
    validate(need(out, "ERROR:\nStudy code is not unique"))
  })
  
  #warn against study code deletion
  output[[ui_elements$ui.output$WarnStudyDeletion]] <- renderText({
    out <- sampleDB:::.CheckStudyDeletion(study_ui = input[[ui_elements$ui.input$DeleteStudyShortCode]], input, database)
    validate(need(out, "ERROR:\nStudy is currently in use"))
  })
}

# General Operations
SmartFreezerDropdownFilter <- function(database, session, input = input, location_ui, levelI_ui, levelII_ui){
  
  observe({
    if(!is.null(input[[location_ui]]) && input[[location_ui]] != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input[[location_ui]])
      updateSelectInput(session, levelI_ui, label = NULL, choices = c(tmp_table.location$level_I) %>% sort())
      levelII_choices <- list(`working baskets` = grep("working", tmp_table.location$level_II, value = T) %>% sort(), 
                              `non-working baskets` = grep("working", tmp_table.location$level_II, value = T, invert = T) %>% sort())
      updateSelectInput(session, levelII_ui, label = NULL, choices = c("", levelII_choices))
    }else{
      updateSelectInput(session, levelI_ui, label = NULL, choices = c(""))
      updateSelectInput(session, levelII_ui, label = NULL, choices = c(""))
    }
  })
}

DataTableRenderOptions <- function(){
  out <- list(
    searching = T,
    server = F,
    paging = T,
    pageLength = 10,
    lengthMenu = c(10, 20, 50, 100),
    language = list(zeroRecords = "There are no EPPIcenter Wetlab Samples that match this search."),
    rowCallback = JS(
      rowCallback <- c(
        "function(row, data){",
        "  for(var i=0; i<data.length; i++){",
        "    if(data[i] === null){",
        "      $('td:eq('+i+')', row).html('NA')",
        "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
        "    }",
        "  }",
        "}"  
      )
    ))
  return(out)
}

#requirements
SetUploadRequirements <- function(input, database, sample_type){  
  #get ui elements
  ui_elements <- GetUIUploadElements(sample_type)
  
  out <- req(
    input[[ui_elements$ui.input$UploadDataSet]]$datapath,
    input[[ui_elements$ui.input$UploadPlateID]],
    input[[ui_elements$ui.input$UploadFreezerName]],
    input[[ui_elements$ui.input$UploadFreezerNameLevelI]],
    input[[ui_elements$ui.input$UploadFreezerNameLevelII]],
  )
  
  return(out)
}

SetMoveRequirements <- function(input, sample_type){
  
  #get ui elements
  ui_elements <- GetUIMoveElements(sample_type)
  
  out <- req(
    input[[ui_elements$ui.input$MoveDataSet]] # user must supply move files
  )
  
  return(out)
}

SetFreezerAddRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$AddFreezerName]],
      input[[ui_elements$ui.input$AddFreezerType]],
      input[[ui_elements$ui.input$AddFreezerLevel_I]],
      input[[ui_elements$ui.input$AddFreezerLevel_II]],
      sampleDB:::.CheckFreezerNameIsUnique(input, database, 
                                           freezer_address = list(freezer_name = input[[ui_elements$ui.input$AddFreezerName]],
                                                                  freezer_levelI = input[[ui_elements$ui.input$AddFreezerLevel_I]],
                                                                  freezer_levelII = input[[ui_elements$ui.input$AddFreezerLevel_II]])) == TRUE)
}

SetFreezerChangeRequirements <- function(input, database, ui_elements){
  req(sum(c(input[[ui_elements$ui.input$RenameFreezerName1]], input[[ui_elements$ui.input$RenameFreezerLevelI1]], input[[ui_elements$ui.input$RenameFreezerLevelII1]]) != "") > 0,
      sum(c(input[[ui_elements$ui.input$RenameFreezerName2]], input[[ui_elements$ui.input$RenameFreezerType2]], input[[ui_elements$ui.input$RenameFreezerLevelI2]], input[[ui_elements$ui.input$RenameFreezerLevelI2]]) != "") > 0,
      sampleDB:::.CheckFreezerNameIsUnique(input, database,
                                           freezer_address = list(freezer_name = input[[ui_elements$ui.input$RenameFreezerName2]],
                                                                  freezer_levelI = input[[ui_elements$ui.input$RenameFreezerLevelI2]],
                                                                  freezer_levelII = input[[ui_elements$ui.input$RenameFreezerLevelII2]])) == TRUE)
}

SetFreezerDeleteRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$DeleteFreezerName]],
      input[[ui_elements$ui.input$DeleteFreezerLevelI]],
      input[[ui_elements$ui.input$DeleteFreezerLevelII]],
      sampleDB:::.CheckFreezerDeletion(input, database,
                                       freezer_address = list(freezer_name = input[[ui_elements$ui.input$DeleteFreezerName]],
                                                              freezer_levelI = input[[ui_elements$ui.input$DeleteFreezerLevelI]],
                                                              freezer_levelII = input[[ui_elements$ui.input$DeleteFreezerLevelII]])) == TRUE)
}

SetAddStudyRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$AddStudyTitle]],
      input[[ui_elements$ui.input$AddStudyDescription]],
      input[[ui_elements$ui.input$AddStudyShortCode]],
      input[[ui_elements$ui.input$AddStudyLeadPerson]],
      sampleDB:::.CheckStudyTitleIsUnique(input = input, database = database,
                                          study_title = input[[ui_elements$ui.input$AddStudyTitle]]) == TRUE,
      sampleDB:::.CheckStudyShortCodeIsUnique(input = input, database = database,
                                              study_short_code = input[[ui_elements$ui.input$AddStudyShortCode]]) == TRUE)
}

SetChangeStudyRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$ChangeStudyShortCode]],
      input[[ui_elements$ui.input$RenameStudyTitle]],
      input[[ui_elements$ui.input$RenameStudyDescription]],
      input[[ui_elements$ui.input$RenameStudyShortCode]],
      input[[ui_elements$ui.input$RenameStudyLeadPerson]],
      sampleDB:::.CheckStudyTitleIsUnique(input = input, database = database,
                                          study_title = input[[ui_elements$ui.input$RenameStudyTitle]]) == TRUE)
}

SetDeleteStudyRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$DeleteStudyShortCode]],
      sampleDB:::.CheckStudyDeletion(input, database,
                                     study_ui = input[[ui_elements$ui.input$DeleteStudyShortCode]]) == TRUE)
}

# Resets
UploadReset <- function(input, output, sample_type){
  
  #get ui elements
  ui_elements <- GetUIUploadElements(sample_type)
  
  observeEvent(
    input[[ui_elements$ui.input$ClearForm]],
    ({
      shinyjs::reset(ui_elements$ui.input$UploadPlateID)
      shinyjs::reset(ui_elements$ui.input$UploadDataSet)
      shinyjs::reset(ui_elements$ui.input$UploadFreezerName)
      shinyjs::reset(ui_elements$ui.input$UploadFreezerNameLevelI)
      shinyjs::reset(ui_elements$ui.input$UploadFreezerNameLevelII)
      output[[ui_elements$ui.output$WarningUploadSampleID]] <- renderText({""})
      output[[ui_elements$ui.input$UploadReturnMessage1]] <- renderText({""})
      output[[ui_elements$ui.input$UploadReturnMessage2]] <- renderText({""})
    }))  
}

MoveReset <- function(input, output){
  observeEvent(
    input$ClearMoveForm,
    ({
      shinyjs::reset("MoveDataSet")
      output$MoveReturnMessage2 <- renderText({""})}))
}

UpdateFreezerDropdowns <- function(database, session){
  shinyjs::reset("AddFreezerName")
  shinyjs::reset("AddFreezerType")
  shinyjs::reset("AddFreezerLevel_I")
  shinyjs::reset("AddFreezerLevel_II")
  shinyjs::reset("RenameFreezerName2")
  updateSelectInput(session = session, inputId = "RenameFreezerName1", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name))
  updateSelectInput(session = session, inputId = "DeleteFreezerName", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name))
}

UpdateSpecimenTypeDropdowns <- function(database, session){
  shinyjs::reset("AddSpecimenType")
  shinyjs::reset("RenameSpecimenType2")
  updateSelectInput(session = session, inputId = "RenameSpecimenType1", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
  updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
}

UpdateStudyDropdowns <- function(database, session){
  shinyjs::reset("AddStudyTitle")
  shinyjs::reset("AddStudyDescription")
  shinyjs::reset("AddStudyLeadPerson")
  shinyjs::reset("AddStudyShortCode")
  shinyjs::reset("AddStudyIsLongitudinal")
  shinyjs::reset("AddStudyIsHidden")
  shinyjs::reset("RenameStudyTitle")
  shinyjs::reset("RenameStudyDescription")
  shinyjs::reset("RenameStudyLeadPerson")
  shinyjs::reset("RenameStudyShortCode")
  shinyjs::reset("DeleteStudyShortCode")
  updateSelectInput(session = session, "ChangeStudyShortCode", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code))
  updateCheckboxInput(session = session, "RenameStudyIsLongitudinal", value = FALSE)
  updateCheckboxInput(session = session, "RenameStudyIsHidden", value = FALSE)
  updateSelectInput(session = session, "DeleteStudyShortCode", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code))
}
