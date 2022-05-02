
# Utility Functions for Main Shiny Functions (e.g. MicronixUpload, MoveWetlabSamples, etc.)

# Get UI Elements
GetUIUploadElements <- function(sample_type, msg = NULL){
  
  if(sample_type == "micronix"){
    ui.input <- list(UploadPlateID = "UploadMicronixPlateID",
                     UploadPlateBarcode = "UploadMicronixPlateBarcode",
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
                     WarningUploadDateFormat = "WarningMicronixUploadDateFormat",
                     WarningUploadStudyShortCodes = "WarningUploadMicronixStudyShortCodes",
                     WarningSpecimenExists = "WarningMicronixSpecimenExists",
                     WarningUploadContainerName = "WarningMicronixUploadContainerName",
                     WarningUploadContainerBarcode = "WarningMicronixUploadContainerBarcode",
                     WarningUploadBarcodeRepeats = "WarningMicronixUploadBarcodeRepeats")
  }

  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIMoveElements <- function(sample_type, msg = NULL){
  
  if(sample_type == "micronix"){
    ui.input <-  list(MicronixFileType = "MoveFileType",
                      MoveDataSet = "MoveDataSet")
    ui.output <- list(WarningLogisticalColnames = "WarningMoveLogisticalColnames",
                      WarningMoveBarcodesExist = "WarningMoveBarcodesExist")
  }
  
  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

# Perform Checks
CheckLogisticalColnamesOfUserProvidedMicronixFile <- function(input, output, users_upload_file, ui_elements){
  
  #read in user uploaded data file
  message("Checking colnames of user provided file...")
  
  #validate colnames of user provided file format and print user messages if file is not valid
  upload_file_type <- input[[ui_elements$ui.input$MicronixFileType]]
  out <- sampleDB:::.CheckLogisticalColnamesOfUserProvidedMicronixFile(upload_file_type = upload_file_type, users_upload_file = users_upload_file)
  
  output[[ui_elements$ui.output$WarningLogisticalColnames]] <- renderText({
    if(upload_file_type == "visionmate"){
      validate(need(out, "ERROR:\nMalformed Logictical Colnames (Valid VisionMate Column Names: LocationRow, LocationColumn, TubeCode)"))
    }
    else if(upload_file_type == "traxcer"){
      validate(need(out, "ERROR:\nMalformed Logictical Colnames (Valid Traxcer Column Names: Position, Tube ID)"))
    }
    else{
      validate(need(out, "ERROR:\nMalformed Logictical Colnames (Valid Column Names: MicronixBarcode, Row, Column)"))
    }
  })

  return(out)
}

CheckMetadataColnamesOfUserProvidedMicronixFile <- function(input, output, users_upload_file, ui_elements){
  
  #read in user uploaded data file
  message("Checking colnames of user provided file...")
  
  #validate colnames of user provided file format and print user messages if file is not valid
  out <- sampleDB:::.CheckMetadataColnamesOfUserProvidedMicronixFile(users_upload_file = users_upload_file)
  output[[ui_elements$ui.output$WarningMetadataColnames]] <- renderText({
    validate(need(out, "ERROR:\nMalformed Metadata Colnames (Valid Metadata Column Names: StudyCode, Participant, SpecimenType, [CollectionDate])"))
  })
  return(out)
}

CheckFormattedUploadFile <- function(output, database, formatted_upload_file, ui_elements){
  
  message("Checking formatted data in file...")
  
  # check valid specimen type
  output[[ui_elements$ui.output$WarningUploadSpecimenTypes]] <- renderText({
    out <- sampleDB:::.CheckUploadSpecimenTypes(database = database, formatted_upload_file = formatted_upload_file) 
    validate(need(out, "ERROR:\nSpecimen Type Not found... Consider creating a new specimen type"))
  })
  
  # check study short codes
  output[[ui_elements$ui.output$WarningUploadStudyShortCodes]] <- renderText({
    out <- sampleDB:::.CheckUploadStudyShortCodes(database = database, formatted_upload_file = formatted_upload_file)
    validate(need(out, "ERROR:\nStudy Short Code Not found... Consider creating a new study"))
  })
  
  # check date format
  output[[ui_elements$ui.output$WarningUploadDateFormat]] <- renderText({
    out <- sampleDB:::.CheckUploadDateFormat(database = database, formatted_upload_file = formatted_upload_file)
    validate(need(out, "ERROR:\nAll Collection Dates are Not in YMD format"))
  })
  
  # check unique barcodes
  output[[ui_elements$ui.output$WarningUploadSampleID]] <- renderText({
    out <- sampleDB:::.CheckBarcodeIsntInDB(database = database, formatted_upload_file = formatted_upload_file)
    validate(need(out$out1, paste("ERROR:\nUnique Barcode Constraint", out$out2)))
  })
  
  # check unique barcodes
  output[[ui_elements$ui.output$WarningUploadBarcodeRepeats]] <- renderText({
    out <- sampleDB:::.CheckBarcodeArentRepeated(database = database, formatted_upload_file = formatted_upload_file)
    validate(need(out$out1, paste("ERROR:\nUnique Barcode Constraint", out$out2)))
  })
}

CheckFormattedMoveFile <- function(output, database, sample_type, formatted_move_file_list){
  #get ui elements
  ui_elements <- GetUIMoveElements(sample_type)
  message("Checking formatted data in file...")
  
  # check valid specimen type
  output[[ui_elements$ui.output$WarningMoveBarcodesExist]] <- renderText({
    out <- sampleDB:::.CheckBarcodesInDatabase(database = database, formatted_move_file_list = formatted_move_file_list)
    validate(need(out, "ERROR:\nAll barcodes are not in the database"))
  })
}

CheckPlates <- function(database, sample_type, input, output){
  
  #get ui elements
  ui_elements <- GetUIUploadElements(sample_type)
  message("Checking user provided plate names...")
  
  # check unique plate names
  output[[ui_elements$ui.output$WarningUploadContainerName]] <- renderText({
    plate_name <- input[[ui_elements$ui.input$UploadPlateID]]
    out <- sampleDB:::.CheckUploadContainerNameDuplication(database = database,plate_name = plate_name)
    validate(need(out, "ERROR:\nPlate name is not unique"))
  })
  
  # check unique plate barcodes
  output[[ui_elements$ui.output$WarningUploadContainerBarcode]] <- renderText({
    plate_barcode <- input[[ui_elements$ui.input$UploadPlateBarcode]]
    out <- sampleDB:::.CheckUploadContainerBarcodeDuplication(plate_barcode = plate_barcode, database = database)
    validate(need(out, "ERROR:\nPlate barcode is not unique"))
  })
}

# Format Files
FormatMicronixUploadData <- function(input, users_upload_file, ui_elements){
  
  #read in validated user provided micronix data file
  message("Formatting user provided file...")
  
  upload_file_type <- input[[ui_elements$ui.input$MicronixFileType]]
  formatted_logistics_upload_file <- .FormatMicronixLogisticalData(upload_file_type, users_upload_file)
  formatted_logistics_and_metadata_file <- .FormatMicronixMetaData(users_upload_file = formatted_logistics_upload_file)
  return(formatted_logistics_and_metadata_file)
}

FormatMicronixMoveData <- function(ui_elements, micronix_move_data, input){
  
  #read in validated user provided micronix data file
  message("Formatting user provided file...")
  
  upload_file_type <- input[[ui_elements$ui.input$MicronixFileType]]
  formatted_move_file_list <- list()
  for(item in names(micronix_move_data)){
    formatted_move_file <- .FormatMicronixLogisticalData(upload_file_type, micronix_move_data[[item]])
    formatted_move_file_list[[item]] <- formatted_move_file
  }
  return(formatted_move_file_list)  
}

.FormatMicronixLogisticalData <- function(upload_file_type, users_upload_file){
  
  if(upload_file_type == "traxcer"){
    formatted_upload_file <- users_upload_file %>% 
      setNames(.[1,]) %>% .[-1,] %>%
      mutate(label = replace(`Tube ID`, nchar(`Tube ID`) != 10, NA),
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      tidyr::drop_na() %>%
      select(-c("Position","Tube ID"))
  }
  else if(upload_file_type == "visionmate"){
    formatted_upload_file <- users_upload_file %>%
      mutate(label = replace(TubeCode, nchar(TubeCode) != 10, NA),
             well_position = paste0(LocationRow, LocationColumn)) %>%
      tidyr::drop_na() %>%
      select(-c("TubeCode","LocationRow", "LocationColumn"))
  }
  else{
    formatted_upload_file <- users_upload_file %>%
      mutate(label = replace(MicronixBarcode, nchar(MicronixBarcode) != 10, NA),
             well_position = paste0(Row, Column)) %>%
      tidyr::drop_na() 
  }
  #removing row if micronix barcode is not string len 10
  
  return(formatted_upload_file)
}

.FormatMicronixMetaData <- function(upload_file_type, users_upload_file){
  
  formatted_upload_file <- users_upload_file %>% 
    rename(specimen_type = SpecimenType,
           study_short_code = StudyCode,
           study_subject_id = Participant)
  
  if("CollectionDate" %in% names(formatted_upload_file)){
    formatted_upload_file <- formatted_upload_file %>% 
      mutate(CollectionDate = na_if(CollectionDate, "")) %>% 
      rename(collection_date = CollectionDate)
  }
  
  return(formatted_upload_file)
}

# General Operations
SmartFreezerDropdownFilter <- function(input, database, session){
  observe({
    if(input$UploadMicronixLocation != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input$UploadMicronixLocation)
      updateSelectInput(session, "UploadLocationMicronixLevelI", label = NULL, choices = c(tmp_table.location$level_I) %>% sort())
      updateSelectInput(session, "UploadLocationMicronixLevelII", label = NULL, choices = c(tmp_table.location$level_II) %>% sort())
    }else{
      updateSelectInput(session, "UploadLocationMicronixLevelI", label = NULL, choices = c(""))
      updateSelectInput(session, "UploadLocationMicronixLevelII", label = NULL, choices = c(""))
    }
  })
}

SetUploadRequirements <- function(input, database, sample_type){
  
  #get ui elements
  ui_elements <- GetUIUploadElements(sample_type)
  
  out <- req(
    
    # user must upload files
    input[[ui_elements$ui.input$UploadDataSet]]$datapath,
    
    # user must supply a plate name
    input[[ui_elements$ui.input$UploadPlateID]],
    
    # user must supply a freezer name
    input[[ui_elements$ui.input$UploadFreezerName]],
    
    # user must supply a freezer name - level I
    input[[ui_elements$ui.input$UploadFreezerNameLevelI]],
    
    # user must supply a freezer name - level II
    input[[ui_elements$ui.input$UploadFreezerNameLevelII]],
  )
  
  return(out)
}

SetMoveRequirements <- function(input, sample_type){
  
  #get ui elements
  ui_elements <- GetUIMoveElements(sample_type)
  
  out <- req(
    # user must supply move files
    input[[ui_elements$ui.input$MoveDataSet]],
  )
  
  return(out)
}

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
      output[[ui_elements$ui.input$UploadReturnMessage1]] <- renderText({""})
      output[[ui_elements$ui.input$UploadReturnMessage2]] <- renderText({""})
    }))  
}

MoveReset <- function(input, output){
  observeEvent(
    input$ClearMoveForm,
    ({
      shinyjs::reset("MoveDataSet")
      output$MoveReturnMessage1 <- renderText({""})
      output$MoveReturnMessage2 <- renderText({""})}))
}
