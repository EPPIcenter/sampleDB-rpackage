
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

CheckColnamesOfUserProvidedMicronixFileFormat <- function(input, output, users_upload_file, sample_type){
  
  #get ui elements to know where to output error message
  ui_elements <- GetUIElements(sample_type)
  
  #read in user uploaded data file
  message("Checking colnames of user provided file...")
  
  #validate colnames of user provided file format and print user messages if file is not valid
  out <- .CheckColnamesOfUserProvidedMicronixFileFormat(input = input, users_upload_file = users_upload_file, sample_type = sample_type)
  output[[ui_elements$ui.output$WarningUploadColnames]] <- renderText({
    if(input[[ui_elements$ui.input$UploadFileType]] == "visionmate"){
      validate(need(out, "ERROR: MALFORMED COLUMN NAMES\nValid VisionMate Column Names are...\nLocationRow, LocationColumn, TubeCode, StudyCode, Participant, SpecimenType, (CollectionDate)"))
    }
    else if(input[[ui_elements$ui.input$UploadFileType]] == "traxcer"){
      validate(need(out, "ERROR: MALFORMED COLUMN NAMES\nValid Traxcer Column Names are...\nPosition, Tube ID, StudyCode, Participant, SpecimenType, (CollectionDate)"))
    }
    else{
      validate(need(out, "ERROR: MALFORMED COLUMN NAMES\nValid Column Names are...\nMicronixBarcode, Row, Column, StudyCode, Participant, SpecimenType, (CollectionDate)"))
    }
  })
  return(out)
}

FormatMicronixUploadData <- function(input, sample_type, users_upload_file){
    
  #get ui elements to know UploadFileType
  ui_elements <- GetUIElements(sample_type)
  
  #read in validated user provided micronix data file
  message("Formatting user provided file...")
  
  #remove row if micronix barcode is not string len 10
  
  if(input[[ui_elements$ui.input$UploadFileType]] == "traxcer"){
    formatted_upload_file <- users_upload_file %>% 
      setNames(.[1,]) %>% .[-1,] %>%
      rename(specimen_type = SpecimenType,
             study_short_code = StudyCode,
             study_subject_id = Participant) %>%
      mutate(label = replace(`Tube ID`, nchar(`Tube ID`) != 10, NA),
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      tidyr::drop_na() %>%
      select(-c("Position","Tube ID"))
  }
  else if(input[[ui_elements$ui.input$UploadFileType]] == "visionmate"){
    formatted_upload_file <- users_upload_file %>%
      rename(specimen_type = SpecimenType,
             study_short_code = StudyCode,
             study_subject_id = Participant) %>% 
      mutate(label = replace(TubeCode, nchar(TubeCode) != 10, NA),
             well_position = paste0(LocationRow, LocationColumn)) %>%
      tidyr::drop_na() %>%
      select(-c("TubeCode","LocationRow", "LocationColumn"))
  }
  else{
    formatted_upload_file <- users_upload_file %>%
      rename(specimen_type = SpecimenType,
             study_short_code = StudyCode,
             study_subject_id = Participant) %>% 
      mutate(label = replace(MicronixBarcode, nchar(MicronixBarcode) != 10, NA),
             well_position = paste0(Row, Column)) %>%
      tidyr::drop_na() 
  }
  
  if("CollectionDate" %in% names(formatted_upload_file)){
    formatted_upload_file <- formatted_upload_file %>% rename(collection_date = CollectionDate)
  }
    
  return(formatted_upload_file)
}

ConductBasicFormattedUploadFileChecks <- function(output, database, sample_type, formatted_upload_file){
  
  #get ui elements
  ui_elements <- GetUIElements(sample_type)
  message("Checking formatted data in file...")
  
  # check valid specimen type
  output[[ui_elements$ui.output$WarningUploadSpecimenTypes]] <- renderText(.CheckUploadSpecimenTypes(database = database, formatted_upload_file = formatted_upload_file))
  
  # check study short codes
  output[[ui_elements$ui.output$WarningUploadStudyShortCodes]] <- renderText(.CheckUploadStudyShortCodes(database = database, formatted_upload_file = formatted_upload_file))
  
  # check date format
  output[[ui_elements$ui.output$WarningUploadDateFormat]] <- renderText(.CheckUploadDateFormat(database = database, formatted_upload_file = formatted_upload_file))
  
  # check unique barcodes
  output[[ui_elements$ui.output$WarningUploadSampleID]] <- renderText(.CheckBarcodeIsntInDB(database = database, formatted_upload_file = formatted_upload_file))
  
  # check unique barcodes
  output[[ui_elements$ui.output$WarningUploadBarcodeRepeats]] <- renderText(.CheckBarcodeArentRepeated(database = database, formatted_upload_file = formatted_upload_file))
  
  # # CHECK THAT SPECIMEN TYPE IS UNIQUE
  # output[[ui_elements$ui.output$WarningSpecimenExists]] <- renderText(.CheckSpecimenExists(input, database, ui_elements$ui.input))  
}

ConductUploadPlateChecks <- function(database, sample_type, input, output){
  
  #get ui elements
  ui_elements <- GetUIElements(sample_type)
  message("Checking user provided plate names...")
  
  # check unique plate names
  output[[ui_elements$ui.output$WarningUploadContainerName]] <- renderText({.CheckUploadContainerNameDuplication(input, database, ui_elements$ui.input)})
  
  # check unique plate barcodes
  output[[ui_elements$ui.output$WarningUploadContainerBarcode]] <- renderText(.CheckUploadContainerBarcodeDuplication(input, database, ui_elements$ui.input))
}

SetUploadRequirements <- function(input, database, sample_type){
  
  #get ui elements
  ui_elements <- GetUIElements(sample_type)
  
  # SET REQUIREMENTS
  out <- req(
    
    # - USER MUST UPLOAD A DATASET
    input[[ui_elements$ui.input$UploadDataSet]]$datapath,
    
    # - USER MUST UPLOAD A PLATE NAME
    input[[ui_elements$ui.input$UploadPlateID]],
    
    # - USER MUST UPLOAD LOCATION
    input[[ui_elements$ui.input$UploadFreezerName]],
    
    # - USER MUST UPLOAD LOCATION
    input[[ui_elements$ui.input$UploadFreezerNameLevelI]],
    
    # - USER MUST UPLOAD LOCATION
    input[[ui_elements$ui.input$UploadFreezerNameLevelII]],
  )
  
  return(out)
}

UploadReset <- function(input, output, sample_type){
  
  #get ui elements
  ui_elements <- GetUIElements(sample_type)
  
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

GetUIElements <- function(sample_type, msg = NULL){
  if(sample_type == "micronix"){
    ui.input <- list(UploadPlateID = "UploadMicronixPlateID",
                     UploadPlateBarcode = "UploadMicronixPlateBarcode",
                     UploadDataSet = "UploadMicronixDataSet",
                     UploadFileType = "UploadFileType",
                     ClearForm = "ClearMicronixUploadForm",
                     UploadFreezerName = "UploadMicronixLocation",
                     UploadFreezerNameLevelI = "UploadLocationMicronixLevelI",
                     UploadFreezerNameLevelII = "UploadLocationMicronixLevelII",
                     UploadReturnMessage1 = "UploadMicronixReturnMessage1",
                     UploadReturnMessage2 = "UploadMicronixReturnMessage2")
    ui.output = list(WarningUploadSampleID = "WarningMicronixUploadSampleID",
                     WarningUploadColnames = "WarningMicronixUploadColnames",
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