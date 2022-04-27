
# Shiny App Functions

GetUIElements <- function(sample_type, msg = NULL){
  # put ui app objects into a list
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
  upload_file_type <- input[[ui_elements$ui.input$UploadFileType]]
  out <- sampleDB:::.CheckColnamesOfUserProvidedMicronixFileFormat(upload_file_type = upload_file_type, users_upload_file = users_upload_file)
  
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

FormatMicronixUploadData <- function(input = input, sample_type = "micronix", users_upload_file = users_upload_file){
  #get ui elements to know UploadFileType
  ui_elements <- GetUIElements(sample_type)
  
  #read in validated user provided micronix data file
  message("Formatting user provided file...")
  
  upload_file_type <- input[[ui_elements$ui.input$UploadFileType]]
  formatted_upload_file <- sampleDB:::.FormatMicronixUploadData(upload_file_type, users_upload_file)
  return(formatted_upload_file)
}

ConductBasicFormattedUploadFileChecks <- function(output, database, sample_type, formatted_upload_file){
  
  #get ui elements
  ui_elements <- GetUIElements(sample_type)
  message("Checking formatted data in file...")
  
  # check valid specimen type
  output[[ui_elements$ui.output$WarningUploadSpecimenTypes]] <- renderText({
    out <- sampleDB:::.CheckUploadSpecimenTypes(database = database, formatted_upload_file = formatted_upload_file) 
    validate(need(out, "Error: Specimen Type Not found... Consider creating a new specimen type"))
  })
  
  # check study short codes
  output[[ui_elements$ui.output$WarningUploadStudyShortCodes]] <- renderText({
    out <- sampleDB:::.CheckUploadStudyShortCodes(database = database, formatted_upload_file = formatted_upload_file)
    validate(need(out, "Error: Study Short Code Not found... Consider creating a new study"))
  })
  
  # check date format
  output[[ui_elements$ui.output$WarningUploadDateFormat]] <- renderText({
    out <- sampleDB:::.CheckUploadDateFormat(database = database, formatted_upload_file = formatted_upload_file)
    validate(need(out, "Error: All Collection Dates are Not in YMD format"))
  })
  
  # check unique barcodes
  output[[ui_elements$ui.output$WarningUploadSampleID]] <- renderText({
    out <- sampleDB:::.CheckBarcodeIsntInDB(database = database, formatted_upload_file = formatted_upload_file)
    validate(need(out$out1, paste("Error: Unique Barcode Constraint", out$out2)))
  })
  
  # check unique barcodes
  output[[ui_elements$ui.output$WarningUploadBarcodeRepeats]] <- renderText({
    out <- sampleDB:::.CheckBarcodeArentRepeated(database = database, formatted_upload_file = formatted_upload_file)
    validate(need(out$out1, paste("Error: Unique Barcode Constraint", out$out2)))
  })
}

ConductUploadPlateChecks <- function(database, sample_type, input, output){
  
  #get ui elements
  ui_elements <- GetUIElements(sample_type)
  message("Checking user provided plate names...")
  
  # check unique plate names
  output[[ui_elements$ui.output$WarningUploadContainerName]] <- renderText({
    plate_name <- input[[ui_elements$ui.input$UploadPlateID]]
    out <- sampleDB:::.CheckUploadContainerNameDuplication(database = database,plate_name = plate_name)
    validate(need(out, "Plate name is not unique"))
  })
  
  # check unique plate barcodes
  output[[ui_elements$ui.output$WarningUploadContainerBarcode]] <- renderText({
    plate_barcode <- input[[ui_elements$ui.input$UploadPlateBarcode]]
    out <- sampleDB:::.CheckUploadContainerBarcodeDuplication(plate_barcode = plate_barcode, database = database)
    validate(need(out, "Plate barcode is not unique"))
  })
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