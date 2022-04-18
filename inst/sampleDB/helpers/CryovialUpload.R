
# Helper Funs for Uploading Samples Through the App

MatrixUpload <- function(session, output, input, database){
  
  # auto-filter freezer addresses in dropdown
  SmartFreezerDropdownFilter(input = input, database = database, session = session)
  
  # conduct matrix upload tests
  ConductUploadDataChecks(database = database, input = input, output = output, sample_type = "micronix")
  
  observeEvent(
    input$UploadMicronixActionButton,
    ({
      # set upload reqs
      SetUploadRequirements(input = input, database = database, sample_type = "micronix")
      # fire upload trigger
      updateTextInput(session = session, "ActionUploadMatrix", value = "Go"); Sys.sleep(.75)
      # print user message
      output$UploadMicronixReturnMessage1 <- renderText({"Working..."})
    }))
  
  observe({
    if(input$ActionUploadMatrix == "Go"){
      # upload samples
      sampleDB::UploadSamples(sample_type = "micronix", upload_data = ReformatMicronixUploadData(input), container_name = input$"UploadMicronixPlateID", 
                              container_barcode = input$"UploadMicronixPlateBarcode", 
                              freezer_address = list(location_name = input$"UploadMicronixLocation", level_I = input$"UploadLocationMicronixLevelI", 
                                                     level_II = input$"UploadLocationMicronixLevelII"))
      # print user message
      output$UploadMicronixReturnMessage2 <- renderText({"Upload Complete"})
      # reset trigger
      updateTextInput(session = session, "ActionUploadMatrix", value = "")
    }
  })
  
  UploadReset(input = input, output = output, sample_type = "micronix")
  UploadExamples(input = input, database = database, output = output, sample_type = "micronix")
}

CryoUpload <- function(session, output, input, database, ref.clear_action){

  observe({
    if(input$UploadLocationCryoFreezerName != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input$UploadLocationCryoFreezerName)
      updateSelectInput(session, "UploadLocationCryoLevelI", label = NULL, choices = c(tmp_table.location$level_I))
      updateSelectInput(session, "UploadLocationCryoLevelII", label = NULL, choices = c(tmp_table.location$level_II))
    }else{
      updateSelectInput(session, "UploadLocationCryoLevelI", label = NULL, choices = c(""))
      updateSelectInput(session, "UploadLocationCryoLevelII", label = NULL, choices = c(""))
    }
  })
  
  # A. PERFORM UPLOAD CHECKS... PRINT GOOD USER MESSAGES
  ConductUploadDataChecks(database = database,
               input,
               output,
               type = "cryo",
               ui.input = list(UploadPlateID = "UploadCryoPlateID",
                               UploadDataSet = "UploadCryoDataSet"),
               ui.output = list(WarningUploadSampleID = "WarningCryoUploadSampleID",
                               WarningUploadColnames = "WarningCryoUploadColnames",
                               WarningUploadSpecimenTypes = "WarningUploadCryoSpecimenTypes",
                               WarningUploadDateFormat = "WarningCryoUploadDateFormat",
                               WarningUploadStudyShortCodes = "WarningUploadCryoStudyShortCodes",
                               WarningSpecimenExists = "WarningCryoSpecimenExists",
                               WarningUploadContainer = "WarningCryoUploadContainer"))
  
  # ADD A NEW PLATE TO THE DATABASE... LINK ACTION BUTTON TO TRIGGER CASCADE
  observeEvent(
    input$UploadActionCryoSamples,
    ({
      # TRIGGER UI CHANGE FOR REACTIVITY - RECYCLE RENAMESTUDYLEADPERSON
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "rEdSD$s&rc7atn#D")
      
      # PAUSE FOR EFFECT AND PRINT WORKING
      Sys.sleep(.75)
      output$UploadCryoReturnMessage1 <- renderText({"Working..."})
    }))
  
  # UPLOAD SAMPLES
  observe({
    
    # WHEN REACTIVE UI IS CHANGED TO INDICATE AN UPLOAD
    if(input$RenameStudyLeadPerson == "rEdSD$s&rc7atn#D"){
      
      # B. CHECK REQUIREMENTS
      SetUploadRequirements(input = input,
                         database = database, 
                         ui.input = list(UploadPlateID = "UploadCryoPlateID",
                                         UploadDataSet = "UploadCryoDataSet",
                                         UploadFreezerName = "UploadLocationCryoFreezerName",
                                         UploadFreezerNameLevelI = "UploadLocationCryoLevelI",
                                         UploadFreezerNameLevelII = "UploadLocationCryoLevelII"))
      
      # UPLOAD SAMPLES
      sampleDB::UploadSamples(sample_type = "cryovial",
                              upload_data = input$UploadCryoSamples$datapath,
                              container_name = input$UploadBoxID,
                              freezer = list(location_name = input$UploadLocationCryoFreezerName, 
                                                   level_I = input$UploadLocationCryoLevelI, 
                                                   level_II = input$UploadLocationCryoLevelII))
      # 
      # #UPDATE THE SEARCH DROPDOWNS
      # warning("search dropdowns are not updated")
      # updateSelectizeInput(session = session,
      #                      "SearchByPlateID",
      #                      choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name),
      #                      label = NULL)
      
      # PRINT UPLOAD MSG
      output$UploadCryoReturnMessage2 <- renderText({"Upload Complete"})
      
      # RESET UI VALUE
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "")
    }
  })
  
  # D. CLEAR FORM
  UploadReset(input = input, 
              output = output, 
              ref.clear_action = ref.clear_action,
              ui.input = list(UploadPlateID = "UploadCryoPlateID",
                              UploadDataSet = "UploadCryoDataSet",
                              UploadLocation = "UploadCryoLocation",
                              UploadLocationLevel_I = "UploadCryoLocation",
                              UploadLocationLevel_II = "UploadCryoLocation",
                              UploadReturnMessage1 = "UploadCryoReturnMessage1",
                              UploadReturnMessage2 = "UploadCryoReturnMessage2"))
  
  # E. EXAMPLES
  UploadExamples(input = input,
                 database = database, 
                 output = output,
                 type = "cryo",
                 ui.output = list(ExampleUploadCSVNoDate = "ExampleUploadCryoCSVNoDate",
                                  ExampleUploadCSVDate = "ExampleUploadCryoCSVDate"))
}

RDTUpload <- function(session, output, input, database, ref.clear_action){
  
  observe({
    if(input$UploadLocationRDTFreezerName != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input$UploadLocationRDTFreezerName)
      updateSelectInput(session, "UploadLocationRDTLevelI", label = NULL, choices = c(tmp_table.location$level_I))
      updateSelectInput(session, "UploadLocationRDTLevelII", label = NULL, choices = c(tmp_table.location$level_II))
    }else{
      updateSelectInput(session, "UploadLocationRDTLevelI", label = NULL, choices = c(""))
      updateSelectInput(session, "UploadLocationRDTLevelII", label = NULL, choices = c(""))
    }
  })
  
  # A. PERFORM UPLOAD CHECKS... PRINT GOOD USER MESSAGES
  ConductUploadDataChecks(database = database,
               input,
               output,
               type = "rdt",
               ui.input = list(UploadPlateID = "UploadRDTPlateID",
                               UploadDataSet = "UploadRDTDataSet"),
               ui.output = list(WarningUploadSampleID = "WarningRDTUploadSampleID",
                                WarningUploadColnames = "WarningRDTUploadColnames",
                                WarningUploadSpecimenTypes = "WarningUploadRDTSpecimenTypes",
                                WarningUploadDateFormat = "WarningRDTUploadDateFormat",
                                WarningUploadStudyShortCodes = "WarningUploadRDTStudyShortCodes",
                                WarningSpecimenExists = "WarningRDTSpecimenExists",
                                WarningUploadContainer = "WarningRDTUploadContainer"))
  
  # ADD A NEW PLATE TO THE DATABASE... LINK ACTION BUTTON TO TRIGGER CASCADE
  observeEvent(
    input$UploadActionRDTSamples,
    ({
      # TRIGGER UI CHANGE FOR REACTIVITY - RECYCLE RENAMESTUDYLEADPERSON
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "9k@YczN!EJ$CNyP9")

      # PAUSE FOR EFFECT AND PRINT WORKING
      Sys.sleep(.75)
      output$UploadRDTReturnMessage1 <- renderText({"Working..."})
    }))
  
  # UPLOAD SAMPLES
  observe({
    
    # WHEN REACTIVE UI IS CHANGED TO INDICATE AN UPLOAD
    if(input$RenameStudyLeadPerson == "9k@YczN!EJ$CNyP9"){
      
      # B. CHECK REQUIREMENTS
      SetUploadRequirements(input = input,
                         database = database,
                         ui.input = list(UploadPlateID = "UploadRDTPlateID",
                                         UploadDataSet = "UploadRDTDataSet",
                                         UploadFreezerName = "UploadLocationRDTFreezerName",
                                         UploadFreezerNameLevelI = "UploadLocationRDTLevelI",
                                         UploadFreezerNameLevelII = "UploadLocationRDTLevelII"))
      
      # UPLOAD SAMPLES
      sampleDB::UploadSamples(sample_type = "rdt",
                              upload_data = input$UploadRDTSamples$datapath,
                              container_name = input$UploadBagID,
                              freezer = list(location_name = input$UploadLocationRDTFreezerName,
                                                   level_I = input$UploadLocationRDTLevelI,
                                                   level_II = input$UploadLocationRDTLevelII))

      #UPDATE THE SEARCH DROPDOWNS
      # warning("search dropdowns are not updated")
      # updateSelectizeInput(session = session,
      #                      "SearchByPlateID",
      #                      choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name),
      #                      label = NULL)
    }
  })
  
  # D. CLEAR FORM
  UploadReset(input = input, 
              output = output, 
              ref.clear_action = ref.clear_action,
              ui.input = list(UploadPlateID = "UploadRDTPlateID",
                              UploadDataSet = "UploadRDTDataSet",
                              UploadLocation = "UploadRDTLocation",
                              UploadLocationLevel_I = "UploadRDTLocation",
                              UploadLocationLevel_II = "UploadRDTLocation",
                              UploadReturnMessage1 = "UploadRDTReturnMessage1",
                              UploadReturnMessage2 = "UploadRDTReturnMessage2"))
      
      # E. EXAMPLES
      UploadExamples(input = input,
                     database = database,
                     output = output,
                     type = "rdt",
                     ui.output = list(ExampleUploadCSVNoDate = "ExampleUploadRDTCSVNoDate",
                                      ExampleUploadCSVDate = "ExampleUploadRDTCSVDate"))
}

PaperUpload <- function(session, output, input, database, ref.clear_action){
  
  observe({
    if(input$UploadLocationPaperFreezerName != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input$UploadLocationPaperFreezerName)
      updateSelectInput(session, "UploadLocationPaperLevelI", label = NULL, choices = c(tmp_table.location$level_I))
      updateSelectInput(session, "UploadLocationPaperLevelII", label = NULL, choices = c(tmp_table.location$level_II))
    }else{
      updateSelectInput(session, "UploadLocationPaperLevelI", label = NULL, choices = c(""))
      updateSelectInput(session, "UploadLocationPaperLevelII", label = NULL, choices = c(""))
    }
  })
  
  # A. PERFORM UPLOAD CHECKS... PRINT GOOD USER MESSAGES
  ConductUploadDataChecks(database = database,
               input,
               output,
               type = "paper",
               ui.input = list(UploadPlateID = "UploadPaperPlateID",
                               UploadDataSet = "UploadPaperDataSet"),
               ui.output = list(WarningUploadSampleID = "WarningPaperUploadSampleID",
                                WarningUploadColnames = "WarningPaperUploadColnames",
                                WarningUploadSpecimenTypes = "WarningUploadPaperSpecimenTypes",
                                WarningUploadDateFormat = "WarningPaperUploadDateFormat",
                                WarningUploadStudyShortCodes = "WarningUploadPaperStudyShortCodes",
                                WarningSpecimenExists = "WarningPaperSpecimenExists",
                                WarningUploadContainer = "WarningPaperUploadContainer"))
  
  # ADD A NEW PLATE TO THE DATABASE... LINK ACTION BUTTON TO TRIGGER CASCADE
  observeEvent(
    input$UploadPaperSamples,
    ({
      # TRIGGER UI CHANGE FOR REACTIVITY - RECYCLE RENAMESTUDYLEADPERSON
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "EX7n8o9@ifc!$Gsz")
      
      # PAUSE FOR EFFECT AND PRINT WORKING
      Sys.sleep(.75)
      output$UploadPaperReturnMessage1 <- renderText({"Working..."})
    }))
  
  # UPLOAD SAMPLES
  observe({
    
    # WHEN REACTIVE UI IS CHANGED TO INDICATE AN UPLOAD
    if(input$RenameStudyLeadPerson == "EX7n8o9@ifc!$Gsz"){
      
      # B. CHECK REQUIREMENTS
      SetUploadRequirements(input = input,
                         database = database, 
                         ui.input = list(UploadPlateID = "UploadPaperPlateID",
                                         UploadDataSet = "UploadPaperDataSet",
                                         UploadFreezerName = "UploadLocationPaperFreezerName",
                                         UploadFreezerNameLevelI = "UploadLocationPaperLevelI",
                                         UploadFreezerNameLevelII = "UploadLocationPaperLevelII"))
      
      # UPLOAD SAMPLES
      sampleDB::UploadSamples(sample_type = "paper",
                              upload_data = read.csv(input$UploadPaperSamples$datapath, check.names = F) %>% tidyr::drop_na(),
                              container_name = input$UploadBagID2,
                              freezer = list(location_name = input$UploadLocationPaperFreezerName, 
                                                   level_I = input$UploadLocationPaperLevelI, 
                                                   level_II = input$UploadLocationPaperLevelII))
      # 
      # #UPDATE THE SEARCH DROPDOWNS
      # warning("search dropdowns are not updated")
      # updateSelectizeInput(session = session,
      #                      "SearchByPlateID",
      #                      choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name),
      #                      label = NULL)
      
      # PRINT UPLOAD MSG
      output$UploadPaperReturnMessage2 <- renderText({"Upload Complete"})
      
      # RESET UI VALUE
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "")
    }
  })
  
  # D. CLEAR FORM
  UploadReset(input = input, 
              output = output, 
              ref.clear_action = ref.clear_action,
              ui.input = list(UploadPlateID = "UploadPaperPlateID",
                              UploadDataSet = "UploadPaperDataSet",
                              UploadLocation = "UploadPaperLocation",
                              UploadLocationLevel_I = "UploadPaperLocation",
                              UploadLocationLevel_II = "UploadPaperLocation",
                              UploadReturnMessage1 = "UploadPaperReturnMessage1",
                              UploadReturnMessage2 = "UploadPaperReturnMessage2"))
  
  # E. EXAMPLES
  UploadExamples(input = input,
                 database = database,
                 output = output,
                 type = "paper",
                 ui.output = list(ExampleUploadCSVNoDate = "ExampleUploadPaperCSVNoDate",
                                  ExampleUploadCSVDate = "ExampleUploadPaperCSVDate"))
  
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

ConductUploadDataChecks <- function(database, sample_type, input, output){
  if(sample_type == "micronix"){
    ui.input <- list(UploadPlateID = "UploadMicronixPlateID",
                     UploadPlateBarcode = "UploadMicronixPlateBarcode",
                     UploadDataSet = "UploadMicronixDataSet")
    ui.output = list(WarningUploadSampleID = "WarningMicronixUploadSampleID",
                     WarningUploadColnames = "WarningMicronixUploadColnames",
                     WarningUploadSpecimenTypes = "WarningUploadMicronixSpecimenTypes",
                     WarningUploadDateFormat = "WarningMicronixUploadDateFormat",
                     WarningUploadStudyShortCodes = "WarningUploadMicronixStudyShortCodes",
                     WarningSpecimenExists = "WarningMicronixSpecimenExists",
                     WarningUploadContainerName = "WarningMicronixUploadContainerName",
                     WarningUploadContainerBarcode = "WarningMicronixUploadContainerBarcode") 
  }
  
  # CHECK PLATE_ID ARE UNIQUE
  CheckUploadContainerNameDuplication <- reactive({.CheckUploadContainerNameDuplication(input, database, ui.input)})
  output[[ui.output$WarningUploadContainerName]] <- renderText(CheckUploadContainerNameDuplication())
  
  # CHECK PLATE_BARCODE ARE UNIQUE
  CheckUploadContainerBarcodeDuplication <- reactive({.CheckUploadContainerBarcodeDuplication(input, database, ui.input)})
  output[[ui.output$WarningUploadContainerBarcode]] <- renderText(CheckUploadContainerBarcodeDuplication())
  
  # CHECK THAT USR SPECIMEN TYPES ARE VALID
  CheckUploadSpecimenTypes <- reactive({.CheckUploadSpecimenTypes(input, database, ui.input, sample_type)})
  output[[ui.output$WarningUploadSpecimenTypes]] <- renderText(CheckUploadSpecimenTypes())
  
  # CHECK THAT DATE IS IN CORRECT FORMAT
  CheckUploadDateFormat <- reactive({.CheckUploadDateFormat(input, database, ui.input, sample_type)})
  output[[ui.output$WarningUploadDateFormat]] <- renderText(CheckUploadDateFormat())
  
  # CHECK THAT USR STUDY SHORT CODES ARE VALID
  CheckUploadStudyShortCode <- reactive({.CheckUploadStudyShortCodes(input, database, ui.input, sample_type)})
  output[[ui.output$WarningUploadStudyShortCodes]] <- renderText(CheckUploadStudyShortCode())
  
  # CHECK THAT COLNAMES ARE CORRECT
  CheckUploadColnames <- reactive({.CheckUploadColnames(input, database, sample_type, ui.input)})
  output[[ui.output$WarningUploadColnames]] <- renderText(CheckUploadColnames())
  
  # CHECK THAT BARCODES/ LABELS ARE NOT IN DATABASE
  CheckUploadPlateUniqSampleIDConstraint <- reactive({.CheckUploadPlateUniqSampleIDConstraint(input, database, sample_type, ui.input)})
  output[[ui.output$WarningUploadSampleID]] <- renderText(CheckUploadPlateUniqSampleIDConstraint())
  
  # # CHECK THAT SPECIMEN TYPE IS UNIQUE
  # CheckSpecimenExists <- reactive({.CheckSpecimenExists(input, database, ui.input)})
  # output[[ui.output$WarningSpecimenExists]] <- renderText(CheckSpecimenExists())
}

ReformatMicronixUploadData <- function(input){
  
  #read in micronix data file and remove lines with blanks or 'No Tube' in them
  upload_data <- read.csv(input$"UploadMicronixDataSet"$datapath, 
                          check.names = F) %>% 
    suppressWarnings()
  
  if(input$UploadFileType == "traxcer"){
    upload_data <- upload_data %>% 
      setNames(.[1,]) %>% .[-1,] %>%
      mutate(label = na_if(`Tube ID`, ""),
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      tidyr::drop_na()
  }else{
    upload_data <- upload_data %>%
      mutate(label = na_if(TubeCode, "No Tube"),
             well_position = paste0(LocationRow, LocationColumn)) %>%
      tidyr::drop_na()
  }
  
  print(upload_data)
  
  return(upload_data)
}

SetUploadRequirements <- function(input, database, sample_type){
  
  if(sample_type == "micronix"){
    ui.input = list(UploadPlateID = "UploadMicronixPlateID",
                    UploadPlateBarcode = "UploadMicronixPlateBarcode",
                    UploadDataSet = "UploadMicronixDataSet",
                    UploadFreezerName = "UploadMicronixLocation",
                    UploadFreezerNameLevelI = "UploadLocationMicronixLevelI",
                    UploadFreezerNameLevelII = "UploadLocationMicronixLevelII")
  }
  
  # SET REQUIREMENTS
  out <- req(
    
    # - USER MUST UPLOAD A DATASET
    input[[ui.input$UploadDataSet]]$datapath,
    
    # - USER MUST UPLOAD A PLATE NAME
    input[[ui.input$UploadPlateID]],
    
    # - USER MUST UPLOAD LOCATION
    input[[ui.input$UploadFreezerName]],
    
    # - USER MUST UPLOAD LOCATION
    input[[ui.input$UploadFreezerName]],
    
    # - USER MUST UPLOAD LOCATION
    input[[ui.input$UploadFreezerNameLevelII]],
    
    # # - DATASET BARCODES CANNOT BE IN DATABASE
    # !(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)),
    # 
    # # - PLATE NAME CANNOT BE IN DATABASE
    # !(input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name))
    )
  
  return(out)
  
}

UploadReset <- function(input, output, sample_type){
  
  if(sample_type == "micronix"){
    ref.clear_action <- "ClearMicronixUploadForm"
    ui.input <- list(UploadPlateID = "UploadMicronixPlateID",
                    UploadDataSet = "UploadMicronixDataSet",
                    UploadLocation = "UploadMicronixLocation",
                    UploadLocationLevel_I = "UploadMicronixLocation",
                    UploadLocationLevel_II = "UploadMicronixLocation",
                    UploadReturnMessage1 = "UploadMicronixReturnMessage1",
                    UploadReturnMessage2 = "UploadMicronixReturnMessage2")
  }
  
  observeEvent(
    input[[ref.clear_action]],
    ({
      shinyjs::reset(ui.input$UploadPlateID)
      shinyjs::reset(ui.input$UploadDataSet)
      shinyjs::reset(ui.input$UploadLocation)
      shinyjs::reset(ui.input$UploadLocationLevel_I)
      shinyjs::reset(ui.input$UploadLocationLevel_II)
      output[[ui.input$UploadReturnMessage1]] <- renderText({""})
      output[[ui.input$UploadReturnMessage2]] <- renderText({""})
    }))  
}

UploadExamples <- function(input, database, output, sample_type){
  if(sample_type == "micronix"){
    ui.output <- list(LogisticsItems = "LogisticsItems",
                      MetadataItems = "MetadataItems",
                      CombinedItems = "CombinedItems") 
  }
  output[[ui.output$LogisticsItems]] <- renderTable({.ExampleLogisticsItems(database, sample_type)}, striped = T, bordered = T)
  output[[ui.output$MetadataItems]] <- renderTable({.ExampleMetadataItems(database, sample_type)}, striped = T, bordered = T) 
  output[[ui.output$CombinedItems]] <- renderTable({.ExampleCombinedItems(database, sample_type)}, striped = T, bordered = T) 
}

.ExampleLogisticsItems <- function(database, type){
  tibble(Row = "A",
         Column = "1",
         `MicronixBarocde` = "974019283")
}

.ExampleMetadataItems <- function(database, type){
  tibble(`StudyCode` = "CodeXXX",
         `StudyParticipant` = "ParticipantXXX",
         `AssayType` = "Plasma",
         `CollectionDate` = "2022-04-11") 
}

.ExampleCombinedItems <- function(database, type){
  tibble(Row = "A",
         Column = "1",
         `MicronixBarocde` = "974019283",
         `StudyCode` = "CodeXXX",
         `StudyParticipant` = "ParticipantXXX",
         `AssayType` = "Plasma",
         `CollectionDate` = "2022-04-11")
}

.ExampleUploadCSVNoDate <- function(database, type){
  if(type == "micronix"){
    tibble(LocationRow = rep("A", 10),
           LocationColumn = c(1:10),
           TubeCode = paste0("XXX", 1:10),
           study_subject_id = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_short_code = "KAM06") %>% print.data.frame(row.names = FALSE) 
  }
  else if(type == "cryo"){
    tibble(row = 1:10,
           column = 1:10,
           label = c("A","B","C","D","E","F","G","H","I","J"),
           study_subject = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_code = "KAM06") %>% print.data.frame(row.names = FALSE)
  }
  else{
    tibble(label = c("A","B","C","D","E","F","G","H","I","J"),
           study_subject = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_code = "KAM06") %>% print.data.frame(row.names = FALSE)
  }
}

.ExampleUploadCSVDate <-  function(database, type){
  if(type == "micronix"){
    tibble(LocationRow = rep("A", 10),
           LocationColumn = c(1:10),
           TubeCode = paste0("XXX", 1:10),
           study_subject_id = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_short_code = "KAM06",
           collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% print.data.frame(row.names = FALSE) 
  }
  else if(type == "cryo"){
    tibble(row = 1:10,
           column = 1:10,
           label = c("A","B","C","D","E","F","G","H","I","J"),
           study_subject = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_code = "KAM06",
           collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% print.data.frame(row.names = FALSE)
  }
  else{
    tibble(label = c("A","B","C","D","E","F","G","H","I","J"),
           study_subject = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_code = "KAM06",
           collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% print.data.frame(row.names = FALSE)
  }
}