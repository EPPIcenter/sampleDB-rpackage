
# Helper Funs for Uploading Samples Through the App

MatrixUpload <- function(session, output, input, database, ref.clear_action){
  
  # A. PERFORM UPLOAD CHECKS... PRINT GOOD USER MESSAGES
  UploadChecks(database = database,
               input = input,
               output = output,
               type = "micronix",
               ui.input = list(UploadPlateID = "UploadMicronixPlateID",
                               UploadDataSet = "UploadMicronixDataSet"),
               ui.output = list(WarningUploadSampleID = "WarningMicronixUploadSampleID",
                                WarningUploadColnames = "WarningMicronixUploadColnames",
                                WarningUploadSpecimenTypes = "WarningUploadMicronixSpecimenTypes",
                                WarningUploadDateFormat = "WarningMicronixUploadDateFormat",
                                WarningUploadStudyShortCodes = "WarningUploadMicronixStudyShortCodes",
                                WarningSpecimenExists = "WarningMicronixSpecimenExists",
                                WarningUploadContainer = "WarningMicronixUploadContainer"))
  
  observeEvent(
    input$UploadMicronixAction,
    ({
      print("hi")
      # TRIGGER UI CHANGE FOR REACTIVITY - RECYCLE RENAMESTUDYLEADPERSON
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "@RBRLdB?GtnJ4kce")
      
      # PAUSE FOR EFFECT AND PRINT WORKING
      Sys.sleep(.75)
      output$UploadMicronixReturnMessage1 <- renderText({"Working..."})
    }))
  
  # UPLOAD SAMPLES
  observe({
    
    # WHEN REACTIVE UI IS CHANGED TO INDICATE AN UPLOAD
    if(input$RenameStudyLeadPerson == "@RBRLdB?GtnJ4kce"){
      
      # B. CHECK REQUIREMENTS
      UploadRequirements(input = input,
                         database = database, 
                         ui.input = list(UploadPlateID = "UploadMicronixPlateID",
                                         UploadDataSet = "UploadMicronixDataSet"))
      
      # C. UPLOAD SAMPLES
      sampleDB::UploadSamples(type = "micronix",
                              csv.upload = input$UploadDataSet$datapath,
                              container = input$UploadPlateID,
                              list.location = list(location_name = input$UploadLocation, 
                                                   level_I = input$UploadLocationMatrixLevelI, 
                                                   level_II = input$UploadLocationMatrixLevelII))
      
      # #UPDATE THE SEARCH DROPDOWNS
      warning("search dropdowns are not updated")
      # updateSelectizeInput(session = session,
      #                      "SearchByPlateID",
      #                      choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$plate_name),
      #                      label = NULL)
      
      # PRINT UPLOAD MSG
      output$UploadMicronixReturnMessage2 <- renderText({"Upload Complete"})
      
      # RESET UI VALUE
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "")
    }
  })
  
  # D. CLEAR FORM
  UploadReset(input = input, 
              output = output, 
              ref.clear_action = ref.clear_action,
              ui.input = list(UploadPlateID = "UploadMicronixPlateID",
                              UploadDataSet = "UploadMicronixDataSet",
                              UploadLocation = "UploadMicronixLocation",
                              UploadLocationLevel_I = "UploadMicronixLocation",
                              UploadLocationLevel_II = "UploadMicronixLocation",
                              UploadReturnMessage1 = "UploadMicronixReturnMessage1",
                              UploadReturnMessage2 = "UploadMicronixReturnMessage2"))
  
  # E. EXAMPLES
  UploadExamples(input = input,
                 database = database, 
                 output = output,
                 type = "micronix",
                 ui.output = list(ExampleUploadCSVNoDate = "ExampleMicronixUploadCSVNoDate",
                                 ExampleUploadCSVDate = "ExampleMicronixUploadCSVDate"))
}

CryoUpload <- function(session, output, input, database, ref.clear_action){

  # A. PERFORM UPLOAD CHECKS... PRINT GOOD USER MESSAGES
  UploadChecks(database = database,
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
      UploadRequirements(input = input,
                         database = database, 
                         ui.input = list(UploadPlateID = "UploadCryoPlateID",
                                         UploadDataSet = "UploadCryoDataSet"))
      
      # UPLOAD SAMPLES
      sampleDB::UploadSamples(type = "cryo",
                              csv.upload = input$UploadCryoSamples$datapath,
                              container = input$UploadBoxID,
                              list.location = list(location_name = input$UploadLocationCryoFreezerName, 
                                                   level_I = input$UploadLocationCryoLevelI, 
                                                   level_II = input$UploadLocationCryoLevelII))
      # 
      # #UPDATE THE SEARCH DROPDOWNS
      warning("search dropdowns are not updated")
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
  
  # A. PERFORM UPLOAD CHECKS... PRINT GOOD USER MESSAGES
  UploadChecks(database = database,
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
      UploadRequirements(input = input,
                         database = database,
                         ui.input = list(UploadPlateID = "UploadRDTPlateID",
                                         UploadDataSet = "UploadRDTDataSet"))
      
      # UPLOAD SAMPLES
      sampleDB::UploadSamples(type = "rdt",
                              csv.upload = input$UploadRDTSamples$datapath,
                              container = input$UploadBagID,
                              list.location = list(location_name = input$UploadLocationRDTFreezerName,
                                                   level_I = input$UploadLocationRDTLevelI,
                                                   level_II = input$UploadLocationRDTLevelII))

      #UPDATE THE SEARCH DROPDOWNS
      warning("search dropdowns are not updated")
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
      # UploadExamples(input = input,
      #                database = database, 
      #                output = output,
      #                type = "rdt",
      #                ui.output = list(ExampleUploadCSVNoDate = "ExampleUploadRDTCSVNoDate",
      #                                 ExampleUploadCSVDate = "ExampleUploadRDTCSVDate"))
      UploadExamples(input = input,
                     database = database,
                     output = output,
                     type = "rdt",
                     ui.output = list(ExampleUploadCSVNoDate = "ExampleUploadRDTCSVNoDate",
                                      ExampleUploadCSVDate = "ExampleUploadRDTCSVDate"))
}

PaperUpload <- function(session, output, input, database, ref.clear_action){
  
  # A. PERFORM UPLOAD CHECKS... PRINT GOOD USER MESSAGES
  UploadChecks(database = database,
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
      UploadRequirements(input = input,
                         database = database, 
                         ui.input = list(UploadPlateID = "UploadPaperPlateID",
                                         UploadDataSet = "UploadPaperDataSet"))
      
      # UPLOAD SAMPLES
      sampleDB::UploadSamples(type = "cryo",
                              csv.upload = input$UploadPaperSamples$datapath,
                              container = input$UploadBagID2,
                              list.location = list(location_name = input$UploadLocationPaperFreezerName, 
                                                   level_I = input$UploadLocationPaperLevelI, 
                                                   level_II = input$UploadLocationPaperLevelII))
      # 
      # #UPDATE THE SEARCH DROPDOWNS
      warning("search dropdowns are not updated")
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

UploadChecks <- function(database, type, input, output, ui.output, ui.input){
  
  # CHECK PLATE_ID IS UNIQUE
  CheckUploadContainerDuplication <- reactive({.CheckUploadContainerDuplication(input, database, ui.input)})
  output[[ui.output$WarningUploadContainer]] <- renderText(CheckUploadContainerDuplication())
  
  # CHECK THAT USR SPECIMEN TYPES ARE VALID
  CheckUploadSpecimenTypes <- reactive({.CheckUploadSpecimenTypes(input, database, ui.input)})
  output[[ui.output$WarningUploadSpecimenTypes]] <- renderText(CheckUploadSpecimenTypes())
  
  # CHECK THAT DATE IS IN CORRECT FORMAT
  CheckUploadDateFormat <- reactive({.CheckUploadDateFormat(input, database, ui.input)})
  output[[ui.output$WarningUploadDateFormat]] <- renderText(CheckUploadDateFormat())
  
  # CHECK THAT USR STUDY SHORT CODES ARE VALID
  CheckUploadStudyShortCode <- reactive({.CheckUploadStudyShortCodes(input, database, ui.input)})
  output[[ui.output$WarningUploadStudyShortCodes]] <- renderText(CheckUploadStudyShortCode())
  
  # CHECK THAT COLNAMES ARE CORRECT
  CheckUploadColnames <- reactive({.CheckUploadColnames(input, database, type, ui.input)})
  output[[ui.output$WarningUploadColnames]] <- renderText(CheckUploadColnames())
  
  # CHECK THAT BARCODES/ LABELS ARE NOT IN DATABASE
  CheckUploadPlateUniqSampleIDConstraint <- reactive({.CheckUploadPlateUniqSampleIDConstraint(input, database, type, ui.input)})
  output[[ui.output$WarningUploadSampleID]] <- renderText(CheckUploadPlateUniqSampleIDConstraint())
  
  # # CHECK THAT SPECIMEN TYPE IS UNIQUE
  # CheckSpecimenExists <- reactive({.CheckSpecimenExists(input, database, ui.input)})
  # output[[ui.output$WarningSpecimenExists]] <- renderText(CheckSpecimenExists())
}

UploadRequirements <- function(input, database, ui.input){
  
  # SET REQUIREMENTS
  out <- req(
    
    # - USER MUST UPLOAD A DATASET
    input[[ui.input$UploadDataSet]]$datapath,
    
    # - USER MUST UPLOAD A PLATE NAME
    input[[ui.input$UploadPlateID]],
    
    # - USER MUST UPLOAD LOCATION
    input[[ui.input$UploadLocation]],
    
    # # - DATASET BARCODES CANNOT BE IN DATABASE
    # !(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)),
    # 
    # # - PLATE NAME CANNOT BE IN DATABASE
    # !(input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name))
    )
  
  return(out)
  
}

UploadReset <- function(input, output, ui.input, ref.clear_action){
  observeEvent(
    input[[ref.clear_action]],
    ({
      reset(ui.input$UploadPlateID)
      reset(ui.input$UploadDataSet)
      reset(ui.input$UploadLocation)
      reset(ui.input$UploadLocationLevel_I)
      reset(ui.input$UploadLocationLevel_II)
      output[[ui.input$UploadReturnMessage1]] <- renderText({""})
      output[[ui.input$UploadReturnMessage2]] <- renderText({""})
    }))  
}

UploadExamples <- function(input, database, output, ui.output, type){
  output[[ui.output$ExampleUploadCSVNoDate]] <- renderPrint({.ExampleUploadCSVNoDate(database, type)})
  output[[ui.output$ExampleUploadCSVDate]] <- renderPrint({.ExampleUploadCSVDate(database, type)}) 
}

.CheckUploadContainerDuplication <- function(input, database, ui.input){
  if(input[[ui.input$UploadPlateID]] != ""){
    message("CHECK: UPLOAD CONTAINER NAME UNIQUENESS")
    toggle <- all(!(input[[ui.input$UploadPlateID]] %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name,
                                                           sampleDB::CheckTable(database = database, "box")$box_name,
                                                           sampleDB::CheckTable(database = database, "bag")$bag_name)))
    out <- validate(need(toggle,
                         "Container Names must be unique"))
  }else{
    out <- NULL
  }
  return(out)
}

.CheckUploadSpecimenTypes <- function(input, database, ui.input){
  
  if(!is.null(input[[ui.input$UploadDataSet]]$datapath)){
    message("CHECK: UPLOAD SPECIMEN TYPES EXISTS")
    specimen_types <- read_csv(input[[ui.input$UploadDataSet]]$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull(specimen_type)
    out <- validate(need(all(specimen_types %in% sampleDB::CheckTable(database = database, table = "specimen_type")$label), 
                         "Error: Specimen Type Not found... Consider creating a new specimen type"))
  }else{
    out <- NULL
  }
  return(out)
}

.CheckUploadDateFormat <- function(input, database, ui.input){
  
  if(!is.null(input[[ui.input$UploadDataSet]]$datapath)){
    message("CHECK: UPLOAD DATE FORMAT REQUIREMENTS")
    if("collection_date" %in% names(read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names=FALSE))){
      collection_dates <- read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% pull(collection_date)
      
      out <- validate(need(all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE), 
                           "Error: All Collection Dates are Not in YMD format"))
    }else{
      out <- NULL
    }
  }else{
    out <- NULL
  }
  return(out)
}

.CheckUploadStudyShortCodes <- function(input, database, ui.input){
  
  if(!is.null(input[[ui.input$UploadDataSet]]$datapath)){
    message("CHECK: UPLOAD STUDY CODE EXISTS")
    study_short_codes <- read_csv(input[[ui.input$UploadDataSet]]$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull(study_short_code)
    out <- validate(need(all(study_short_codes %in% sampleDB::CheckTable(database = database, table = "study")$short_code), 
                         "Error: Study Short Code Not found... Consider creating a new study"))
  }else{
    out <- NULL
  }
  return(out)
}

.CheckUploadColnames <- function(input, database, type, ui.input){
  
  names.base <- c("study_subject_id", "specimen_type", "study_short_code")
  
  if(!is.null(input[[ui.input$UploadDataSet]]$datapath)){
    
    message("CHECK: UPLOAD CSV COLUMN NAMES REQUIREMENTS")    
    upload_names <- read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% names()
    
    if("Tube ID" %in% names(input[[ui.input$UploadDataSet]]$datapath)){
      names.traxer.nodate <- c(names.base, "Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date")
      names.traxer.date <- c(names.base, "Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "collection_date")
      out <- validate(need(setequal(names.traxer.nodate, upload_names) || setequal(names.traxer.date, upload_names)), 
                           "Error: Malformed Colnames")
    }
    else if("TubeCode" %in% names(input[[ui.input$UploadDataSet]]$datapath)){
      names.visionmate.nodate <- c(names.base, "LocationRow", "LocationColumn", "TubeCode")
      names.visionmate.date <- c(names.base, "LocationRow", "LocationColumn", "TubeCode", "collection_date")
      out <- validate(need(setequal(names.visionmate.nodate, upload_names) || setequal(names.visionmate.date, upload_names)),
                           "Error: Malformed Colnames")
    }
    else if(type == "rdt" || type == "paper"){
      out <- validate(need(setequal(c(names.base, "label"), upload_names) || setequal(c(names.base, "label", "collection_date"), upload_names)),
                      "Error: Malformed Colnames")
    }
    else{
      out <- validate(need(setequal(c(names.base, "row", "column", "label"), upload_names) || setequal(c(names.base, "row", "column", "label", "collection_date"), upload_names)),
                      "Error: Malformed Colnames")
    }
  }else{
    out <- NULL
  }
  
  return(out)
}

.CheckUploadPlateUniqSampleIDConstraint <- function(input, database, type, ui.input){
  
  if(!is.null(input[[ui.input$UploadDataSet]]$datapath)){
    message("CHECK: BARCODE/LABEL UNIQUENESS")
    
    if(type == "micronix"){
      if("TubeCode" %in% names(read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names=FALSE))){
        upload_barcodes <- read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% pull(TubeCode)
      }else{
        upload_barcodes <- read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% pull("Tube ID")
      }
      barcodes.existing <- upload_barcodes[which(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))]
      out <- validate(need(all(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))), 
                           paste("Error: Unique Barcode Constraint", barcodes.existing)))
   }
   else{
     labels <- read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% pull(TubeCode)
     out <- validate(need(all(!(labels %in% c(sampleDB::CheckTable(database = database, "tube")$labels,
                                              sampleDB::CheckTable(database = database, "rdt")$labels,
                                              sampleDB::CheckTable(database = database, "paper")$labels))), 
                          paste("Error: Unique Label Constraint", barcodes.existing)))
   }
  }else{
    out <- NULL
  }
  return(out)
}

.CheckSpecimenExists <- function(input, database, ui.input){
  
  toggle <- TRUE
  if(!is.null(input[[ui.input$UploadDataSet]]$datapath)){
    message("CHECK: STUDY SUBJECT LONGITUDINAL REQUIREMENTS")
    csv <- read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names = F) %>% tidyr::drop_na()
    
    check.study_subject <- inner_join(CheckTable(database = database, "study_subject"),
                                      tibble(subject = csv$"study_subject_id", 
                                             study_id = filter(CheckTable(database = database, "study"), short_code %in% csv$study_short_code)$id, 
                                             specimen_type_id = filter(CheckTable(database = database, "specimen_type"), label %in% csv$specimen_type)$id),
                                     by = c("subject", "study_id"))
    
    if(nrow(check.study_subject) != 0){
      
      if("collection_date" %in% names(csv.upload)){
        test_table.specimen <- check.study_subject %>% rename("study_subject_id" = "id")
        test_table.specimen$collection_date <- csv.upload$collection_date
      }else{
        test_table.specimen <- check.study_subject %>% rename("study_subject_id" = "id")
        test_table.specimen$collection_date <- NA
      } 
      
      #CHECK IF SPECIMEN ALREADY EXISTS
      check.specimen <- inner_join(sampleDB::CheckTable(database = database, "specimen"), 
                                       test_table.specimen,
                                       by = c("study_subject_id", "specimen_type_id", "collection_date"))
      
      if(nrow(check.specimen) > 0){
        toggle <- FALSE
      }
    }
    
    out <- validate(need(toggle, "Error: Specimen already exists in the database. Specimen Unique Constraint: SUBJECT + STUDY + SPECIMEN TYPE + COLLECTION DATE"))
    
  }else{
    out <- NULL
  }
  return(out)
}

.CheckUploadPlateUniqSampleIDConstraintFileInput <- function(input, database, ui.input){
  
  if(!is.null(input$UploadDataSet$datapath)){
    if("TubeCode" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
      upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull(TubeCode)
    }else{
      upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull("Tube ID")
    }
    
    # upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$TubeCode
    toggle <- upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)
    
    out <- shinyFeedback::feedbackWarning("UploadDataSet", toggle, "Failed: Barcode Unique Constraint")
  }else{
    out <- NULL
  }
}

.ExampleUploadCSVNoDate <- function(database, type){
  if(type == "micronix"){
    tibble(LocationRow = rep("A", 10),
           LocationColumn = c(1:10),
           TubeCode = paste0("XXX", 1:10),
           study_subject_id = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_short_code = "KAM06") %>% as.data.frame() 
  }
  else if(type == "cryo"){
    tibble(row = 1:10,
           column = 1:10,
           label = c("A","B","C","D","E","F","G","H","I","J"),
           study_subject = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_code = "KAM06") %>% as.data.frame()
  }
  else{
    tibble(label = c("A","B","C","D","E","F","G","H","I","J"),
           study_subject = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_code = "KAM06") %>% as.data.frame()
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
           collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% as.data.frame() 
  }
  else if(type == "cryo"){
    tibble(row = 1:10,
           column = 1:10,
           label = c("A","B","C","D","E","F","G","H","I","J"),
           study_subject = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_code = "KAM06",
           collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% as.data.frame()
  }
  else{
    tibble(label = c("A","B","C","D","E","F","G","H","I","J"),
           study_subject = paste0("subject_", 1:10),
           specimen_type = "PLASMA",
           study_code = "KAM06",
           collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% as.data.frame()
  }
}