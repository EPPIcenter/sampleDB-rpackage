
MoveWetlabSamples <- function(session, input, database, output){
  
  # Run Checks
  .MoveChecks(input,database, output)
  
  observeEvent(
    input$MoveAction,
    ({
      
      # TRIGGER UI CHANGE FOR REACTIVITY - RECYCLE RENAMESTUDYLEADPERSON
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "a6sFH$DKdsbgGLY9")
      
      # PAUSE FOR EFFECT AND PRINT WORKING
      Sys.sleep(.75)
      output$MoveReturnMessage1 <- renderText({"Working..."})
      
    }))
  
  # UPLOAD SAMPLES
  observe({
    
    # WHEN REACTIVE UI IS CHANGED TO INDICATE AN UPLOAD
    if(input$RenameStudyLeadPerson == "a6sFH$DKdsbgGLY9"){
      
      # CHECK REQUIREMENTS
      # .MoveRequirements(input, database)
      
      #CREATE LIST FOR MOVE
      move_data_list <- list()
      for(i in 1:length(input$MoveDataSet[,1])){
        plate.name <- input$MoveDataSet[[i, 'name']] %>% gsub("\\.csv","",.)
        move_data <- read.csv(input$MoveDataSet[[i, 'datapath']])
        if(input$MoveSampleType == "micronix" && !"LocationRow" %in% names(move_data)){
          names(move_data) <- move_data[1,]
          move_data <- move_data[-1,]
        }
        move_data_list[[plate.name]] <- move_data
      }
      
      # MOVE SAMPLES -- FUN INPUT SHOULD PROBABLY BE A LIST OF FILES
      sampleDB::MoveSamples(sample_type = input$MoveSampleType,
                            move_data = move_data_list)
      
      # PRINT UPLOAD MSG
      output$MoveReturnMessage2 <- renderText({"Successfully Moved Samples"})
      
      # RESET UI VALUE
      updateTextInput(session = session, "RenameStudyLeadPerson", value = "")
    }
  })
  
  # CLEAR FORM
  .MoveReset(input, output)
  
  # EXAMPLES
  .MoveExamples(input, database, output)
}


.MoveReset <- function(input, output){
  observeEvent(
  input$ClearMoveForm,
  ({
    shinyjs::reset("MoveDataSet")
    output$MoveReturnMessage1 <- renderText({""})
    output$MoveReturnMessage2 <- renderText({""})}))
}

.MoveChecks <- function(input, database, output){
  # CHECK THAT COLNAMES ARE FORMED CORRECTLY
  CheckMoveColnames <- reactive({helper.CheckMoveColnames(input, database)})
  output$WarningMoveColnames <- renderText(CheckMoveColnames()) 
}

.MoveExamples <- function(input, database, output){
  # MOVE EXAMPLES
  output$ExampleMoveSamplesCSV <- renderPrint({helper.ExampleMoveCSVDate(database)}) 
}

.MoveRequirements <- function(input, database){
  
  # READ IN CSV FOR UNIQUE BARCODE CHECK
  csv <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na()
  
  # - GET BARCODES
  if("TubeCode" %in% names(csv)){
    upload_barcodes <- csv$TubeCode
  }else{
    upload_barcodes <- csv$`Tube ID`
  }
  
  # SET REQUIREMENTS
  out <- req(
    
    # - USER MUST UPLOAD A DATASET
    input$UploadDataSet$datapath,
    
    # - USER MUST UPLOAD A PLATE NAME
    input$UploadPlateID,
    
    # - USER MUST UPLOAD LOCATION
    input$UploadLocation,
    
    # - USER MUST SUPPLY A STUDY CODE
    input$UploadStudyShortCode,
    
    # - DATASET BARCODES CANNOT BE IN DATABASE
    !(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)),
    
    # - PLATE NAME CANNOT BE IN DATABASE
    !(input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)))
  
  return(out)
  
}

helper.CheckMoveColnames <- function(input, database){
  
  # VALID COLNAMES
  names.traxer <- c("Position", "Tube ID")
  names.visionmate <- c("LocationRow", "LocationColumn", "TubeCode")
  
  if(!is.null(input$MoveDataSet)){
    
    list.move <- list()
    for(i in 1:length(input$MoveDataSet[,1])){
      plate.name <- input$MoveDataSet[[i, 'name']] %>% gsub("\\.csv","",.)
      move_data <- read.csv(input$MoveDataSet[[i, 'datapath']])
      if(!"LocationRow" %in% names(move_data)){
        names(move_data) <- move_data[1,]
        move_data <- move_data[-1,]
      }
      list.move[[plate.name]] <- move_data
    }
    
    toggle <- TRUE
    for(lst.names in names(list.move)){
      if(!(all(names.traxer %in% names(list.move[[lst.names]])) || all(names.visionmate %in% names(list.move[[lst.names]])))){
        toggle <- FALSE
      }
    }

    out <- validate(need(toggle, "Failed: Malformed Colnames"))
  }else{
    out <- NULL
  }
  
  return(out)
}

helper.ExampleMoveCSVDate <-  function(database){
  tibble(LocationRow = rep("A", 10),
         LocationColumn = c(1:10),
         TubeCode = CheckTable(database = database, "matrix_tube")$barcode %>% head(10)) %>% print.data.frame(row.names = FALSE)
}