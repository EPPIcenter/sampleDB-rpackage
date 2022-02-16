MoveReset <- function(input, output){
  observeEvent(
  input$ClearMoveForm,
  ({
    reset("MoveDataSet")
    output$MoveReturnMessage1 <- renderText({""})
    output$MoveReturnMessage2 <- renderText({""})}))
  }
MoveChecks <- function(input, database, output){
  # CHECK THAT COLNAMES ARE FORMED CORRECTLY
  CheckMoveColnames <- reactive({helper.CheckMoveColnames(input, database)})
  output$WarningMoveColnames <- renderText(CheckMoveColnames()) 
}

#################################################################################

helper.CheckMoveColnames <- function(input, database){
  
  # VALID COLNAMES
  names.traxer <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date") 
  names.visionmate <- c("LocationRow", "LocationColumn", "TubeCode")
  
  if(!is.null(input$MoveDataSet)){
    
    list.move <- list()
    for(i in 1:length(input$MoveDataSet[,1])){
      plate.name <- input$MoveDataSet[[i, 'name']] %>% gsub("\\.csv","",.)
      list.move[[plate.name]] <- read_csv(input$MoveDataSet[[i, 'datapath']], col_types = cols()) %>% tidyr::drop_na()
    }
    
    toggle <- TRUE
    for(lst.names in names(list.move)){
      name.col <- names(list.move[[lst.names]])
      if(!(identical(name.col, names.traxer) || identical(name.col, names.visionmate))){
        toggle <- FALSE
      }
    }
  

    upload_names <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% names()
    out <- validate(need(toggle, "Failed: Malformed Colnames"))
  }else{
    out <- NULL
  }
  
  return(out)
}

MoveRequirements <- function(input, database){
  
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

MoveExamples <- function(input, database, output){
  # MOVE EXAMPLES
  output$ExampleMoveSamplesCSV <- renderPrint({helper.ExampleMoveCSVDate(database)}) 
}

################################################################################

helper.ExampleMoveCSVDate <-  function(database){
  tibble(LocationRow = rep("A", 10),
         LocationColumn = c(1:10),
         TubeCode = CheckTable(database = database, "matrix_tube")$barcode %>% head(10)) %>% as.data.frame()
}