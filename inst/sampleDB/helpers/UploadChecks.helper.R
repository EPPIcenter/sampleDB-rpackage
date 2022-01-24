
#NEED TO DROP NAs AND NA-LIKE ROWS (IE AS.CHAR(NA))... CAN PROBABLY DO SO DIRECTLY IN READ_CSV
helper.CheckUploadPlateDuplication <- function(input, database){
  toggle <- input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)
  out <- shinyFeedback::feedbackWarning("UploadPlateID", toggle, "Plate IDs must be unique")
  return(out)
}

helper.CheckUploadColnames <- function(input, database){
  
  # VALID COLNAMES
  names.traxer.nodate <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "study_subject_id", "specimen_type") 
  names.traxer.date <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "study_subject_id", "specimen_type", "collection_date")
  names.visionmate.nodate <- c("LocationRow", "LocationColumn", "TubeCode", "study_subject_id", "specimen_type")
  names.visionmate.date <- c("LocationRow", "LocationColumn", "TubeCode", "study_subject_id", "specimen_type", "collection_date")
  
  if(!is.null(input$UploadDataSet$datapath)){
    if("TubeCode" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
      upload_names <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% names()
    }else{
      upload_names <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% names()
      print(read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na())
    }
    
    out <- validate(need(identical(upload_names, names.traxer.nodate) || identical(upload_names, names.traxer.date) || identical(upload_names, names.visionmate.nodate) || identical(upload_names, names.visionmate.date), "Failed: Malformed Colnames"))
  }else{
    out <- NULL
  }
  
  return(out)
}

# COULD BE TESTED MORE
helper.CheckUploadDateFormat <- function(input, database){

    if(!is.null(input$UploadDataSet$datapath)){
      if("collection_date" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
        collection_dates <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% drop_na %>% pull(collection_date)
        
        out <- validate(need(all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE), "Failed: All Collection Dates are Not in YMD format"))
      }else{
          out <- NULL
      }
    }else{
      out <- NULL
    }
  
  return(out)
}

helper.CheckUploadSpecimenTypes <- function(input, database){
    if(!is.null(input$UploadDataSet$datapath)){
      specimen_types <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull(specimen_type)
      print(specimen_types)
      out <- validate(need(all(specimen_types %in% CheckTable(database = database, table = "specimen_type")$label), "Failed: Specimen Type Not found"))
    }else{
      out <- NULL
    }
  return(out)
}

helper.CheckUploadPlateUniqBarcodeConstraint <- function(input, database){
    if(!is.null(input$UploadDataSet$datapath)){
      if("TubeCode" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
        upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull(TubeCode)
      }else{
        upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull("Tube ID")
      }
      out <- validate(need(all(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))), "Failed: Barcode Unique Constraint"))
    }else{
      out <- NULL
    }
  return(out)
}

CheckUploadPlateUniqBarcodeConstraintFileInput <- function(input, database){
  
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

UploadRequirements <- function(input, database){
  
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
