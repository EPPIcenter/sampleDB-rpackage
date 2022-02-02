
#NEED TO DROP NAs AND NA-LIKE ROWS (IE AS.CHAR(NA))... CAN PROBABLY DO SO DIRECTLY IN READ_CSV
helper.CheckUploadPlateDuplication <- function(input, database){
  if(input$UploadPlateID != ""){
    message("CHECK: UPLOAD PLATE NAME UNIQUENESS")
    toggle <- input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)
    out <- shinyFeedback::feedbackWarning("UploadPlateID", toggle, "Plate IDs must be unique")
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckUploadColnames <- function(input, database){
  
  names.traxer.nodate <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "study_subject_id", "specimen_type", "study_short_code") 
  names.traxer.date <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "study_subject_id", "specimen_type", "study_short_code", "collection_date")
  names.visionmate.nodate <- c("LocationRow", "LocationColumn", "TubeCode", "study_subject_id", "specimen_type", "study_short_code")
  names.visionmate.date <- c("LocationRow", "LocationColumn", "TubeCode", "study_subject_id", "specimen_type", "study_short_code", "collection_date")
  
  if(!is.null(input$UploadDataSet$datapath)){
    message("CHECK: UPLOAD CSV COLUMN NAMES REQUIREMENTS")
    if("TubeCode" %in% names(read.csv(input$UploadDataSet$datapath, check.names=FALSE))){
      upload_names <- read.csv(input$UploadDataSet$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% names()
    }else{
      upload_names <- read.csv(input$UploadDataSet$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% names()
    }
    
    # print(upload_names)
    
    out <- validate(need(all(names.traxer.nodate %in% upload_names) || all(names.traxer.date %in% upload_names) || all(names.visionmate.nodate %in% upload_names) || all(names.visionmate.date %in% upload_names), 
                         "Failed: Malformed Colnames"))
  }else{
    out <- NULL
  }
  
  return(out)
}

# COULD BE TESTED MORE
helper.CheckUploadDateFormat <- function(input, database){
  
  if(!is.null(input$UploadDataSet$datapath)){
    message("CHECK: UPLOAD DATE FORMAT REQUIREMENTS")
    if("collection_date" %in% names(read.csv(input$UploadDataSet$datapath, check.names=FALSE))){
      collection_dates <- read.csv(input$UploadDataSet$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% pull(collection_date)
      
      out <- validate(need(all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE), 
                           "Failed: All Collection Dates are Not in YMD format"))
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
    message("CHECK: UPLOAD SPECIMEN TYPES EXISTS")
    specimen_types <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull(specimen_type)
    out <- validate(need(all(specimen_types %in% CheckTable(database = database, table = "specimen_type")$label), 
                         "Failed: Specimen Type Not found... Consider creating a new specimen type"))
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckUploadStudyShortCodes <- function(input, database){
  
  if(!is.null(input$UploadDataSet$datapath)){
    message("CHECK: UPLOAD STUDY CODE EXISTS")
    stud_short_codes <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull(study_short_code)
    out <- validate(need(all(stud_short_codes %in% CheckTable(database = database, table = "study")$short_code), 
                         "Failed: Study Short Code Not found... Consider creating a new study"))
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckUploadPlateUniqBarcodeConstraint <- function(input, database){

  if(!is.null(input$UploadDataSet$datapath)){
    message("CHECK: BARCODE UNIQUENESS")
    if("TubeCode" %in% names(read.csv(input$UploadDataSet$datapath, check.names=FALSE))){
      upload_barcodes <- read.csv(input$UploadDataSet$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% pull(TubeCode)
    }else{
      upload_barcodes <- read.csv(input$UploadDataSet$datapath, check.names=FALSE) %>% tidyr::drop_na() %>% pull("Tube ID")
    }
    barcodes.existing <- upload_barcodes[which(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))]
    out <- validate(need(all(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))), 
                         paste("Failed: Barcode Unique Constraint", barcodes.existing)))
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

helper.CheckStudySubjectLongitudinal <- function(input, database){

  toggle <- TRUE
  if(!is.null(input$UploadDataSet$datapath)){
    message("CHECK: STUDY SUBJECT LONGITUDINAL REQUIREMENTS")
    csv <- read.csv(input$UploadDataSet$datapath, check.names = F)
    tmp_table.study_subject <- inner_join(CheckTable(database = database, "study_subject")[, c("uid", "study_id")],
                                          tibble(uid = csv$"study_subject_id", study_id = filter(CheckTable(database = database, "study"), short_code %in% csv$study_short_code)$id, short_code = csv$"study_short_code"),
                                          by = c("uid", "study_id"))
    if(nrow(tmp_table.study_subject) > 0){
      if(!("collection_date" %in% names(csv))){
        toggle <- FALSE
      }
      if(NA %in% inner_join(tmp_table.study_subject %>% distinct(), csv, by = c("uid" = "study_subject_id", "short_code" = "study_short_code"))$collection_date)(
        toggle <- FALSE
      )
    }
    
    out <- validate(need(toggle, "Failed: Study Subject found in database. Longitudinal information required"))
    
  }else{
    out <- NULL
  }
  return(out)
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
    
    # # - USER MUST SUPPLY A STUDY CODE
    # input$UploadStudyShortCode,
    
    # - DATASET BARCODES CANNOT BE IN DATABASE
    !(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)),
    
    # - PLATE NAME CANNOT BE IN DATABASE
    !(input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)))
  
  return(out)
  
}
