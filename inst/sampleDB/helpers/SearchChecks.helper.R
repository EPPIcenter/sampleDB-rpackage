

#need to verify checks in this file
helper.CheckSubjectBarcodeFileColnames <- function(input, database){
  if(!is.null(input$SearchByBarcode$datapath)){
    
    names.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% colnames()
    ncols.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% ncol()
    
    toggle <- ncols.barcode_file == 1 & names.barcode_file == "barcode"
    
    out <- validate(need(toggle, "Failed: Barcode File is Malformed"))
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckSubjectBarcodeFileColnames2 <- function(input, database){
  if(!is.null(input$SearchByBarcode$datapath)){
    
    names.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% colnames()
    ncols.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% ncol()
    
    toggle <- !(ncols.barcode_file == 1 & names.barcode_file == "barcode")
    
    out <- shinyFeedback::feedbackWarning("SearchByBarcode", toggle, "Failed: Barcode File is Malformed")
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckSubjectUIDFileColnames <- function(input, database){
  
  if(!is.null(input$SearchBySubjectUIDFile$datapath)){
    
    names.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% colnames()
    ncols.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% ncol()
    
    toggle <- ncols.subject_uid_file == 1 & names.subject_uid_file == "subject_uid"
    out <- validate(need(toggle, "Failed: Subject UID File is Malformed"))
  }else{
    out <- NULL
  }
  return(out)
}

helper.CheckSubjectUIDFileColnames2 <- function(input, database){
  if(!is.null(input$SearchBySubjectUIDFile$datapath)){
    
    names.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% colnames()
    ncols.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% ncol()
    
    toggle <- !(ncols.subject_uid_file == 1 & names.subject_uid_file == "subject_uid")
    
    out <- shinyFeedback::feedbackWarning("SearchBySubjectUIDFile", toggle, "Failed: Subject UID File is Malformed")
  }else{
    out <- NULL
  }
  return(out)
}


# STUFF BELOW SHOULD REALLY BE PUT ELSEWHERE
helper.SubsetPlateNames <- function(input, database){
   study_ref_id <- filter(sampleDB::CheckTable(database = database, "study"), short_code %in% input$SearchByStudy)$id
   study_subject_ref_id <- filter(sampleDB::CheckTable(database = database, "study_subject"), study_id %in% study_ref_id)$id
   specimen_ref_id <- filter(sampleDB::CheckTable(database = database, "specimen"), study_subject_id %in% study_subject_ref_id)$id
   storage_container_id <- filter(sampleDB::CheckTable(database = database, "storage_container"), specimen_id %in% specimen_ref_id)$id
   matrix_tube_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), id %in% storage_container_id)$id
   
   plate_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), id %in% matrix_tube_ids)$plate_id %>% unique()
   plate_names <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), id %in% plate_ids)$uid
   return(plate_names)
 }


