

# Micronix Upload File Checks
.CheckColnamesOfUserProvidedMicronixFileFormat <- function(input, users_upload_file, sample_type){

  #get ui elements to know UploadFileType
  ui_elements <- GetUIElements(sample_type)
  
  #establish required metadata column names
  names.base <- c("Participant", "SpecimenType", "StudyCode")
  
  if(input[[ui_elements$ui.input$UploadFileType]] == "visionmate"){
    required_visionmate_colnames <- c(names.base, "LocationRow", "LocationColumn", "TubeCode")
    visionmate_colnames_withdate <- c(required_visionmate_colnames, "CollectionDate")
    out <- all(required_visionmate_colnames %in% names(users_upload_file)) || all(visionmate_colnames_withdate %in% names(users_upload_file))
    # out1 <- validate(need(all(required_visionmate_colnames %in% names(users_upload_file)) || all(visionmate_colnames_withdate %in% names(users_upload_file)),
    #                       paste0("Error: Malformed Colnames\nVisionMate Column Names: LocationRow, LocationColumn, TubeCode\nDetected Column Names: ", names(users_upload_file))))
  }
  else if(input[[ui_elements$ui.input$UploadFileType]] == "traxcer"){
    users_upload_file <- users_upload_file %>% setNames(.[1,]) %>% .[-1,]
    required_traxcer_colnames <- c(names.base, "Position", "Tube ID")
    traxcer_colnames_withdate <- c(required_traxcer_colnames, "CollectionDate")
    out <- all(required_traxcer_colnames %in% names(users_upload_file)) || all(traxcer_colnames_withdate %in% names(users_upload_file))
    # out1 <- validate(need(all(required_traxcer_colnames %in% names(users_upload_file)) || all(traxcer_colnames_withdate %in% names(users_upload_file)),
    #                      "Error: Malformed Colnames\nRequired Traxcer Column Names are: Position, Tube ID"))
  }
  else{
    general_colnames <- c(names.base, "MicronixBarocde", "Row", "Column")
    out <- all(general_colnames %in% names(users_upload_file))
    # out1 <- validate(need(all(general_colnames %in% names(users_upload_file)),
    #                      "Error: Malformed Colnames\nRequired Traxcer Column Names are: MicronixBarocde, Row, Column"))
  }
  return(out)
}

.CheckUploadSpecimenTypes <- function(database, formatted_upload_file){

  specimen_types <- formatted_upload_file %>% pull(specimen_type)
  out <- validate(need(all(specimen_types %in% sampleDB::CheckTable(database = database, table = "specimen_type")$label),
                       "Error: Specimen Type Not found... Consider creating a new specimen type"))
  return(out)
}

.CheckUploadStudyShortCodes <- function( database, formatted_upload_file){
  
  study_short_codes <- formatted_upload_file %>% pull(study_short_code)
  out <- validate(need(all(study_short_codes %in% sampleDB::CheckTable(database = database, table = "study")$short_code), 
                       "Error: Study Short Code Not found... Consider creating a new study"))

  return(out)
}

.CheckUploadDateFormat <- function(database, formatted_upload_file){
  
  if("collection_date" %in% names(formatted_upload_file)){
    collection_dates <- formatted_upload_file %>% pull(collection_date)
    out <- validate(need(all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE), 
                         "Error: All Collection Dates are Not in YMD format"))
  }else{
    out <- NULL
  }
  return(out)
}

.CheckBarcodeIsntInDB <- function(database, formatted_upload_file){
  
    upload_barcodes <- formatted_upload_file %>% pull(label)
    barcodes.existing <- upload_barcodes[which(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))]
    out <- validate(need(all(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))), 
                         paste("Error: Unique Barcode Constraint", barcodes.existing)))
  # }
  # else{
  #   labels <- formatted_upload_file %>% pull(TubeCode)
  #   out <- validate(need(all(!(labels %in% c(sampleDB::CheckTable(database = database, "tube")$labels,
  #                                            sampleDB::CheckTable(database = database, "rdt")$labels,
  #                                            sampleDB::CheckTable(database = database, "paper")$labels))), 
  #                        paste("Error: Unique Label Constraint", barcodes.existing)))
  # }
  return(out)
}

.CheckBarcodeArentRepeated <- function(database, formatted_upload_file){
  
  upload_barcodes <- formatted_upload_file %>% pull(label)
  dups <- upload_barcodes[duplicated(upload_barcodes)]
  out <- validate(need(length(dups) == 0, 
                       paste("Error: Unique Barcode Constraint", dups))) 
  # }
  # else{
  #   labels <- formatted_upload_file %>% pull(TubeCode)
  #   out <- validate(need(all(!(labels %in% c(sampleDB::CheckTable(database = database, "tube")$labels,
  #                                            sampleDB::CheckTable(database = database, "rdt")$labels,
  #                                            sampleDB::CheckTable(database = database, "paper")$labels))), 
  #                        paste("Error: Unique Label Constraint", barcodes.existing)))
  # }
  return(out)
}

# Plate Checks
.CheckUploadContainerNameDuplication <- function(input, database, ui.input){
  
  toggle <- all(!(input[[ui.input$UploadPlateID]] %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name,
                                                         sampleDB::CheckTable(database = database, "box")$box_name,
                                                         sampleDB::CheckTable(database = database, "bag")$bag_name)))
  out <- validate(need(toggle, "Plate name is not unique"))
  return(out)
}

.CheckUploadContainerBarcodeDuplication <- function(input, database, ui.input){

  if(input[[ui.input$UploadPlateBarcode]] != ""){
    toggle <- all(!(input[[ui.input$UploadPlateBarcode]] %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_barcode)))
    # toggle <- all(!(input[[ui.input$UploadPlateBarcode]] %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_barcode,
    #                                                              sampleDB::CheckTable(database = database, "box")$box_barcode,
    #                                                              sampleDB::CheckTable(database = database, "bag")$bag_barcode))) 
    out <- validate(need(toggle, "Plate barcode is not unique"))
  }else{
    out <- NULL
  }
  return(out)
}

# .CheckSpecimenExists <- function(input, database, ui.input){
#   
#   toggle <- TRUE
#   if(!is.null(input[[ui.input$UploadDataSet]]$datapath)){
#     message("CHECK: STUDY SUBJECT LONGITUDINAL REQUIREMENTS")
#     csv <- read.csv(input[[ui.input$UploadDataSet]]$datapath, check.names = F) %>% tidyr::drop_na()
#     
#     check.study_subject <- inner_join(CheckTable(database = database, "study_subject"),
#                                       tibble(subject = csv$"study_subject_id", 
#                                              study_id = filter(CheckTable(database = database, "study"), short_code %in% csv$study_short_code)$id, 
#                                              specimen_type_id = filter(CheckTable(database = database, "specimen_type"), label %in% csv$specimen_type)$id),
#                                      by = c("subject", "study_id"))
#     
#     if(nrow(check.study_subject) != 0){
#       
#       if("collection_date" %in% names(csv.upload)){
#         test_table.specimen <- check.study_subject %>% rename("study_subject_id" = "id")
#         test_table.specimen$collection_date <- csv.upload$collection_date
#       }else{
#         test_table.specimen <- check.study_subject %>% rename("study_subject_id" = "id")
#         test_table.specimen$collection_date <- NA
#       } 
#       
#       #CHECK IF SPECIMEN ALREADY EXISTS
#       check.specimen <- inner_join(sampleDB::CheckTable(database = database, "specimen"), 
#                                        test_table.specimen,
#                                        by = c("study_subject_id", "specimen_type_id", "collection_date"))
#       
#       if(nrow(check.specimen) > 0){
#         toggle <- FALSE
#       }
#     }
#     
#     out <- validate(need(toggle, "Error: Specimen already exists in the database. Specimen Unique Constraint: SUBJECT + STUDY + SPECIMEN TYPE + COLLECTION DATE"))
#     
#   }else{
#     out <- NULL
#   }
#   return(out)
# }

# .CheckUploadPlateUniqSampleIDConstraintFileInput <- function(input, database, ui.input){
#   
#   if(!is.null(input$UploadDataSet$datapath)){
#     if("TubeCode" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
#       upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull(TubeCode)
#     }else{
#       upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% tidyr::drop_na() %>% pull("Tube ID")
#     }
#     
#     # upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$TubeCode
#     toggle <- upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)
#     
#     out <- shinyFeedback::feedbackWarning("UploadDataSet", toggle, "Failed: Barcode Unique Constraint")
#   }else{
#     out <- NULL
#   }
# }
