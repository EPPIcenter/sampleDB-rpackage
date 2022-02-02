#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export

#REFORMAT CSV -- IF LOCATIONROW IS A COLUMN THEN THE DATA CAME OFF VISIONMATE
ReformatUploadCSV <- function(csv.upload){

  if(!("LocationRow" %in% names(csv.upload))){
    csv.upload.reformatted <- drop_na(csv.upload) %>%
      mutate(barcode = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))
    message("UploadCSV from Traxer detected...")
  }else{
    csv.upload.reformatted <- drop_na(csv.upload) %>%
      mutate(barcode = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))
    message("UploadCSV from VisionMate detected...")
  }
  
  return(csv.upload.reformatted)
}