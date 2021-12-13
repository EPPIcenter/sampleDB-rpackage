#' @export

UploadSamples <- function(){}

traxer_csv <- read_csv("~/eppicenter/library/R/shiny/sampleDB/files/traxer_data_example.csv") %>%
  drop_na() %>%
  mutate(barcode = `Tube ID`,
         well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
  select(-c(Position:Date))
  #need to add freezer info (entry per tube)
  #need to add tube uid info (entry per tube)
  #need to add study stubject (single entry)
  #need to add plate uid info (single entry)
  #??? need to add study subject uid (entry per tube)

CheckTable("matrix_tube") %>% head(2)
  #contains "plate_id"

  #for each tube in upload_csv
  sampleDB::AddToTable("matrix_tube",
                       list(plate_id = 123,
                            barcode = "barcode1",
                            well_position = "A01"))

CheckTable("matrix_plate") %>% head(2)
  #contains "uid" (autogeneration option?)
  #contains "location_id"
  sampleDB::AddToTable("matrix_plate",
                       list(created = "dummy",
                            last_updated = "dummy",
                            uid = "abc",
                            hidden = 0,
                            location_id = 1))

CheckTable("study_subject") %>% head(2)
  #contains "uid"
  #contains "study_id"

CheckTable("specimen") %>% head(2)
  #contains "study_subject_id"
  #contains "specimen_type_id"
