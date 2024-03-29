.ExampleLogisticsItems <- function(database, type){
  tibble(Row = "A",
         Column = "1",
         `Barcode` = "974019283")
}

.ExampleMetadataItems <- function(database, type){
  tibble(`StudyCode` = "CodeXXX",
         `StudySubject` = "StudySubjectXXX",
         `SpecimenType` = "Plasma",
         `CollectionDate` = "2022-04-11") 
}

.ExampleInDatabasePlateOne <- function(database, type){
  tibble(Row = c("A","A","A"),
         Column = c("1","2","3"),
         `Barcode` = c("ItemA","ItemB","ItemC"))
}

.ExampleInDatabasePlateTwo <- function(database, type){
  tibble(Row = c("A","A","A"),
         Column = c("1","2","3"),
         `Barcode` = c("ItemD","ItemE","ItemF"))
}

.ExamplePlateOneMove <- function(database, type){
  tibble(Row = c("A","A","A"),
         Column = c("1","2","3"),
         `Barcode` = c("974019283","974019284","974019285"))
}

.ExamplePlateTwoMove <- function(database, type){
  tibble(Row = c("A","A","A"),
         Column = c("1","2","3"),
         `Barcode` = c("974019286","974019287","974019288"))
}

.ExampleCombinedItems <- function(database, type){
  tibble(Row = "A",
         Column = "1",
         `Barcode` = "974019283",
         `StudyCode` = "CodeXXX",
         `StudySubject` = "StudySubjectXXX",
         `SpecimenType` = "Plasma",
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