



helper.ExampleUploadCSVDate <-  function(database){
  tibble(LocationRow = rep("A", 10),
         LocationColumn = c(1:10),
         TubeCode = CheckTable(database = database, "matrix_tube")$barcode %>% head(10),
         study_subject_id = CheckTable(database = database, "study_subject")$uid %>% head(10),
         specimen_type = "PLASMA",
         collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% as.data.frame()
}

helper.ExampleUploadCSVNoDate <- function(database){
  tibble(LocationRow = rep("A", 10),
         LocationColumn = c(1:10),
         TubeCode = CheckTable(database = database, "matrix_tube")$barcode %>% head(10),
         study_subject_id = CheckTable(database = database, "study_subject")$uid %>% head(10),
         specimen_type = "PLASMA") %>% as.data.frame()
}