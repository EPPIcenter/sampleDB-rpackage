
UploadExamples <- function(input, database, output){
  output$ExampleUploadCSVNoDate <- renderPrint({helper.ExampleUploadCSVNoDate(database)})
  output$ExampleUploadCSVDate <- renderPrint({helper.ExampleUploadCSVDate(database)}) 
}

################################################################################

helper.ExampleUploadCSVDate <-  function(database){
  tibble(LocationRow = rep("A", 10),
         LocationColumn = c(1:10),
         TubeCode = paste0("XXX", 1:10),
         study_subject_id = paste0("subject_", 1:10),
         specimen_type = "PLASMA",
         study_short_code = "KAM06",
         collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% as.data.frame()
}

helper.ExampleUploadCSVNoDate <- function(database){
  tibble(LocationRow = rep("A", 10),
         LocationColumn = c(1:10),
         TubeCode = paste0("XXX", 1:10),
         study_subject_id = paste0("subject_", 1:10),
         specimen_type = "PLASMA",
         study_short_code = "KAM06") %>% as.data.frame()
}