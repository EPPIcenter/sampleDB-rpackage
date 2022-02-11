

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