
SearchReset <- function(input){
  observeEvent(input$ClearSearchBarcodes, ({reset("SearchByBarcode")}))
  observeEvent(input$ClearSearchUIDFile, ({reset("SearchBySubjectUIDFile")})) 
}