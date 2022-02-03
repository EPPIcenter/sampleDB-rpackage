
UploadReset <- function(input, output){
  observeEvent(
    input$ClearUploadForm,
    ({
      reset("UploadDataSet")
      reset("UploadStudyShortCode")
      reset("UploadPlateID")
      reset("UploadLocation")
      output$UploadReturnMessage1 <- renderText({""})
      output$UploadReturnMessage2 <- renderText({""})
    }))  
}
