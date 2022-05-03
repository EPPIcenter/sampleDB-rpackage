

DeleteEmptyWetlabContainers <- function(session, input, database, output){
  
  
  # DeletePlateChecks(input, database, output)
  
  observeEvent(
    input$DeleteContainerAction,
    ({
      
      # set requirements
      # DeleteEmptyPlateRequirement(input, database)
      
      # get user information
      type.container <- input$DeleteContainerSampleType
      name.container <- input$DeleteContainerName
      
      # delete plate
      return_message <- sampleDB::DeleteEmptyContainer(type = type.container, container_name = name.container)
      output$DeleteEmptyContainerOfSamples <- renderText({return_message})
    }))
  
  #smart dropdown
  observe({
    if(input$DeleteContainerSampleType == "micronix"){
      updateSelectizeInput(session, "DeleteContainerName", label = NULL, choices = c("", sampleDB::CheckTable("matrix_plate")$plate_name))
    }
    else if(input$DeleteContainerSampleType == "cryovial"){
      updateSelectizeInput(session, "DeleteContainerName", label = NULL, choices = c("", sampleDB::CheckTable("box")$box_name))
    }
    else if(input$DeleteContainerSampleType == "rdt"){
      updateSelectizeInput(session, "DeleteContainerName", label = NULL, choices = c("", sampleDB::CheckTable("bag")$bag_name))
    }else{
      updateSelectizeInput(session, "DeleteContainerName", label = NULL, choices = c("", sampleDB::CheckTable("bag")$bag_name))
    }
  })
}