

DeleteEmptyWetlabContainers <- function(session, input, database, output){
  observe({
    if(input$DeleteContainerSampleType == "micronix"){
      updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("matrix_plate")$plate_name))
    }
    else if(input$DeleteContainerSampleType == "cryovial"){
      updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("box")$box_name))
    }
    else if(input$DeleteContainerSampleType == "rdt"){
      updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
    }else{
      updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
    }
  })
  
  # DeletePlateChecks(input, database, output)
  
  observeEvent(
    input$DeleteContainerAction,
    ({
      
      # # SET REQUIREMENT FOR DELETEING PLATE
      # DeleteEmptyPlateRequirement(input, database)
      
      # DELETE PLATE
      type.container <- input$DeleteContainerSampleType
      name.container <- input$DelteContainerName
      sampleDB::DeleteEmptyContainer(type = type.container, container_name = name.container)
      
      # RESET PLATE NAMES DROPDOWN
      # DeleteEmptyPlateReset(session, database)
    }))
}