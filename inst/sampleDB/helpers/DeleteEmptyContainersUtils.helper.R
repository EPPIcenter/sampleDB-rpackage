

DeleteEmptyWetlabContainers <- function(session, input, database, output){
  observe({
    if(input$DeleteContainerSampleType == "Micronix"){
      updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("matrix_plate")$plate_name))
    }
    else if(input$DeleteContainerSampleType == "Cryovile"){
      updateSelectizeInput(session, "DelteContainerName", label = NULL, choices = c(sampleDB::CheckTable("box")$box_name))
    }
    else if(input$DeleteContainerSampleType == "RDT"){
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
      # print(type.container)
      # print(name.container)
      # DeleteEmptyPlates(type.container, name.container)
      
      # RESET PLATE NAMES DROPDOWN
      # DeleteEmptyPlateReset(session, database)
    }))
}