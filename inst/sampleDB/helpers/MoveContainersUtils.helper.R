
MoveWetlabContainers <- function(session, input, database, output){
  observe({
    if(input$MoveContainerSampleType == "Micronix"){
      updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("matrix_plate")$plate_name))
    }
    else if(input$MoveContainerSampleType == "Cryovile"){
      updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("box")$box_name))
    }
    else if(input$MoveContainerSampleType == "RDT"){
      updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
    }else{
      updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
    }
  })
  
  observe({
    if(input$MoveContainerLocation != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input$MoveContainerLocation)
      updateSelectizeInput(session, "MoveContainerLocationLevelI", label = NULL, choices = c(tmp_table.location$level_I))
      updateSelectizeInput(session, "MoveContainerLocationLevelII", label = NULL, choices = c(tmp_table.location$level_II))
    }else{
      updateSelectizeInput(session, "MoveContainerLocationLevelI", label = NULL, choices = c(""))
      updateSelectizeInput(session, "MoveContainerLocationLevelII", label = NULL, choices = c(""))
    }
  })
  
  observeEvent(
    input$MoveContainerAction,({
      container.type <- input$MoveContainerSampleType
      container.name <- input$MoveContainerName
      freezer.name <- input$MoveContainerLocation
      freezer.levelI <- input$MoveContainerLocationLevelI
      freezer.levelII <- input$MoveContainerLocationLevelII
      sampleDB::MoveContainers(type = container.type,
                               container_name = container.name,
                               location = list(freezer.name,
                                               freezer.levelI,
                                               freezer.levelII))
    }))
}