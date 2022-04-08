
MoveWetlabContainers <- function(session, input, database, output){
  
  # auto filter container names
  observe({
    if(input$MoveContainerSampleType == "micronix"){
      updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("matrix_plate")$plate_name))
    }
    else if(input$MoveContainerSampleType == "cryovile"){
      updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("box")$box_name))
    }
    else if(input$MoveContainerSampleType == "rdt"){
      updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
    }else{
      updateSelectizeInput(session, "MoveContainerName", label = NULL, choices = c(sampleDB::CheckTable("bag")$bag_name))
    }
  })
  
  # auto filter freezers
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
  
  # move container
  observeEvent(
    input$MoveContainerAction,({
      container.type <- input$MoveContainerSampleType
      container.name <- input$MoveContainerName
      freezer.name <- input$MoveContainerLocation
      freezer.levelI <- input$MoveContainerLocationLevelI
      freezer.levelII <- input$MoveContainerLocationLevelII
      sampleDB::MoveContainers(sample_type = container.type,
                               container_name = container.name,
                               freezer = list(freezer.name = freezer.name,
                                              freezer.levelI = freezer.levelI,
                                              freezer.levelII = freezer.levelII))
      output$MoveContainerMessage <- renderText("Successfully Moved Container")
      shinyjs::reset("MoveContainerName")
      shinyjs::reset("MoveContainerLocation")
      shinyjs::reset("MoveContainerMessage")
    }))
  
}