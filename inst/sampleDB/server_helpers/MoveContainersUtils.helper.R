
MoveWetlabContainers <- function(session, input, database, output){
  
  observeEvent(
    input$MoveContainerAction,({
      #get user info
      container.type <- input$MoveContainerSampleType
      container.name <- input$MoveContainerName
      freezer.name <- input$MoveContainerLocation
      freezer.levelI <- input$MoveContainerLocationLevelI
      freezer.levelII <- input$MoveContainerLocationLevelII
      
      #move container
      return_message <- sampleDB::MoveContainers(sample_type = container.type,
                                                 container_name = container.name,
                                                 freezer = list(freezer.name = freezer.name,
                                              freezer.levelI = freezer.levelI,
                                              freezer.levelII = freezer.levelII))
      output$MoveContainerMessage <- renderText(return_message)
      
      #reset
      shinyjs::reset("MoveContainerName")
      shinyjs::reset("MoveContainerLocation")
      shinyjs::reset("MoveContainerMessage")
    }))
  
  # smart dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = "MoveContainerLocation", 
                             levelI_ui = "MoveContainerLocationLevelI", 
                             levelII_ui = "MoveContainerLocationLevelII")
  
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
  
}