
EditWetlabContainers <- function(session, input, database, output){
  
  observeEvent(
    input$MoveContainerAction,({
      #get user info
      container.type <- input$EditContainerSampleType
      container.name <- input$EditContainerName
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
      shinyjs::reset("EditContainerName")
      shinyjs::reset("MoveContainerLocation")
      shinyjs::reset("MoveContainerMessage")
    }))
  
  observeEvent(
    input$RenameContainerAction,
    ({
      #get user info
      container.type <- input$EditContainerSampleType
      current_container.name <- input$EditContainerName
      new_container.name <- input$RenameContainerPlateName
      
      #rename container
      return_message <- sampleDB::RenameContainers(sample_type = container.type, 
                                                   new_container_name = new_container.name, 
                                                   current_container_name = current_container.name)
      output$RenameContainerMessage <- renderText(return_message)
      
      #reset
      shinyjs::reset("EditContainerName")
      shinyjs::reset("RenameContainerPlateName")
      shinyjs::reset("RenameContainerMessage")
    }))
  
  observeEvent(
    input$DeleteContainerAction,
    ({
      
      # set requirements
      # DeleteEmptyPlateRequirement(input, database)
      
      # get user information
      container.type <- input$EditContainerSampleType
      container.name <- input$EditContainerName
      
      # delete plate
      return_message <- sampleDB::DeleteEmptyContainer(type = container.type, container_name = container.name)
      output$DeleteContainerMessage <- renderText({return_message})
      
      shinyjs::reset("EditContainerName")
    }))
  
  # smart dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = "MoveContainerLocation", 
                             levelI_ui = "MoveContainerLocationLevelI", 
                             levelII_ui = "MoveContainerLocationLevelII")
  
  # user selected storage type
  observe({
    if(input$EditContainerSampleType == "micronix"){
      updateSelectizeInput(session, "EditContainerName", label = NULL, choices = c("", sampleDB::CheckTable("matrix_plate")$plate_name))
    }
    else if(input$EditContainerSampleType == "cryovile"){
      updateSelectizeInput(session, "EditContainerName", label = NULL, choices = c("", sampleDB::CheckTable("box")$box_name))
    }
    else if(input$EditContainerSampleType == "rdt"){
      updateSelectizeInput(session, "EditContainerName", label = NULL, choices = c("", sampleDB::CheckTable("bag")$bag_name))
    }else{
      updateSelectizeInput(session, "EditContainerName", label = NULL, choices = c("", sampleDB::CheckTable("bag")$bag_name))
    }
  })

  observeEvent(dbUpdateEvent(), {
    if(input$EditContainerSampleType == "micronix"){
      updateSelectizeInput(session, "EditContainerName", label = NULL, choices = c("", sampleDB::CheckTable("matrix_plate")$plate_name))
    }
    else if(input$EditContainerSampleType == "cryovile"){
      updateSelectizeInput(session, "EditContainerName", label = NULL, choices = c("", sampleDB::CheckTable("box")$box_name))
    }
    else if(input$EditContainerSampleType == "rdt"){
      updateSelectizeInput(session, "EditContainerName", label = NULL, choices = c("", sampleDB::CheckTable("bag")$bag_name))
    }else{
      updateSelectizeInput(session, "EditContainerName", label = NULL, choices = c("", sampleDB::CheckTable("bag")$bag_name))
    }
  })
  
}
