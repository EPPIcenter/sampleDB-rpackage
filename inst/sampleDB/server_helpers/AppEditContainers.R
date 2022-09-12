
EditWetlabContainers <- function(session, input, database, output){
  
  observeEvent(
    input$MoveContainerAction,({
      #get user info
      container.type <- input$EditContainerSampleType
      container.name <- input$EditContainerName
      freezer.name <- input$MoveContainerLocation
      freezer.levelI <- input$MoveContainerLocationLevelI
      freezer.levelII <- input$MoveContainerLocationLevelII

      database <- Sys.getenv("SDB_PATH")
      conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
      RSQLite::dbBegin(conn)


      tryCatch(
        expr = {

          #move container
          return_message <- sampleDB::MoveContainers(
            sample_type = container.type,
            container_name = container.name,
            freezer = list(
              freezer.name = freezer.name,
              freezer.levelI = freezer.levelI,
              freezer.levelII = freezer.levelII
            ),
            conn = conn)

          output$RenameContainerMessage <- renderText(return_message)
        },
        warning = function(w) {
          output$RenameContainerMessage <- renderText({ paste("ERROR:", w$message) })
          message(w)
        },
        error = function(e) {
          output$RenameContainerMessage <- renderText({ paste("ABORT:", e$message) })
          message(e)
        },
        finally = {
          RSQLite::dbCommit(conn)
          RSQLite::dbDisconnect(conn)
        }
      )
    }))
  
  observeEvent(
    input$RenameContainerAction,
    ({
      #get user info
      container.type <- input$EditContainerSampleType
      current_container.name <- input$EditContainerName
      new_container.name <- input$RenameContainerPlateName

      database <- Sys.getenv("SDB_PATH")
      conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
      RSQLite::dbBegin(conn)
      
      tryCatch(
        expr = {

          #rename container
          return_message <- sampleDB::RenameContainers(sample_type = container.type, 
                                                       new_container_name = new_container.name, 
                                                       current_container_name = current_container.name,
                                                       conn = conn)

          output$RenameContainerMessage <- renderText(return_message)
        },
        warning = function(w) {
          output$RenameContainerMessage <- renderText({ paste("ERROR:", w$message) })
          message(w)
        },
        error = function(e) {
          output$RenameContainerMessage <- renderText({ paste("ABORT:", e$message) })
          message(e)
        },
        finally = {
          RSQLite::dbCommit(conn)
          RSQLite::dbDisconnect(conn)
        }
      )
    }))
  
  observeEvent(
    input$DeleteContainerAction,
    ({
      
      # set requirements
      # DeleteEmptyPlateRequirement(input, database)
      
      # get user information
      container.type <- input$EditContainerSampleType
      container.name <- input$EditContainerName

      database <- Sys.getenv("SDB_PATH")
      conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
      RSQLite::dbBegin(conn)
      
      # delete plate
      tryCatch(
        expr = {
          return_message <- sampleDB::DeleteEmptyContainer(type = container.type, container_name = container.name, conn = conn)
          output$RenameContainerMessage <- renderText({return_message})
        },
        warning = function(w) {
          output$RenameContainerMessage <- renderText({ paste("ERROR:", w$message) })
          message(w)
        },
        error = function(e) {
          output$RenameContainerMessage <- renderText({ paste("ABORT:", e$message) })
          message(e)
        },
        finally = {
          RSQLite::dbCommit(conn)
          RSQLite::dbDisconnect(conn)
        }
      )
    }))
  
  observe({
    updateSelectInput(session, selected = input$MoveContainerLocation, "MoveContainerLocation", choices = dbUpdateEvent()$location %>% sort())
    updateSelectizeInput(session, selected = input$EditContainerName, "EditContainerName", choices = dbUpdateEvent()$plate_name %>% sort())
  })
  
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
  
}
