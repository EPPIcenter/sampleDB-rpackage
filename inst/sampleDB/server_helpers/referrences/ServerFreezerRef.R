UpdateLabFreezers <- function(session, input, output, database){
  
  # get ui freezer elements
  ui_elements <- GetUIFreezerElements()
  
  #check freezer update
  FreezerChangesChecks(input, database, output, ui_elements = ui_elements)
  
  #option1: add freezer address to the database
  observeEvent(
    input[[ui_elements$ui.input$AddFreezerAction]],
    ({
      
      # save user input
      new.freezer_name <- input[[ui_elements$ui.input$AddFreezerName]]
      new.freezer_type <- input[[ui_elements$ui.input$AddFreezerType]]
      new.freezer_levelI <- input[[ui_elements$ui.input$AddFreezerLevel_I]]
      new.freezer_levelII <- input[[ui_elements$ui.input$AddFreezerLevel_II]]
      
      # set requirements
      SetFreezerAddRequirements(input = input, database = database, ui_elements = ui_elements)
      
      tryCatch(
        return_message <- sampleDB::UpdateReferences(reference = "freezer",
                                                     operation = "add",
                                                     update = list(freezer_name = new.freezer_name,
                                                                   freezer_type = new.freezer_type,
                                                                   freezer_levelI = new.freezer_levelI,
                                                                   freezer_levelII = new.freezer_levelII)),
        error=function(e){
          print(e)
        }
      )
      
      # print user message
      output[[ui_elements$ui.output$FreezerReturnMessage]] <- renderText({return_message})
      
      # print freezers
      ShowFreezers(output, database)
      
      # update dropdowns
      UpdateFreezerDropdowns(database, session)
    }))
  
  #option2: change freezer address name
  observeEvent(
    input[[ui_elements$ui.input$RenameFreezerAction]],
    ({
      
      # save user input
      old.freezer_name <- input[[ui_elements$ui.input$RenameFreezerName1]]
      old.freezer_levelI <- input[[ui_elements$ui.input$RenameFreezerLevelI1]]
      old.freezer_levelII <- input[[ui_elements$ui.input$RenameFreezerLevelII1]]
      new.freezer_name <- input[[ui_elements$ui.input$RenameFreezerName2]]
      new.freezer_type <- input[[ui_elements$ui.input$RenameFreezerType2]]
      new.freezer_levelI <- input[[ui_elements$ui.input$RenameFreezerLevelI2]]
      new.freezer_levelII <- input[[ui_elements$ui.input$RenameFreezerLevelI2]]

      #set requirements
      SetFreezerChangeRequirements(input = input, database = database, ui_elements = ui_elements)
      
      return_message <- sampleDB::UpdateReferences(reference = "freezer",
                                                   operation = "modify",
                                                   identifier = list(freezer_name = old.freezer_name,
                                                                     freezer_levelI = old.freezer_levelI,
                                                                     freezer_levelII = old.freezer_levelII),
                                                   update = list(freezer_name = new.freezer_name,
                                                                 freezer_type = new.freezer_type,
                                                                 freezer_levelI = new.freezer_levelI,
                                                                 freezer_levelII = new.freezer_levelII) %>% purrr::discard(function(x){is.null(x) || x == ""}))
      
      
      output[[ui_elements$ui.output$FreezerReturnMessage]] <- renderText({return_message})
      
      # print freezers
      ShowFreezers(output, database)
      
      # update dropdowns
      UpdateFreezerDropdowns(database, session)
      
    }))
  
  # smart populate freezer dropdowns 
  SmartFreezerDropdownFilter(database = database, session = session, input = input, 
                             location_ui = ui_elements$ui.input$RenameFreezerName1, 
                             levelI_ui = ui_elements$ui.input$RenameFreezerLevelI1, 
                             levelII_ui = ui_elements$ui.input$RenameFreezerLevelII1)
  
  #option3: remove freezer address
  observeEvent(
    input$DeleteFreezerAction,
    ({
      
      # save user input
      delete.freezer_name <- input[[ui_elements$ui.input$DeleteFreezerName]]
      delete.freezer_levelI <- input[[ui_elements$ui.input$DeleteFreezerLevelI]]
      delete.freezer_levelII <- input[[ui_elements$ui.input$DeleteFreezerLevelII]]
      
      # set requirements
      SetFreezerDeleteRequirements(input = input, database = database, ui_elements = ui_elements)
      
      return_message <- sampleDB::UpdateReferences(reference = "freezer",
                                                   operation = "delete",
                                                   identifier = list(freezer_name = delete.freezer_name,
                                                                     freezer_levelI = delete.freezer_levelI,
                                                                     freezer_levelII = delete.freezer_levelII))
      # print user message
      output$FreezerReturnMessage <- renderText({return_message})
      
      # print freezers
      ShowFreezers(output, database)
      
      # update dropdowns
      UpdateFreezerDropdowns(database, session)
      
    })
  )
  
  # smart populate freezer dropdowns 
  SmartFreezerDropdownFilter(database = database, session = session, input = input, 
                             location_ui = "DeleteFreezerName", levelI_ui = "DeleteFreezerLevelI", levelII_ui = "DeleteFreezerLevelII")
  
  # print freezers
  ShowFreezers(output, database)
}

ShowFreezers <- function(output, database){
  output$TableFreezer <- DT::renderDataTable({
    sampleDB::CheckTable(database = database, "location") %>%
      dplyr::select(-c(created:id, level_III)) %>%
      rename(`Freezer Name` = location_name,
             `Type` = location_type,
             `Level I` = level_I,
             `Level II` = level_II)
  })
}
