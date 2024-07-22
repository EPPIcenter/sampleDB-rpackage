UpdateSpecimenTypes <- function(session, input, output, database){
  
  # get ui freezer elements
  ui_elements <- GetUISpecimenTypeElements()
  
  # Initialize Dropdowns
  observeEvent(TRUE, {
    UpdateSpecimenTypeDropdowns(database, session) 
  }, ignoreNULL = TRUE, once = TRUE)

  #check freezer update
  SpecimenTypeChangesChecks(input, database, output, ui_elements = ui_elements)
  
  #option1: add specimen type to the database
  observeEvent(
    input[[ui_elements$ui.input$AddSpecimenTypeAction]],
    ({
      
      # save user input
      new.specimen_type <- input[[ui_elements$ui.input$AddSpecimenType]]
      
      #set requirements
      req(input[[ui_elements$ui.input$AddSpecimenType]],
          CheckSpecimenTypeUnique(input, database, specimen_type = input[[ui_elements$ui.input$AddSpecimenType]]) == TRUE)
      
      return_message <- UpdateReferences(reference = "specimen_type",
                                                   operation = "add",
                                                   update = list(specimen_type_name = new.specimen_type))
      # print user message
      output[[ui_elements$ui.output$SpecimenReturnMessage]] <- renderText({return_message})

      # print specimen types
      ShowSpecimenTypes(output, database)
      
      # update dropdowns
      UpdateSpecimenTypeDropdowns(database, session)
    }))
  
  #option 2: change a specimen type
  observeEvent(
    input[[ui_elements$ui.input$RenameSpecimenTypeAction]],
    ({
      
      # save user input
      old.name <- input$RenameSpecimenType1
      new.name <- input$RenameSpecimenType2
      
      #set requirements
      req(input$RenameSpecimenType1,
          input$RenameSpecimenType2,
          CheckSpecimenTypeUnique(input, database, specimen_type = input[[ui_elements$ui.input$RenameSpecimenType2]]) == TRUE)
      
      return_message <- UpdateReferences(reference = "specimen_type",
                                                   operation = "modify",
                                                   identifier = list(specimen_type_name = old.name),
                                                   update = list(specimen_type_name = new.name))
                        
      # print user message
      output$SpecimenReturnMessage <- renderText({return_message})
      
      # print specimen types
      ShowSpecimenTypes(output, database)
      
      # update dropdowns
      UpdateSpecimenTypeDropdowns(database, session)
    }))
  
  #option3: remove specimen type
  observeEvent(
    input[[ui_elements$ui.input$DeleteSpecimenTypeAction]],
    ({
      
      # save user input
      delete.specimen_type <- input[[ui_elements$ui.input$DeleteSpecimenType]]
      
      #set requirements
      req(input$DeleteSpecimenTypeAction,
          CheckSpecimenTypeDeletion(input, database , specimen_type = input[[ui_elements$ui.input$DeleteSpecimenType]]) == TRUE)
      
      return_message <- UpdateReferences(reference = "specimen_type",
                                                   operation = "delete",
                                                   identifier = list(specimen_type_name = delete.specimen_type))
      
      # print user message
      output[[ui_elements$ui.output$SpecimenReturnMessage]] <- renderText({return_message})
      
      # print specimen types
      ShowSpecimenTypes(output, database)
      
      # update dropdowns
      UpdateSpecimenTypeDropdowns(database, session)
    }))
  
  # print specimen types
  ShowSpecimenTypes(output, database)      
}

ShowSpecimenTypes <- function(output, database){
  output$TableSpecimenType <- DT::renderDataTable({
    CheckTable(database = database, "specimen_type") %>%
      dplyr::select(-c(`created`:id)) %>%
      rename(`Specimen Type` = name)})
}
