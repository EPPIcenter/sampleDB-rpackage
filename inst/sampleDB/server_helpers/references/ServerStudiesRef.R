

UpdateLabStudies <- function(session, input, output, database){
  
  # get ui freezer elements
  ui_elements <- GetUIStudiesElements()
  
  #check freezer update
  StudyChangesChecks(input, database, output, ui_elements = ui_elements)
  
  #option1: add study to the database
  observeEvent(
    input[[ui_elements$ui.input$AddStudyAction]],
    ({
      
      # set requirements
      SetAddStudyRequirements(input, database, ui_elements)
      
      return_message <- UpdateReferences(reference = "study",
                                                   operation = "add",
                                                   update = list(study_title = input[[ui_elements$ui.input$AddStudyTitle]],
                                                                 study_description = input[[ui_elements$ui.input$AddStudyDescription]],
                                                                 study_short_code = input[[ui_elements$ui.input$AddStudyShortCode]],
                                                                 study_lead_person = input[[ui_elements$ui.input$AddStudyLeadPerson]],
                                                                 study_longitudinal = input[[ui_elements$ui.input$AddStudyIsLongitudinal]]))
      
      # print user message
      output[[ui_elements$ui.output$StudyReturnMessage]] <- renderText({return_message})
      
      # print studies
      ShowStudies(output, database)
      
      #update dropdowns
      UpdateStudyDropdowns(database, session)
    })
  )
  
  #option 2: change study in the database
  observeEvent(
    input[[ui_elements$ui.input$RenameStudyAction]],
    ({
      
      # set requirements
      SetChangeStudyRequirements(input, database, ui_elements)
      
      return_message <- UpdateReferences(reference = "study",
                                                   operation = "modify",
                                                   identifier = list(study_short_code = input[[ui_elements$ui.input$ChangeStudyShortCode]]),
                                                   update = list(study_title = input[[ui_elements$ui.input$RenameStudyTitle]],
                                                                 study_description = input[[ui_elements$ui.input$RenameStudyDescription]],
                                                                 study_short_code = input[[ui_elements$ui.input$RenameStudyShortCode]],
                                                                 study_lead_person = input[[ui_elements$ui.input$RenameStudyLeadPerson]],
                                                                 study_longitudinal = sum(input[[ui_elements$ui.input$RenameStudyIsLongitudinal]])))
      # print user message
      output[[ui_elements$ui.output$StudyReturnMessage]] <- renderText({return_message})
      
      # print studies
      ShowStudies(output, database)
      
      #update dropdowns
      UpdateStudyDropdowns(database, session)
    }))
  
  #option3: delete study from the database
  observeEvent(
    input[[ui_elements$ui.input$DeleteStudyAction]],
    ({
      
      # set requirements
      SetDeleteStudyRequirements(input, database, ui_elements)
      
      return_message <- UpdateReferences(reference = "study",
                                                   operation = "delete",
                                                   identifier = list(study_short_code = input[[ui_elements$ui.input$DeleteStudyShortCode]]))
      
      # print user message
      output[[ui_elements$ui.output$StudyReturnMessage]] <- renderText({return_message})
      
      # print studies
      ShowStudies(output, database)
      
      #update dropdowns
      UpdateStudyDropdowns(database, session)
    })
  )
  
  # print studies
  ShowStudies(output, database) 
}

ShowStudies <- function(output, database){

  con = dbConnect(SQLite(), database)
  output$TableStudy <- DT::renderDataTable({
    dbReadTable(con, "study") %>%
      dplyr::select(-c(id, last_updated)) %>%
      mutate(is_longitudinal = as.logical(is_longitudinal)) %>%
      rename(Title = title,
            Created = created,
             Description = description,
             `Study Code` = short_code,
             `Longitudinal` = is_longitudinal,
             `Lead Person` = lead_person)
  }, selection = 'single')
}

# AddStudyRequirements <- function(input){
#   req(input$AddStudyTitle,
#       input$AddStudyDescription,
#       input$AddStudyShortCode,
#       input$AddStudyLeadPerson) 
# }
