

UpdateLabFreezers <- function(session, input, output, database){
  FreezerChangesChecks(input, database, output)
  
  #ADD FREEZER TO DATABASE
  observeEvent(
    input$AddFreezerAction,
    ({
      
      #SAVE FREEZER NAMES INVOLVED
      new.freezer_name <- input$AddFreezerName
      new.freezer_type <- input$AddFreezerType
      new.freezer_levelI <- input$AddFreezerLevel_I
      new.freezer_levelII <- input$AddFreezerLevel_II
      
      #SET REQUIREMENTS
      req(input$AddFreezerName,
          input$AddFreezerType,
          input$AddFreezerLevel_I,
          input$AddFreezerLevel_II)
      
      #ADD FREEZER NAME
      tryCatch(
        sampleDB::UpdateReferences(reference = "freezer",
                                   operation = "add",
                                   update = list(freezer_name = new.freezer_name,
                                                 freezer_type = new.freezer_type,
                                                 freezer_levelI = new.freezer_levelI,
                                                 freezer_levelII = new.freezer_levelII)),
        error=function(e){
          print(e)
        }
      )
      
      #PRINT EXIT MESSAGE
      output$FreezerReturnMessage <- renderText({paste("Added Freezer:", 
                                                       new.freezer_name, 
                                                       new.freezer_type, 
                                                       new.freezer_levelI, 
                                                       new.freezer_levelII,
                                                       emoji('tada'))})
      
      #MODIFY TABLE
      ShowFreezers(output, database)
      
      #UPDATE DROPDOWNS
      UpdateFreezerDropdowns(database, session)
    }))
  
  # CHANGE FREEZER NAMES
  
  # AUTO FILTER LEVELS ACCORDING TO USER SELECTED FREEZER
  observe({
    if(input$RenameFreezerName1 != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input$RenameFreezerName1)
      updateSelectInput(session, "RenameFreezerLevelI1", label = NULL, choices = c(tmp_table.location$level_I))
      updateSelectInput(session, "RenameFreezerLevelII1", label = NULL, choices = c(tmp_table.location$level_II))
    }else{
      updateSelectInput(session, "RenameFreezerLevelI1", label = NULL, choices = c(""))
      updateSelectInput(session, "RenameFreezerLevelII1", label = NULL, choices = c(""))
    }
  })
  
  observeEvent(
    input$RenameFreezerAction,
    ({
      
      #SAVE FREEZER NAMES INVOLVED
      old.freezer_name <- input$RenameFreezerName1
      old.freezer_levelI <- input$RenameFreezerLevelI1
      old.freezer_levelII <- input$RenameFreezerLevelII1
      new.freezer_name <- input$RenameFreezerName2
      new.freezer_type <- input$RenameFreezerType2
      new.freezer_levelI <- input$RenameFreezerLevelI2
      new.freezer_levelII <- input$RenameFreezerLevelI2
      
      # #MODIFY TABLE IF NEW FREEZER NAME IS UNIQUE
      # req(input$RenameFreezer1,
      #     input$RenameFreezer2,
      #     !(new.name %in% c(sampleDB::CheckTable(database = database, "location")$location_name)))
      
      #CHANGE FREEZER NAME
      sampleDB::UpdateReferences(reference = "freezer",
                                 operation = "modify",
                                 identifier = list(freezer_name = old.freezer_name,
                                                    freezer_levelI = old.freezer_levelI,
                                                   freezer_levelII = old.freezer_levelII),
                                 update = list(freezer_name = new.freezer_name,
                                               freezer_type = new.freezer_type,
                                               freezer_levelI = new.freezer_levelI,
                                               freezer_levelII = new.freezer_levelII) %>% purrr::discard(function(x){is.null(x) || x == ""}))
      
      #PRINT EXIT MESSAGE
      # NOTE PRINT MESSAGE IS MISSING TYPE
      output$FreezerReturnMessage <- renderText({paste("Renamed Freezer", 
                                                       old.freezer_name,
                                                       old.freezer_levelI,
                                                       old.freezer_levelII,
                                                       "to",
                                                       new.freezer_name,
                                                       new.freezer_levelI,
                                                       new.freezer_levelII,
                                                       emoji('tada'))})
      
      #REFRESH REFERENCES
      ShowFreezers(output, database)
      
      #UPDATE DROPDOWNS
      UpdateFreezerDropdowns(database, session)
      
    }))
  
  #REMOVE FREEZER FROM DATABASE
  
  # AUTO FILTER LEVELS ACCORDING TO USER SELECTED FREEZER
  observe({
    if(input$DeleteFreezerName != ""){
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == input$DeleteFreezerName)
      updateSelectInput(session, "DeleteFreezerLevelI", label = NULL, choices = c(tmp_table.location$level_I))
      updateSelectInput(session, "DeleteFreezerLevelII", label = NULL, choices = c(tmp_table.location$level_II))
    }else{
      updateSelectInput(session, "DeleteFreezerLevelI", label = NULL, choices = c(""))
      updateSelectInput(session, "DeleteFreezerLevelII", label = NULL, choices = c(""))
    }
  })
  
  observeEvent(
    input$DeleteFreezerAction,
    ({
      
      #SAVE FREEZER NAMES INVOLVED
      delete.freezer_name <- input$DeleteFreezerName
      delete.freezer_levelI <- input$DeleteFreezerLevelI
      delete.freezer_levelII <- input$DeleteFreezerLevelII
      
      # #SET REQUIREMENTS
      # req(input$DeleteFreezer,
      #     !(filter(sampleDB::CheckTable(database = database, "location"), location_name == delete.freezer_name)$id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id))
      
      #DELETE FREEZER
      sampleDB::UpdateReferences(reference = "freezer",
                                 operation = "delete",
                                 identifier = list(freezer_name = delete.freezer_name,
                                                   freezer_levelI = delete.freezer_levelI,
                                                   freezer_levelII = delete.freezer_levelII))
      #PRINT EXIT MESSAGE
      output$FreezerReturnMessage <- renderText({paste("Deleted Freezer",
                                                       delete.freezer_name, 
                                                       delete.freezer_levelI,
                                                       delete.freezer_levelII,
                                                       emoji('tada'))})
      
      #REFRESH REFERENCES
      ShowFreezers(output, database)
      
      #UPDATE DROPDOWNS
      UpdateFreezerDropdowns(database, session)
      
    })
  )
  
  # IDLY PRESENT FREEZERS IN DATATABLE
  ShowFreezers(output, database)
}

UpdateSpecimenTypes <- function(session, input, output, database){
  SpecimenTypeChangesChecks(input, database, output)
  
  #ADD A SPECIMEN TYPE TO THE DATABASE
  observeEvent(
    input$AddSpecimenTypeAction,
    ({
      
      #SAVE SPECIMEN_TYPE NAMES INVOLVED
      new.specimen_type <- input$AddSpecimenType
      
      # #SET REQUIREMENT
      # req(input$AddSpecimenType,
      #     !(new.specimen_type %in% c(sampleDB::CheckTable(database = database, "specimen_type")$label)))
      
      #ADD SPECIMEN TYPE
      sampleDB::UpdateReferences(reference = "specimen_type",
                                 operation = "add",
                                 update = list(specimen_type_name = new.specimen_type))
      
      #PRINT EXIT MESSAGE
      output$SpecimenReturnMessage <- renderText({paste("Added Specimen Type", new.specimen_type, emoji('tada'))})
      
      #REFRESH REFERENCES
      ShowSpecimenTypes(output, database)
      
      #UPDATE DROPDOWNS
      UpdateSpecimenTypeDropdowns(database, session)
    }))
  
  #CHANGE A SPECIMEN TPYE
  observeEvent(
    input$RenameSpecimenTypeAction,
    ({
      
      #SAVE SPECIMEN_TYPE NAMES INVOLVED
      old.name <- input$RenameSpecimenType1
      new.name <- input$RenameSpecimenType2
      
      # #CHANGE SPECIMEN_TYPE IF IT IS UNIQUE
      # req(input$RenameSpecimenType1,
      #     input$RenameSpecimenType2,
      #     !(new.name %in% c(sampleDB::CheckTable(database = database, "specimen_type")$label)))
      
      #CHANGE SPECIMEN TYPE NAME
      sampleDB::UpdateReferences(reference = "specimen_type",
                                 operation = "modify",
                                 identifier = list(specimen_type_name = old.name),
                                 update = list(specimen_type_name = new.name))
      
      #PRINT EXIT MESSAGE
      output$SpecimenReturnMessage <- renderText({paste("Renamed Specimen Type", old.name, "to", new.name, emoji('tada'))})
      
      #REFRESH REFERENCES
      ShowSpecimenTypes(output, database)
      
      #UPDATE DROPDOWNS
      UpdateSpecimenTypeDropdowns(database, session)
    }))
  
  #DELETE A SPECIMEN TYPE
  observeEvent(
    input$DeleteSpecimenTypeAction,
    ({
      
      #SAVE SPECIMEN_TYPE NAMES INVOLVED
      delete.specimen_type <- input$DeleteSpecimenType
      
      # #SET REQUIREMENT
      # req(input$DeleteSpecimenTypeAction,
      #     !(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == delete.specimen_type)$id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id))
      
      #DELETE SPECIMEN TYPE
      sampleDB::UpdateReferences(reference = "specimen_type",
                                 operation = "delete",
                                 identifier = list(specimen_type_name = delete.specimen_type))
      
      #PRINT EXIT MESSAGE
      output$SpecimenReturnMessage <- renderText({paste("Deleted Specimen Type", delete.specimen_type, emoji('tada'))})
      
      #REFRESH REFERENCES
      ShowSpecimenTypes(output, database)
      
      #UPDATE DROPDOWNS
      UpdateSpecimenTypeDropdowns(database, session)
    }))
  
  #IDLY PRESENT SPECIMEN TYPES
  ShowSpecimenTypes(output, database)      
}

UpdateLabStudies <- function(session, input, output, database){
  StudyChangesChecks(input, database, output)
  
  #ADD A STUDY TO THE DATABASE
  observeEvent(
    input$AddStudyAction,
    ({
      
      # # SET REQUIREMENTS
      # AddStudyRequirements(input)
      
      #ADD STUDY
      sampleDB::UpdateReferences(reference = "study",
                                 operation = "add",
                                 update = list(study_title = input$AddStudyTitle,
                                               study_description = input$AddStudyDescription,
                                               study_short_code = input$AddStudyShortCode,
                                               study_lead_person = input$AddStudyLeadPerson,
                                               study_longitudinal = input$AddStudyIsLongitudinal))
      
      #PRINT EXIT MESSAGE
      output$StudyReturnMessage <- renderText({paste("Added Study to the Database", emoji('tada'))})
      
      #REFRESH REFERENCES
      ShowStudies(output, database)
      
      #UPDATE DROPDOWNS
      UpdateStudyDropdowns(database, session)
    })
  )
  
  # CHANGE A STUDY
  observeEvent(
    input$RenameStudyAction,
    ({
      
      # MODIFY STUDY
      sampleDB::UpdateReferences(reference = "study",
                                 operation = "modify",
                                 identifier = list(study_short_code = input$ChangeStudyShortCode),
                                 update = list(study_title = input$RenameStudyTitle,
                                               study_description = input$RenameStudyDescription,
                                               study_short_code = input$RenameStudyShortCode,
                                               study_lead_person = input$RenameStudyLeadPerson,
                                               study_longitudinal = sum(input$RenameStudyIsLongitudinal)))
      #PRINT EXIT MESSAGE
      output$StudyReturnMessage <- renderText({paste("Modified Study in the Database", emoji('tada'))})
      
      #REFRESH REFERENCES
      ShowStudies(output, database)
      
      #UPDATE DROPDOWNS
      UpdateStudyDropdowns(database, session)
    }))
  
  # DELETE A STUDY FROM THE DATABASE
  observeEvent(
    input$DeleteStudyAction,
    ({
      #DELETE STUDY
      sampleDB::UpdateReferences(reference = "study",
                                 operation = "delete",
                                 identifier = list(study_short_code = input$DeleteStudyShortCode))
      
      #PRINT EXIT MESSAGE
      output$StudyReturnMessage <- renderText({paste("Deleted Study from the Database", emoji('tada'))})
      
      #REFRESH REFERENCES
      ShowStudies(output, database)
      
    })
  )
  
  #IDLY SHOW STUDIES
  ShowStudies(output, database) 
}

UpdateFreezerDropdowns <- function(database, session){
  updateTextInput(session = session, inputId ="AddFreezerName", value = "", placeholder = "New Name")
  updateTextInput(session = session, inputId = "RenameFreezerName2", value = "", placeholder = "New Name")
  updateSelectInput(session = session, inputId = "RenameFreezerName1", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name))
  updateSelectInput(session = session, inputId = "DeleteFreezerName", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name))
}

UpdateSpecimenTypeDropdowns <- function(database, session){
  updateTextInput(session = session, "AddSpecimenType", value = "", placeholder = "New Name")
  updateTextInput(session = session, "RenameSpecimenType2", value = "", placeholder = "New Name")
  updateSelectInput(session = session, inputId = "RenameSpecimenType1", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
  updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
}

#STUDY UPLATE DROPDOWNS
UpdateStudyDropdowns <- function(database, session){
  updateTextInput(session = session, "AddStudyTitle", value = "", placeholder = "New Title")
  updateTextInput(session = session, "AddStudyDescription", value = "", placeholder = "New Description")
  updateTextInput(session = session, "AddStudyLeadPerson", value = "", placeholder = "New Lead Person")
  updateTextInput(session = session, "AddStudyShortCode", value = "", placeholder = "New Short Code")
  updateCheckboxInput(session = session, "AddStudyIsLongitudinal", value = FALSE)
  updateCheckboxInput(session = session, "AddStudyIsHidden", value = FALSE)
  updateTextInput(session = session, "RenameStudyTitle", value = "",)
  updateTextInput(session = session, "RenameStudyDescription", value = "",)
  updateTextInput(session = session, "RenameStudyLeadPerson", value = "",)
  updateTextInput(session = session, "RenameStudyShortCode", value = "",)
  updateSelectInput(session = session, "ChangeStudyShortCode", choices = c("", sampleDB::CheckTable(database = database, "study")$short_code))
  updateCheckboxInput(session = session, "RenameStudyIsLongitudinal", value = FALSE)
  updateCheckboxInput(session = session, "RenameStudyIsHidden", value = FALSE)
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

ShowSpecimenTypes <- function(output, database){
  output$TableSpecimenType <- DT::renderDataTable({
    sampleDB::CheckTable(database = database, "specimen_type") %>%
      dplyr::select(-c(`created`:id)) %>%
      rename(`Specimen Type` = label)})
}

ShowStudies <- function(output, database){
  output$TableStudy <- DT::renderDataTable({
    sampleDB::CheckTable(database = database, "study") %>%
      dplyr::select(-c(id, created, last_updated)) %>%
      mutate(is_longitudinal = as.logical(is_longitudinal)) %>%
      rename(Title = title,
             Description = description,
            `Study Code` = short_code,
            `Longitudinal` = is_longitudinal,
            `Lead Person` = lead_person)
    }, selection = 'single')
  
  # output$TableStudy <- DT::renderDataTable({
  #   sampleDB::CheckTable(database = database, "study") %>%
  #     dplyr::select(-c(id, created, last_updated, hidden))}, selection = 'single')
}

AddStudyRequirements <- function(input){
  req(input$AddStudyTitle,
      input$AddStudyDescription,
      input$AddStudyShortCode,
      input$AddStudyLeadPerson) 
}

FreezerChangesChecks <- function(input, database, output){
  #PROTECT AGAINST REDUNDANT FREEZER NAMES
  CheckFreezerNameAddUnique <- reactive({helper.CheckFreezerNameUnique("AddFreezer", type.dup = "names", input, database)})
  output$WarningFreezerNameAddUnique <- renderText(CheckFreezerNameAddUnique())
  
  #PREVENT DUPLICATION OF FREEZER NAMES
  CheckFreezerNameChangeUnique <- reactive({helper.CheckFreezerNameUnique("RenameFreezer2", type.dup = "names", input, database)})
  output$WarningFreezerNameChangeUnique <- renderText(CheckFreezerNameChangeUnique())
  
  #PREVENT DELETION OF FREEZER THAT IS IN USE
  CheckFreezerDeletion <- reactive({helper.CheckFreezerDeletion(input, database)})
  output$WarningFreezerDeletion <- renderText(CheckFreezerDeletion()) 
}

SpecimenTypeChangesChecks <- function(input, database, output){
  #PROTECT AGAINST SPECIMEN TYPE NAME DUPLICATION
  CheckAddSpecimenTypeUnique <- reactive({helper.CheckSpecimenTypeUnique("AddSpecimenType", type.dup = "names", input, database)})
  output$WaringAddSpecimenTypeUnique <- renderText(CheckAddSpecimenTypeUnique())
  
  #PREVENT REPLICATE SPECIMEN TYPE NAMES
  CheckChangeSpecimenTypeUnique <- reactive({helper.CheckSpecimenTypeUnique("RenameSpecimenType2", type.dup = "names", input, database)})
  output$WarningChangeSpecimenTypeUnique <- renderText(CheckChangeSpecimenTypeUnique())
  
  #PROTECT AGAINST DELETION OF SPECIMEN TYPE IN USE
  CheckSpecimenTypeDeletion <- reactive({helper.CheckSpecimenTypeDeletion(input, database)})
  output$WarningSpecimenTypeDeletion <- renderText(CheckSpecimenTypeDeletion())
}

StudyChangesChecks <- function(input, database, output){
  #PROTECT AGAINST STUDY NAME DUPLICATION
  CheckStudyAddTitleUnique <- reactive({helper.CheckStudyUnique("AddStudyTitle", type.dup = "title", input, database)})
  output$WarningStudyAddTitleUnique <- renderText(CheckStudyAddTitleUnique())
  
  #PROTECT AGAINST STUDY NAME DUPLICATION
  CheckStudyChangeTitleUnique <- reactive({helper.CheckStudyUnique("RenameStudyTitle", type.dup = "title", input, database)})
  output$WarningStudyChangeTitleUnique <- renderText(CheckStudyChangeTitleUnique())
  
  #PROTECT AGAINST STUDY SHORT CODE DUPLICATION
  CheckStudyAddShortCodeUnique <- reactive({helper.CheckStudyUnique("AddStudyShortCode", type.dup = "short code", input, database)})
  output$WarningStudyAddShortCodeUnique <- renderText(CheckStudyAddShortCodeUnique())
  
  #PROTECT AGAINST STUDY SHORT CODE DUPLICATION
  CheckStudyChangeShortCodeUnique <- reactive({helper.CheckStudyUnique("RenameStudyShortCode", type.dup = "short code", input, database)})
  output$WarningStudyChangeShortCodeUnique <- renderText(CheckStudyChangeShortCodeUnique())
  
  # #CHECK IF BARCODES ARE ALREADY IN DATABASE
  CheckStudyDeletion <- reactive({helper.CheckStudyDeletion(input, database)})
  output$WarnStudyDeletion <- renderText(CheckStudyDeletion())
}

####################################################################################
helper.CheckFreezerNameUnique <- function(id.input, type.dup, input, database){
  toggle <- input$id.input %in% sampleDB::CheckTable(database = database, "location")$location_name
  shinyFeedback::feedbackWarning(id.input, toggle, paste0("Freezer", type.dup, "must be unique"))
}

helper.CheckSpecimenTypeUnique <- function(id.input, type.dup, input, database){
  toggle <- input$id.input %in% c(sampleDB::CheckTable(database = database, "specimen_type") %>% dplyr::select(label) %>% dplyr::pull())
  shinyFeedback::feedbackWarning(id.input, toggle, paste0("Specimen Type", type.dup, "must be unique"))
}

helper.CheckStudyUnique <- function(id.input, type.dup, input, database){
  toggle <- input$AddStudyTitle %in% c(sampleDB::CheckTable(database = database, "study")$title)
  shinyFeedback::feedbackWarning(id.input, toggle, paste0("Study", type.dup, "must be unique"))
}

helper.CheckFreezerDeletion <- function(input, database){
  freezer_id <- sampleDB::CheckTable(database = database, "location") %>% filter(location_name == input$DeleteFreezerName) %>% pull(id)
  toggle <- freezer_id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id
  shinyFeedback::feedbackWarning("DeleteFreezerName", toggle, "Freezer is currently is use") 
}

helper.CheckSpecimenTypeDeletion <- function(input, database){
  specimen_type_id <- sampleDB::CheckTable(database = database, "specimen_type") %>% filter(label == input$DeleteSpecimenType) %>% pull(id)
  toggle <- specimen_type_id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id
  shinyFeedback::feedbackWarning("DeleteSpecimenType", toggle, "Specimen Type is currently is use")
}

helper.CheckStudyDeletion <- function(input, database){
  if(!is.null(input$TableStudy_rows_selected)){
    id <- sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]$"id"
    validate(need(!(id %in% sampleDB::CheckTable(database = database, "study_subject")$study_id),"Study currently in use. Cannot Delete."))}
}