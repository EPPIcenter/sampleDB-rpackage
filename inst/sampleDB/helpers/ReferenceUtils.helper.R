



UpdateFreezerDropdowns <- function(database, session){
  updateTextInput(session = session, inputId ="AddFreezer", value = "", placeholder = "New Name")
  updateTextInput(session = session, inputId = "RenameFreezer2", value = "", placeholder = "New Name")
  updateSelectInput(session = session, inputId = "RenameFreezer1", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name))
  updateSelectInput(session = session, inputId = "DeleteFreezer", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name))
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

ShowStudies <- function(output, database){
  output$TableStudy <- DT::renderDataTable({
    sampleDB::CheckTable(database = database, "study") %>%
      dplyr::select(-c(id, created, last_updated, hidden))}, selection = 'single')  
}

ShowFreezers <- function(output, database){
  output$TableFreezer <- DT::renderDataTable({
    sampleDB::CheckTable(database = database, "location") %>%
      dplyr::select(created, location_name) %>%
      rename(`Date Created` = created, Name = location_name) %>%
      relocate(Name, `Date Created`)})
}

ShowSpecimenTypes <- function(output, database){
  output$TableSpecimenType <- DT::renderDataTable({
    sampleDB::CheckTable(database = database, "specimen_type") %>%
      rename(`Date Created` = created) %>%
      relocate(`Date Created`)})
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
  freezer_id <- sampleDB::CheckTable(database = database, "location") %>% filter(location_name == input$DeleteFreezer) %>% pull(id)
  toggle <- freezer_id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id
  shinyFeedback::feedbackWarning("DeleteFreezer", toggle, "Freezer is currently is use") 
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