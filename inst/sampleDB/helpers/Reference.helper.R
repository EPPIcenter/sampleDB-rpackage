



UpdateFreezerDropdowns <- function(database, session){
  updateTextInput(session = session, inputId ="AddFreezer", value = "", placeholder = "New Name")
  updateTextInput(session = session, inputId = "RenameFreezer2", value = "", placeholder = "New Name")
  updateSelectInput(session = session, inputId = "RenameFreezer1", choices = c("", sampleDB::CheckTable(database = database, "location")$description))
  updateSelectInput(session = session, inputId = "DeleteFreezer", choices = c("", sampleDB::CheckTable(database = database, "location")$description))
}

UpdateSpecimenTypeDropdowns <- function(database, session){
  updateTextInput(session = session, "AddSpecimenType", value = "", placeholder = "New Name")
  updateTextInput(session = session, "RenameSpecimenType2", value = "", placeholder = "New Name")
  updateSelectInput(session = session, inputId = "RenameSpecimenType1", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
  updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
}

#STUDY UPLATE DROPDOWNS
UpdateStudyDropdowns <- function(session){
  updateTextInput(session = session, "AddStudyTitle", value = "", placeholder = "New Title")
  updateTextInput(session = session, "AddStudyDescription", value = "", placeholder = "New Description")
  updateTextInput(session = session, "AddStudyLeadPerson", value = "", placeholder = "New Lead Person")
  updateTextInput(session = session, "AddStudyShortCode", value = "", placeholder = "New Short Code")
  updateCheckboxInput(session = session, "AddStudyIsLongitudinal", value = FALSE)
  updateCheckboxInput(session = session, "AddStudyIsHidden", value = FALSE)
  updateTextInput(session = session, "RenameStudyTitle", value = "", placeholder = "New Title")
  updateTextInput(session = session, "RenameStudyDescription", value = "", placeholder = "New Description")
  updateTextInput(session = session, "RenameStudyLeadPerson", value = "", placeholder = "New Lead Person")
  updateTextInput(session = session, "RenameStudyShortCode", value = "", placeholder = "New Short Code")
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
      dplyr::select(created, description) %>%
      rename(`Date Created` = created, Name = description) %>%
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