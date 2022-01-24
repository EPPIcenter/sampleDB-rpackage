


#SSTUDY UPLATE DROPDOWNS
StudyUpdateAddDropdowns <- function(session){
  updateTextInput(session = session, "AddStudyTitle", value = "", placeholder = "New Title")
  updateTextInput(session = session, "AddStudyDescription", value = "", placeholder = "New Description")
  updateTextInput(session = session, "AddStudyLeadPerson", value = "", placeholder = "New Lead Person")
  updateTextInput(session = session, "AddStudyShortCode", value = "", placeholder = "New Short Code")
  updateCheckboxInput(session = session, "AddStudyIsLongitudinal", value = FALSE)
  updateCheckboxInput(session = session, "AddStudyIsHidden", value = FALSE)  
}

StudyUpdateRenameDropdowns <- function(session){
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