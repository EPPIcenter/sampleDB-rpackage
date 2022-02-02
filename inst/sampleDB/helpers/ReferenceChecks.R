
helper.CheckFreezerNameUnique <- function(id.input, type.dup, input, database){
  toggle <- input$id.input %in% sampleDB::CheckTable(database = database, "location")$description
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
  freezer_id <- CheckTable(database = database, "location") %>% filter(description == input$DeleteFreezer) %>% pull(id)
  toggle <- freezer_id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id
  shinyFeedback::feedbackWarning("DeleteFreezer", toggle, "Freezer is currently is use") 
}

helper.CheckSpecimenTypeDeletion <- function(input, database){
  specimen_type_id <- CheckTable(database = database, "specimen_type") %>% filter(label == input$DeleteSpecimenType) %>% pull(id)
  toggle <- specimen_type_id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id
  shinyFeedback::feedbackWarning("DeleteSpecimenType", toggle, "Specimen Type is currently is use")
}

helper.CheckStudyDeletion <- function(input, database){
  if(!is.null(input$TableStudy_rows_selected)){
    id <- sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]$"id"
    validate(need(!(id %in% sampleDB::CheckTable(database = database, "study_subject")$study_id),"Study currently in use. Cannot Delete."))}
}