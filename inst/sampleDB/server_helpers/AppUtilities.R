library(yaml)
library(shinyjs)
# Utility Functions for Main Shiny Functions (e.g. MicronixUpload, MoveSamples, etc.)

# Get UI Elements
GetUISearchElements <- function(){

  ui.input <- list(SearchByLocation = "SearchByLocation",
                   SearchByLevelI = "SearchByLevelI",
                   SearchByLevelII = "SearchByLevelII",
                   dateRange = "dateRange",
                   SearchByStatus = "SearchByStatus",
                   SearchByState = "SearchByState",
                   SearchBySpecimenType = "SearchBySpecimenType",
                   SearchByStudy = "SearchByStudy",
                   SearchBySampleType = "SearchBySampleType",
                   SubjectUIDSearchType = "SubjectUIDSearchType",
                   SearchBySubjectUID = "SearchBySubjectUID",
                   SearchBySubjectUIDFile = "SearchBySubjectUIDFile",
                   SearchByBarcode = "SearchByBarcode",
                   SearchByCryovialLabels = "SearchByCryovialLabels",
                   SearchByRDTLabels = "SearchByRDTLabels",
                   SearchByPaperLabels = "SearchByPaperLabels",
                   SearchByPlate = "SearchByPlate",
                   SearchByBox = "SearchByBox",
                   SearchByManifest = "SearchByManifest",
                   SearchByRDTBag = "SearchByRDTBag",
                   SearchByPaperBag = "SearchByPaperBag",
                   SearchBySingleBarcode = "SearchBySingleBarcode")
  ui.output <- list(SearchResultsTable = "SearchResultsTable",
                    downloadData = "downloadData")

  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIMoveElements <- function(sample_type, msg = NULL){

  if(sample_type == "micronix"){
    ui.input <-  list(MicronixFileType = "MoveFileType",
                      MoveDataSet = "MoveDataSet",
                      CreateEmptyManifestLocation = "CreateEmptyManifestLocation",
                      CreateEmptyManifestLevelI = "CreateEmptyManifestLevelI",
                      CreateEmptyManifestLevelII = "CreateEmptyManifestLevelII")
    ui.output <- list(WarningLogisticalColnames = "WarningMoveLogisticalColnames",
                      WarningMoveBarcodesExist = "WarningMoveBarcodesExist")
  }

  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIFreezerElements <- function(msg = NULL){

  ui.input <- list(AddFreezerName = "AddFreezerName",
                   AddFreezerType = "AddFreezerType",
                   AddFreezerDesc = "AddFreezerDesc",
                   AddFreezerLevel_I = "AddFreezerLevel_I",
                   AddFreezerLevel_II = "AddFreezerLevel_II",
                   AddFreezerAction = "AddFreezerAction",
                   RenameFreezerName1 = "RenameFreezerName1",
                   RenameFreezerLevelI1 = "RenameFreezerLevelI1",
                   RenameFreezerLevelII1 = "RenameFreezerLevelII1",
                   RenameFreezerName2 = "RenameFreezerName2",
                   RenameFreezerType2 = "RenameFreezerType2",
                   RenameFreezerLevelI2 = "RenameFreezerLevelI2",
                   RenameFreezerLevelII2 = "RenameFreezerLevelII2",
                   RenameFreezerAction = "RenameFreezerAction",
                   DeleteFreezerName = "DeleteFreezerName",
                   DeleteFreezerLevelI = "DeleteFreezerLevelI",
                   DeleteFreezerLevelII = "DeleteFreezerLevelII",
                   DeleteFreezerAction = "DeleteFreezerAction")
  ui.output = list(WarningFreezerNameAddUnique = "WarningFreezerNameAddUnique",
                   WarningFreezerNameChangeUnique = "WarningFreezerNameChangeUnique",
                   WarningFreezerDeletion = "WarningFreezerDeletion",
                   FreezerReturnMessage = "FreezerReturnMessage")

  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUISpecimenTypeElements <- function(msg = NULL){

  ui.input <- list(AddSpecimenType = "AddSpecimenType",
                   AddSpecimenTypeAction = "AddSpecimenTypeAction",
                   RenameSpecimenType1 = "RenameSpecimenType1",
                   RenameSpecimenType2 = "RenameSpecimenType2",
                   RenameSpecimenTypeAction = "RenameSpecimenTypeAction",
                   DeleteSpecimenType = "DeleteSpecimenType",
                   DeleteSpecimenTypeAction = "DeleteSpecimenTypeAction")
  ui.output = list(WaringAddSpecimenTypeUnique = "WaringAddSpecimenTypeUnique",
                   WarningChangeSpecimenTypeUnique = "WarningChangeSpecimenTypeUnique",
                   WarningSpecimenTypeDeletion = "WarningSpecimenTypeDeletion",
                   SpecimenReturnMessage = "SpecimenReturnMessage")

  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIStudiesElements <- function(msg = NULL){

  ui.input <-  list(AddStudyTitle = "AddStudyTitle",
                    AddStudyDescription = "AddStudyDescription",
                    AddStudyLeadPerson = "AddStudyLeadPerson",
                    AddStudyShortCode = "AddStudyShortCode",
                    AddStudyIsLongitudinal = "AddStudyIsLongitudinal",
                    AddStudyAction = "AddStudyAction",
                    ChangeStudyShortCode = "ChangeStudyShortCode",
                    AddStudyIsControl = "AddStudyIsControl",
                    RenameStudyTitle = "RenameStudyTitle",
                    RenameStudyDescription = "RenameStudyDescription",
                    RenameStudyLeadPerson = "RenameStudyLeadPerson",
                    RenameStudyShortCode = "RenameStudyShortCode",
                    RenameStudyIsLongitudinal = "RenameStudyIsLongitudinal",
                    RenameStudyAction = "RenameStudyAction",
                    DeleteStudyShortCode = "DeleteStudyShortCode",
                    DeleteStudyAction = "DeleteStudyAction")
  ui.output <- list(WarningStudyAddTitleUnique = "WarningStudyAddTitleUnique",
                    WarningStudyAddShortCodeUnique = "WarningStudyAddShortCodeUnique",
                    WarningStudyChangeTitleUnique = "WarningStudyChangeTitleUnique",
                    WarningStudyChangeShortCodeUnique = "WarningStudyChangeShortCodeUnique",
                    WarnStudyDeletion = "WarnStudyDeletion",
                    StudyReturnMessage = "StudyReturnMessage")

  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

GetUIDelArchElements <- function(){

  ui.input <- list(SearchByLocation = "DelArchSearchByLocation",
                   SearchByLevelI = "DelArchSearchByLevelI",
                   SearchByLevelII = "DelArchSearchByLevelII",
                   dateRange = "DelArchdateRange",
                   SearchByStatus = "DelArchSearchByStatus",
                   SearchByState = "DelArchSearchByState",
                   SearchBySpecimenType = "DelArchSearchBySpecimenType",
                   SearchByStudy = "DelArchSearchByStudy",
                   SearchBySampleType = "DelArchSearchBySampleType",
                   SubjectUIDSearchType = "DelArchSubjectUIDSearchType",
                   SearchBySubjectUID = "DelArchSearchBySubjectUID",
                   SearchBySubjectUIDFile = "DelArchSearchBySubjectUIDFile",
                   SearchByBarcode = "DelArchSearchByBarcode",
                   SearchByCryovialLabels = "DelArchSearchByCryovialLabels",
                   SearchByRDTLabels = "DelArchSearchByRDTLabels",
                   SearchByPaperLabels = "DelArchSearchByPaperLabels",
                   SearchByPlate = "DelArchSearchByPlate",
                   SearchByBox = "DelArchSearchByBox",
                   SearchByRDTBag = "DelArchSearchByRDTBag",
                   SearchByPaperBag = "DelArchSearchByPaperBag",
                   SearchByManifest = "DelArchSearchByManifest",
                   ArchiveAction = "ArchiveAction",
                   DeleteAction = "DeleteAction",
                   DelArchComment = "DelArchComment",
                   DelArchStatus = "DelArchStatus",
                   DelArchVerification = "DelArchVerification",
                   SearchBySingleBarcode = "DelArchSearchBySingleBarcode")
  ui.output <- list(SearchResultsTable = "DelArchSearchResultsTable",
                    SelectedRowsTable = "DelArchSearchResultsTable",
                    DelArchMessage = "DelArchMessage")

  ui_elements <- list(ui.input = ui.input, ui.output = ui.output)
  return(ui_elements)
}

FreezerChangesChecks <- function(input, database, output, ui_elements){
  # warn if freezer name is redundant
  output[[ui_elements$ui.output$WarningFreezerNameAddUnique]] <- renderText({
    out <- CheckFreezerNameIsUnique(input, database,
                                     freezer_address = list(freezer_name = input[[ui_elements$ui.input$AddFreezerName]],
                                                            freezer_levelI = input[[ui_elements$ui.input$AddFreezerLevel_I]],
                                                            freezer_levelII = input[[ui_elements$ui.input$AddFreezerLevel_II]]))
    validate(need(out, "ERROR:\nFreezer address must be unique"))
  })

  # warn if freezer name is redundant
  output[[ui_elements$ui.output$WarningFreezerNameChangeUnique]] <- renderText({
    out <- CheckFreezerNameIsUnique(input, database,
                              freezer_address = list(freezer_name = input[[ui_elements$ui.input$RenameFreezerName2]],
                                                        freezer_levelI = input[[ui_elements$ui.input$RenameFreezerLevelI2]],
                                                        freezer_levelII = input[[ui_elements$ui.input$RenameFreezerLevelII2]]))
    validate(need(out, "ERROR:\nFreezer address must be unique"))
  })

  #warn deletion of freezer in use
  output$WarningFreezerDeletion <- renderText({
    out <- CheckFreezerDeletion(input, database,
                          freezer_address = list(freezer_name = input[[ui_elements$ui.input$DeleteFreezerName]],
                                                    freezer_levelI = input[[ui_elements$ui.input$DeleteFreezerLevelI]],
                                                    freezer_levelII = input[[ui_elements$ui.input$DeleteFreezerLevelII]]))
    validate(need(out, "ERROR:\nFreezer is currently in use"))
  })
}

SpecimenTypeChangesChecks <- function(input, database, output, ui_elements){
  # warn if specimen type is redundant
  output[[ui_elements$ui.output$WaringAddSpecimenTypeUnique]] <- renderText({
    out <- CheckSpecimenTypeUnique(input, database, specimen_type = input[[ui_elements$ui.input$AddSpecimenType]])
    validate(need(out, "ERROR:\nSpecimen type is not unique"))
  })

  # warn if specimen type is redundant
  output[[ui_elements$ui.output$WarningChangeSpecimenTypeUnique]] <- renderText({
    out <- CheckSpecimenTypeUnique(input, database, specimen_type = input[[ui_elements$ui.input$RenameSpecimenType2]])
    validate(need(out, "ERROR:\nSpecimen type is not unique"))
  })

  #warn deletion of specimen type in use
  output[[ui_elements$ui.output$WarningSpecimenTypeDeletion]] <- renderText({
    out <- CheckSpecimenTypeDeletion(input, database , specimen_type = input[[ui_elements$ui.input$DeleteSpecimenType]])
    validate(need(out, "ERROR:\nSpecimen type is currently in use"))
  })
}

StudyChangesChecks <- function(input, database, output, ui_elements) {
  #warn against study title duplication
  output[[ui_elements$ui.output$WarningStudyAddTitleUnique]] <- renderText({
    out <- CheckStudyTitleIsUnique(study_title = input[[ui_elements$ui.input$AddStudyTitle]], input = input, database = database)
    validate(need(out, "ERROR:\nStudy title is not unique"))
  })

  #warn against study title duplication
  output[[ui_elements$ui.output$WarningStudyChangeTitleUnique]] <- renderText({
    out <- CheckStudyTitleIsUnique(study_title = input[[ui_elements$ui.input$RenameStudyTitle]], input = input, database = database)
    validate(need(out, "ERROR:\nStudy title is not unique"))
  })

  #warn against study code duplication
  output[[ui_elements$ui.output$WarningStudyAddShortCodeUnique]] <- renderText({
    out <- CheckStudyShortCodeIsUnique(study_short_code = input[[ui_elements$ui.input$AddStudyShortCode]], input = input, database = database)
    validate(need(out, "ERROR:\nStudy code is not unique"))
  })

  #warn against study code duplication
  output[[ui_elements$ui.output$WarningStudyChangeShortCodeUnique]] <- renderText({
    out <- CheckStudyShortCodeIsUnique(study_short_code = input[[ui_elements$ui.input$RenameStudyShortCode]], input = input, database = database)
    validate(need(out, "ERROR:\nStudy code is not unique"))
  })

  #warn against study code deletion
  output[[ui_elements$ui.output$WarnStudyDeletion]] <- renderText({
    out <- CheckStudyDeletion(study_ui = input[[ui_elements$ui.input$DeleteStudyShortCode]], input, database)
    validate(need(out, "ERROR:\nStudy is currently in use"))
  })
}

# General Operations
SmartFreezerDropdownFilter <- function(database, session, input = input, location_ui, levelI_ui, levelII_ui){

  observe({
    if(!is.null(input[[location_ui]]) && input[[location_ui]] != ""){
      tmp_table.location <- filter(CheckTable(database = database, "location"), location_root == input[[location_ui]])
      updateSelectInput(session, levelI_ui, label = NULL, choices = c("", tmp_table.location$level_I) %>% sort())
      levelII_choices <- list(`working baskets` = grep("working", tmp_table.location$level_II, value = T) %>% sort(),
                              `non-working baskets` = grep("working", tmp_table.location$level_II, value = T, invert = T) %>% sort())
      updateSelectInput(session, levelII_ui, label = NULL, choices = c("", levelII_choices))
    }else{
      updateSelectInput(session, levelI_ui, label = NULL, choices = c(""))
      updateSelectInput(session, levelII_ui, label = NULL, choices = c(""))
    }
  })
}

DataTableRenderOptions <- function(){
  out <- list(
    searching = T,
    server = F,
    paging = T,
    pageLength = 10,
    lengthMenu = c(10, 20, 50, 100),
    language = list(zeroRecords = "There are no wetlab samples that match this search."),
    rowCallback = JS(
      rowCallback <- c(
        "function(row, data){",
        "  for(var i=0; i<data.length; i++){",
        "    if(data[i] === null){",
        "      $('td:eq('+i+')', row).html('NA')",
        "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
        "    }",
        "  }",
        "}"
      )
    ))
  return(out)
}

SetFreezerAddRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$AddFreezerName]],
      input[[ui_elements$ui.input$AddFreezerType]],
      input[[ui_elements$ui.input$AddFreezerLevel_I]],
      input[[ui_elements$ui.input$AddFreezerLevel_II]],
      CheckFreezerNameIsUnique(input, database,
                                           freezer_address = list(freezer_name = input[[ui_elements$ui.input$AddFreezerName]],
                                                                  freezer_levelI = input[[ui_elements$ui.input$AddFreezerLevel_I]],
                                                                  freezer_levelII = input[[ui_elements$ui.input$AddFreezerLevel_II]])) == TRUE)
}

SetFreezerChangeRequirements <- function(input, database, ui_elements){
  req(sum(c(input[[ui_elements$ui.input$RenameFreezerName1]], input[[ui_elements$ui.input$RenameFreezerLevelI1]], input[[ui_elements$ui.input$RenameFreezerLevelII1]]) != "") > 0,
      sum(c(input[[ui_elements$ui.input$RenameFreezerName2]], input[[ui_elements$ui.input$RenameFreezerType2]], input[[ui_elements$ui.input$RenameFreezerLevelI2]], input[[ui_elements$ui.input$RenameFreezerLevelI2]]) != "") > 0,
      CheckFreezerNameIsUnique(input, database,
                                           freezer_address = list(freezer_name = input[[ui_elements$ui.input$RenameFreezerName2]],
                                                                  freezer_levelI = input[[ui_elements$ui.input$RenameFreezerLevelI2]],
                                                                  freezer_levelII = input[[ui_elements$ui.input$RenameFreezerLevelII2]])) == TRUE)
}

SetFreezerDeleteRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$DeleteFreezerName]],
      input[[ui_elements$ui.input$DeleteFreezerLevelI]],
      input[[ui_elements$ui.input$DeleteFreezerLevelII]],
      CheckFreezerDeletion(input, database,
                                       freezer_address = list(freezer_name = input[[ui_elements$ui.input$DeleteFreezerName]],
                                                              freezer_levelI = input[[ui_elements$ui.input$DeleteFreezerLevelI]],
                                                              freezer_levelII = input[[ui_elements$ui.input$DeleteFreezerLevelII]])) == TRUE)
}

SetAddStudyRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$AddStudyTitle]],
      input[[ui_elements$ui.input$AddStudyDescription]],
      input[[ui_elements$ui.input$AddStudyShortCode]],
      input[[ui_elements$ui.input$AddStudyLeadPerson]],
      CheckStudyTitleIsUnique(input = input, database = database,
                                          study_title = input[[ui_elements$ui.input$AddStudyTitle]]) == TRUE,
      CheckStudyShortCodeIsUnique(input = input, database = database,
                                              study_short_code = input[[ui_elements$ui.input$AddStudyShortCode]]) == TRUE)
}

SetChangeStudyRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$ChangeStudyShortCode]],
      input[[ui_elements$ui.input$RenameStudyTitle]],
      input[[ui_elements$ui.input$RenameStudyDescription]],
      input[[ui_elements$ui.input$RenameStudyShortCode]],
      input[[ui_elements$ui.input$RenameStudyLeadPerson]],
      CheckStudyTitleIsUnique(input = input, database = database,
                                          study_title = input[[ui_elements$ui.input$RenameStudyTitle]]) == TRUE)
}

SetDeleteStudyRequirements <- function(input, database, ui_elements){
  req(input[[ui_elements$ui.input$DeleteStudyShortCode]],
      CheckStudyDeletion(input, database,
                                     study_ui = input[[ui_elements$ui.input$DeleteStudyShortCode]]) == TRUE)
}

MoveReset <- function(input, output){
  observeEvent(
    input$ClearMoveForm,
    ({
      shinyjs::reset("MoveDataSet")
      output$MoveReturnMessage2 <- renderText({""})}))
}

UpdateFreezerDropdowns <- function(database, session){
  shinyjs::reset("AddFreezerName")
  shinyjs::reset("AddFreezerType")
  shinyjs::reset("AddFreezerLevel_I")
  shinyjs::reset("AddFreezerLevel_II")
  shinyjs::reset("RenameFreezerName2")
  updateSelectInput(session = session, inputId = "RenameFreezerName1", choices = c("", CheckTable(database = database, "location")$location_root))
  updateSelectInput(session = session, inputId = "DeleteFreezerName", choices = c("", CheckTable(database = database, "location")$location_root))
}

UpdateSpecimenTypeDropdowns <- function(database, session){
  shinyjs::reset("AddSpecimenType")
  shinyjs::reset("RenameSpecimenType2")
  updateSelectInput(session = session, inputId = "RenameSpecimenType1", choices = c("", CheckTable(database = database, "specimen_type")$name))
  updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = c("", CheckTable(database = database, "specimen_type")$name))
}

UpdateStudyDropdowns <- function(database, session){
  shinyjs::reset("AddStudyTitle")
  shinyjs::reset("AddStudyDescription")
  shinyjs::reset("AddStudyLeadPerson")
  shinyjs::reset("AddStudyShortCode")
  shinyjs::reset("AddStudyIsLongitudinal")
  shinyjs::reset("AddStudyIsHidden")
  shinyjs::reset("RenameStudyTitle")
  shinyjs::reset("RenameStudyDescription")
  shinyjs::reset("RenameStudyLeadPerson")
  shinyjs::reset("RenameStudyShortCode")
  shinyjs::reset("DeleteStudyShortCode")
  updateSelectInput(session = session, "ChangeStudyShortCode", choices = c("", CheckTable(database = database, "study")$short_code))
  updateCheckboxInput(session = session, "RenameStudyIsLongitudinal", value = FALSE)
  updateCheckboxInput(session = session, "RenameStudyIsHidden", value = FALSE)
  updateSelectInput(session = session, "DeleteStudyShortCode", choices = c("", CheckTable(database = database, "study")$short_code))
}

heckUploadContainerBarcodeDuplication <- function(plate_barcode, database){

  if(plate_barcode != "" && !is.null(plate_barcode)){
    out <- all(!(plate_barcode %in% c(CheckTable(database = database, "micronix_plate")$plate_barcode)))
  }else{
    out <- TRUE
  }
  return(out)
}
#Freezer Checks
CheckFreezerNameIsUnique <- function(input, database, freezer_address){

  freezer_address_dup_test <- filter(CheckTable("location"),
                                     location_root == freezer_address$freezer_name,
                                     level_I == freezer_address$freezer_levelI,
                                     level_II == freezer_address$freezer_levelII) %>% nrow()

  if(freezer_address_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

CheckFreezerDeletion <- function(input, database, freezer_address){
  num_items_at_address <- 0

  freezer_address <- filter(CheckTable(database = database, "location"),
                            location_root == freezer_address$freezer_name,
                            level_I == freezer_address$freezer_levelI,
                            level_II == freezer_address$freezer_levelII)
  if(length(freezer_address$id) > 0){
    items_at_address <- filter(CheckTable(database = database, "micronix_plate"), location_id == freezer_address$id)
    num_items_at_address <- items_at_address %>% nrow()
  }
  if(num_items_at_address > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

#Specimen Type Check
CheckSpecimenTypeUnique <- function(input, database, specimen_type){
  specimen_type_dup_test <- filter(CheckTable(database = database, "specimen_type"), name == specimen_type) %>% nrow()
  if(specimen_type_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}


CheckSpecimenTypeDeletion <- function(input, database, specimen_type){
  num_items_of_specimen_type <- 0
  specimen_type <- filter(CheckTable(database = database, "specimen_type"), name == specimen_type)
  if(length(specimen_type$id) > 0){
    num_items_of_specimen_type <- filter(CheckTable(database = database, "specimen"), specimen_type_id == specimen_type$id) %>% nrow()
  }
  if(num_items_of_specimen_type > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

#Study Check
CheckStudyTitleIsUnique <- function(study_title, test, input, database){
  study_title_dup_test <- filter(CheckTable(database = database, "study"), title == study_title) %>% nrow()
  if(study_title_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

CheckStudyShortCodeIsUnique <- function(study_short_code, test, input, database){
  study_short_code_dup_test <- filter(CheckTable(database = database, "study"), short_code == study_short_code) %>% nrow()
  if(study_short_code_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

CheckStudyDeletion <- function(study_ui, input, database){
  num_items_of_studies <- 0
  studies <- filter(CheckTable(database = database, "study"), short_code == study_ui)
  if(length(studies$id) > 0){
    num_items_of_studies <- filter(CheckTable(database = database, "study_subject"), study_id == studies$id) %>% nrow()
  }
  if(num_items_of_studies > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

#upload a new micronix plate

ViewArchiveStatuses <- function(database) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
  out_table <- RSQLite::dbGetQuery(conn, "SELECT * FROM view_archive_statuses") %>% tibble()
  RSQLite::dbDisconnect(conn)
  return(out_table)
}



# You can now call handle_formatting_error whenever you encounter a formatting error.


show_formatting_error_modal <- function(error) {

  message("Preparing format error modal.")

  df <- error$data %>%
    dplyr::rename(
      Column = column, 
      Reason = reason,
      `Triggered By` = trigger
    ) %>%
    reactable(.)
  
  showModal(
    modalDialog(
      size = "m",
      title = "Formatting Error Detected",
      error$message,
      tags$hr(),
      renderReactable({ df }),
      footer = modalButton("Exit")
    )
  )
}

# Create a function to generate reactable for displaying errors
generate_error_reactable <- function(error_collection, index = 1) {

  specific_error <- error_collection$error_data_list[[index]]
  selected_cols <- c("RowNumber", specific_error$columns)
  
  error_details <- error_collection$get_error_details_by_index(index)
  
  reactable(
    error_details,
    outlined = TRUE,
    striped = TRUE,
    theme = reactableTheme(
      headerStyle = list(
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
        borderColor = "#555"
      )
    ),
    defaultColDef = colDef(na = "-", align = "center")
  )
}

# Function to display validation error modal
show_validation_error_modal <- function(error) {
  
  message("Preparing validation error modal.")
  
  error_collection <- error$data
  
  if (inherits(error_collection, "ErrorData")) {
    # Your code to generate a single table for the ErrorData class
    
    errors_df <- data.frame(
      Description = error_collection$description,
      Columns = toString(error_collection$columns),
      Rows = toString(error_collection$rows)
    )
    
    main_table <- reactable(errors_df)
    
  } else if (inherits(error_collection, "ValidationErrorCollection")) {
    # Your code to generate multiple tables for the ValidationErrorCollection class
    
    # Extracting unique error descriptions
    errors <- unique(sapply(error_collection$error_data_list, function(x) x$description))
    errors_df <- data.frame(Error = errors)
    
    main_table <- reactable(
      errors_df,
      details = function(index) {
        htmltools::div(
          style = "padding: 1rem",
          generate_error_reactable(error_collection, index)
        )
      }
    )
  } else {
    stop("Unknown error collection type.")
  }
  
  # Display the modal with the main error table
  showModal(
    modalDialog(
      size = "l",
      title = error$title,
      tags$p("One or more rows had invalid or missing data. See the errors below and expand them to see which rows caused this error."),
      tags$p("Press the button below to download your file with annotations"),
      downloadButton("ErrorFileDownload"),
      tags$hr(),
      renderReactable({ main_table }),
      footer = modalButton("Exit")
    )
  )
}



show_general_error_modal <- function(error) {
  errcall = ifelse(is.null(error$call), "No function call information available", error$call)
  errmsg = ifelse(is.null(error$message), "No message available", error$message)
  showModal(
    modalDialog(
      size = "l",
      title = error$title,
      tags$p("Something went wrong - contact the app author, and report the error message below."),
      tags$hr(),
      tags$p(errcall),
      tags$p(errmsg),
      footer = modalButton("Exit")
    )
  )
}


#' Collate User Input for Sample Data
#'
#' This function combines the outputs of both `get_location_by_sample` and 
#' `get_container_by_sample`, and then updates the resultant named list with 
#' input values.
#' 
#' @param sample_type The type of sample to get data for.
#' @param input A list of user inputs that includes location and container information.
#' @param sample_file The JSON file that contains sample information.
#' @param app_file The JSON file that contains app-related data.
#' 
#' @return A list that combines location and container data, and updates with user input.
#' @examples
#' # With mock input data
#' input <- list(UploadManifestName = "MyPlate", 
#'               UploadLocationRoot = "Minus20Freezer", 
#'               UploadLocationLevelI = "Shelf1", 
#'               UploadLocationLevelII = "Basket2")
#' collate_user_input_sample_data("micronix", input)
#' 
#' @export
collate_user_input_sample_data <- function(sample_type, 
                                           input, 
                                           sample_file = "samples.json", 
                                           app_file = "app.json") {

  # Get data from both functions
  location_data <- get_location_by_sample(sample_type, sample_file, app_file)
  container_data <- get_container_by_sample(sample_type, sample_file, app_file)
  
  collated_user_data <- NULL
  
  # Update container name
  if (is.character(input$UploadManifestName) && input$UploadManifestName != "") {
    collated_user_data <- setNames(list(input$UploadManifestName), container_data[["container_name_key"]])
  }

  # Update location data
  if (is.character(input$UploadLocationRoot) && input$UploadLocationRoot != "") {
    collated_user_data <- c(collated_user_data,  setNames(list(input$UploadLocationRoot), location_data[["location_root"]]))
  }
  if (is.character(input$UploadLocationLevelI) && input$UploadLocationLevelI != "") {
    collated_user_data <- c(collated_user_data, setNames(list(input$UploadLocationLevelI), location_data[["level_i"]]))
  }
  if (is.character(input$UploadLocationLevelII) && input$UploadLocationLevelII != "") {
    collated_user_data <- c(collated_user_data, setNames(list(input$UploadLocationLevelII), location_data[["level_ii"]]))
  }

  return(collated_user_data)
}

#' Create a container to store filters
#' 
#' This function creates a reactive value that stores filters for the app.
#' 
#' @param defaults A list of default filters to use.
createFilterSetReactive <- function(defaults = list()) {
  rv <- reactiveVal(defaults)
  
  list(
    get = function() { rv() },
    set = function(new_filters) {
      rv(new_filters)
    },
    insert = function(new_filters) {
      existing_filters <- rv()
      updated_filters <- modifyList(existing_filters, new_filters)
      rv(updated_filters)
    },
    remove = function(filters_to_remove) {
      existing_filters <- rv()
      updated_filters <- existing_filters[!names(existing_filters) %in% filters_to_remove]
      rv(updated_filters)
    },
    clear = function() { rv(list()) },
    reset = function() { rv(defaults) },
    filter = function(df) {
      filters <- rv()
      if (length(filters) > 0) {
          exprs <- map2(names(filters), filters, ~ rlang::expr(!!rlang::sym(.x) == !!.y))
          df <- df %>% filter(!!!exprs)
      }
      return(df)
    }
  )
}

#' Process Filters for Shiny App
#'
#' This function processes filter inputs for a Shiny app. It removes empty or NULL values,
#' inserts new filters into a given filter set, and removes any outdated filters.
#'
#' @param input_filters A list of filter inputs to be processed.
#' @param filter_set A reactive object that contains the current state of filters.
#'
#' @return This function is called for its side effects and does not return a value.
#' @examples
#' \dontrun{
#' observe({
#'   input_filters <- list(
#'     strain = input$InputControlSearchStrain,
#'     percentage = input$InputControlSearchPercentage,
#'     composition_types = input$InputControlSearchCompositionTypes
#'   )
#'   process_filters(input_filters, composition_filter_set)
#' })
#' }
#' @export
process_filters <- function(input_filters, filter_set) {

  # Modified the inner function to handle NAs explicitly
  new_filters <- purrr::map(input_filters, function(x) {
    purrr::discard(x, ~ is.null(.x) | is.na(.x) | .x == "" | length(.x) == 0)
  })  
  new_filters <- purrr::discard(new_filters, ~ is.null(.x) | length(.x) == 0)

  # Insert new filters
  if (length(new_filters) > 0) {
    filter_set$insert(new_filters)
  }

  # Get existing filters
  existing_filters <- filter_set$get()

  # Identify filters to remove
  filters_to_remove <- setdiff(names(existing_filters), names(new_filters))
  
  # Remove selected filters
  if (length(filters_to_remove) > 0) {
    filter_set$remove(filters_to_remove)
  }
}

#' Show a an upload success notification
show_success_notification <- function(session, category, res) {
  # Customize your success message
  category <- ifelse(res > 1, paste0(category, "s"), category)

  msg <- sprintf("Success! %d %s added.", res, category)
  showNotification(
    msg, 
    duration = 3,
    id = "GenericUploadSuccess", 
    type = "default",
    session = session
  )
}