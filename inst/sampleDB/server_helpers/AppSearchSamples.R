

SearchWetlabSamples <- function(session, input, database, output, DelArch = FALSE){
  
  # get search ui elements
  ui_elements <- GetUISearchElements()
  
  # create a null value to store the search results
  values <- reactiveValues(data = NULL)
  list.search_results <- NULL
  observe({
    
    #search
    list.search_results <- SearchFunction(input, output, ui_elements)
    message(list.search_results)
    
    if(!is.null(list.search_results)){
      values$data <- list.search_results$results
    } else {
      values$data <- NULL
    }
    
    # print search results
    output[[ui_elements$ui.output$SearchResultsTable]] <- DT::renderDataTable({
      if(!is.null(values$data)){
        values$data
      }else{
        tibble(a = c(1)) %>% filter(a == 2)
      }
      }, options = DataTableRenderOptions(), rownames = FALSE)
    
    # download handler
    output[[ui_elements$ui.output$downloadData]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(values$data, con)
      }
    )
  })

  # smart dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = ui_elements$ui.input$SearchByLocation,
                             levelI_ui = ui_elements$ui.input$SearchByLevelI,
                             levelII_ui = ui_elements$ui.input$SearchByLevelII)
  
  # load dropdown using the server -- saves time
  updateSelectizeInput(session, 'SearchBySubjectUID', 
                       choices = c("", sampleDB::CheckTable(database = database, "study_subject")$subject %>% 
                                     unique()), 
                       server = TRUE)
  
  # clear files
  .SearchReset(input)

  observe({
      updateSelectInput(session, selected = input$SearchByPlate, "SearchByPlate", label = "Plate Name", choices = c("", dbUpdateEvent()$plate_name))
      updateSelectInput(session, selected = input$SearchByBox, "SearchByBox", label = "Box Name", choices = c("", dbUpdateEvent()$box_name))
      updateSelectInput(session, selected = input$SearchByRDTBag, "SearchByRDTBag", label = "Bag Name", choices = c("", dbUpdateEvent()$rdt_bag_name))
      updateSelectInput(session, selected = input$SearchByPaperBag, "SearchByPaperBag", label = "Bag Name", choices = c("", dbUpdateEvent()$paper_bag_name))

      updateSelectizeInput(session, selected = input$SearchByStudy, "SearchByStudy", "Study", choices = c("", names(dbUpdateEvent()$study)))
      updateSelectizeInput(session, selected = input$SearchBySpecimenType, "SearchBySpecimenType", "Specimen Type", choices = c("", dbUpdateEvent()$specimen_type))
      updateSelectizeInput(session, selected = input$SearchByLocation, "SearchByLocation", "Storage Location", choices = c("", dbUpdateEvent()$location))

      # load dropdown using the server -- saves time
      # updateSelectizeInput(session, selected = input$SearchBySubjectUID, 'SearchBySubjectUID', "Study Subject", choices = c("", dbUpdateEvent()$subject) %>% 
      #                                  unique(), server = TRUE)

      updateSelectizeInput(session, "SearchByState", "State", choices = c(dbUpdateEvent()$state))
      updateSelectizeInput(session, "SearchByStatus", "Status", choices = c(dbUpdateEvent()$status))
    })

  observeEvent(input$SearchByStudy, {
    study_id <- match(input$SearchByStudy, dbUpdateEvent()$study)
    req(study_id)
    subject_indexes <- which(unname(dbUpdateEvent()$subject) == study_id)

    updateSelectizeInput(session,
      "SearchBySubjectUID",
      "Study Subject",
      choices = names(dbUpdateEvent()$subject[subject_indexes]),
      server = TRUE)
  })
}

.SearchReset <- function(input){
  observeEvent(input$ClearSearchBarcodes, ({shinyjs::reset("SearchByBarcode")}))
  observeEvent(input$ClearSearchUIDFile, ({shinyjs::reset("SearchBySubjectUIDFile")})) 
}

# SubsetPlateNames <- function(input, database){
#    study_ref_id <- filter(sampleDB::CheckTable(database = database, "study"), short_code %in% input$SearchByStudy)$id
#    study_subject_ref_id <- filter(sampleDB::CheckTable(database = database, "study_subject"), study_id %in% study_ref_id)$id
#    specimen_ref_id <- filter(sampleDB::CheckTable(database = database, "specimen"), study_subject_id %in% study_subject_ref_id)$id
#    storage_container_id <- filter(sampleDB::CheckTable(database = database, "storage_container"), specimen_id %in% specimen_ref_id)$id
#    matrix_tube_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), id %in% storage_container_id)$id
#    
#    plate_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), id %in% matrix_tube_ids)$plate_id %>% unique()
#    plate_names <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), id %in% plate_ids)$uid
#    return(plate_names)
#  }


