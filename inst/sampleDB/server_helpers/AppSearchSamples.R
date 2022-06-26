

SearchWetlabSamples <- function(session, input, database, output, DelArch = FALSE){
  
  # get search ui elements
  ui_elements <- GetUISearchElements()
  
  # create a null value to store the search results
  list.search_results <- NULL
  observe({
    
    #search
    list.search_results <- SearchFunction(input, output, ui_elements)
    
    if(!is.null(list.search_results)){
      search_results <- list.search_results$results
      storage_container_ids <- list.search_results$id.wetlab_samples 
    }
    
    # print search results
    output[[ui_elements$ui.output$SearchResultsTable]] <- DT::renderDataTable({
      if(!is.null(list.search_results)){
        search_results
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
        write.csv(search_results, con)
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

  .CheckDatabaseUpdate(session)
}

.CheckDatabaseUpdate <- function(session, database) {
  getData <- reactivePoll(1000 * 10, session,
    function() file.mtime(sampleDB:::.GetSampleDBPath()),
    function(database) {
      list.data <- list(
          plate_name = sampleDB::CheckTable("matrix_plate")$plate_name,
          box_name = sampleDB::CheckTable("box")$box_name,
          rdt_bag_name = sampleDB::CheckTable("bag")$bag_name,
          paper_bag_name = sampleDB::CheckTable("bag")$bag_name,
          study = sampleDB::CheckTable("study")$short_code,
          specimen_type = sampleDB::CheckTable("specimen_type")$label,
          location = sampleDB::CheckTable("location")$location_name
        )

      return(list.data)
    }
  )

  observeEvent(getData(), {
      updateSelectInput(session, "SearchByPlate", label = "Plate Name", choices = c("", getData()$plate_name))
      updateSelectInput(session, "SearchByBox", label = "Box Name", choices = c("", getData()$box_name))
      updateSelectInput(session, "SearchByRDTBag", label = "Bag Name", choices = c("", getData()$rdt_bag_name))
      updateSelectInput(session, "SearchByPaperBag", label = "Bag Name", choices = c("", getData()$paper_bag_name))

      updateSelectizeInput(session, "SearchByStudy", "Study", choices = c("", getData()$study))
      updateSelectizeInput(session, "SearchBySpecimenType", "Specimen Type", choices = c("", getData()$specimen_type))
      updateSelectizeInput(session, "SearchByLocation", "Storage Location", choices = c("", getData()$location))
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


