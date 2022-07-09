

DelArchSamples <- function(session, input, database, output, inputs, outputs){
  
  # get search ui elements
  ui_elements <- GetUIDelArchElements()
  
  # create a null value to store the search results
  list.search_results <- NULL
  
  #create empty value to store data for delarch
  val <- reactiveValues(data = NULL)
  
  observe({
    
    #search
    list.search_results <- SearchFunction(input, output, ui_elements)
    
    if(!is.null(list.search_results)){
      storage_container_ids <- list.search_results$id.wetlab_samples
      search_results <- list.search_results$results %>% 
        mutate(`Sample ID` = storage_container_ids) %>% 
        relocate(`Sample ID`) 
      val$data <- search_results
    }
    
    # print search results
    output[[ui_elements$ui.output$SearchResultsTable]] <- DT::renderDataTable({
        if(!is.null(list.search_results)){
          search_results
        }else{
          tibble(a = c(1)) %>% filter(a == 2)
        }
        }, options = DataTableRenderOptions(), rownames = FALSE)
  })
  
  # smart dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = ui_elements$ui.input$SearchByLocation,
                             levelI_ui = ui_elements$ui.input$SearchByLevelI,
                             levelII_ui = ui_elements$ui.input$SearchByLevelII)
  
  # load dropdown using the server -- saves time
  updateSelectizeInput(session, 'DelArchSearchBySubjectUID', 
                       choices = c("", sampleDB::CheckTable(database = database, "study_subject")$subject %>% 
                                     unique()), 
                       server = TRUE)
  
  # handle archive and deletions
  # - archive item
  delarch_val <- reactiveValues(data = NULL, operation = NULL)
  
  observeEvent(input[[ui_elements$ui.input$ArchiveAction]], {
    output[[ui_elements$ui.output$DelArchMessage]] <- NULL
    delarch_val$data <- as.numeric(input[[ui_elements$ui.input$DelArchID]])
    delarch_val$operation = "archive"
    showModal(dataModal(sample_number = delarch_val$data, operation = delarch_val$operation, data = val$data))
  })
  
  observeEvent(input[[ui_elements$ui.input$DelArchVerification]], {
    return_message <- sampleDB::ArchiveAndDeleteSamples(operation = delarch_val$operation,
                                                        sample_id = delarch_val$data,
                                                        verification = F)
    removeModal()
    shinyjs::reset("DelArchID")
    output[[ui_elements$ui.output$DelArchMessage]] <- renderPrint(return_message)
  })
  
  # - delete item
  observeEvent(input[[ui_elements$ui.input$DeleteAction]], {
    output[[ui_elements$ui.output$DelArchMessage]] <- NULL
    delarch_val$data <- as.numeric(input[[ui_elements$ui.input$DelArchID]])
    delarch_val$operation = "delete"
    showModal(dataModal(sample_number = delarch_val$data, operation = delarch_val$operation, data = val$data))
  })

  observeEvent(dbUpdateEvent(), {
    updateSelectInput(session, "DelArchSearchByPlate", label = "Plate Name", choices = c("", dbUpdateEvent()$plate_name))
    updateSelectInput(session, "DelArchSearchByBox", label = "Box Name", choices = c("", dbUpdateEvent()$box_name))
    updateSelectInput(session, "DelArchSearchByRDTBag", label = "Bag Name", choices = c("", dbUpdateEvent()$rdt_bag_name))
    updateSelectInput(session, "DelArchSearchByPaperBag", label = "Bag Name", choices = c("", dbUpdateEvent()$paper_bag_name))

    updateSelectizeInput(session, "DelArchSearchByStudy", "Study", choices = c("", dbUpdateEvent()$study))
    updateSelectizeInput(session, "DelArchSearchBySpecimenType", "Specimen Type", choices = c("", dbUpdateEvent()$specimen_type))
    updateSelectizeInput(session, "DelArchSearchByLocation", "Storage Location", choices = c("", dbUpdateEvent()$location))
  })
    
  # popup window
  dataModal <- function(failed = FALSE, sample_number, operation, data) {
    modalDialog(
      div(tags$b(HTML(paste0("<h3>Are you sure you would like to <b>", toupper(operation), "</b> the following sample?</h3>")), style = "color: #ce2029;")),
      hr(),
      DT::renderDataTable(filter(data, `Sample ID` == sample_number), 
                          options = list(scrollX = T, ordering=F, paging = F, searching = F, info = FALSE), rownames = F),
      footer = tagList(
        actionButton("DelArchVerification", "Yes"),
        modalButton("Cancel")
      )
    )
  }
}

.SearchReset <- function(input){
  observeEvent(input$ClearSearchBarcodes, ({shinyjs::reset("SearchByBarcode")}))
  observeEvent(input$ClearSearchUIDFile, ({shinyjs::reset("SearchBySubjectUIDFile")})) 
}
