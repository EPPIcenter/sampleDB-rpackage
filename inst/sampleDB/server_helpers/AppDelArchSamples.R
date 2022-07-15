

DelArchSamples <- function(session, input, database, output, inputs, outputs){
  
  # get search ui elements
  ui_elements <- GetUIDelArchElements()
  
  # create a null value to store the search results
  list.search_results <- NULL
  
  #create empty value to store data for delarch
  values <- reactiveValues(data = NULL, selected = NULL, operation = NULL)
  
  observe({
    #search
    list.search_results <- SearchFunction(input, output, ui_elements)
    if (!is.null(list.search_results)) {
      search_results <- list.search_results$results
      storage_container_ids <- list.search_results$id.wetlab_samples 
      values$data <- search_results %>%
        mutate(`Sample ID` = storage_container_ids)    
      }
  })

  # print search results
  output[[ui_elements$ui.output$SearchResultsTable]] <- DT::renderDataTable({
    if (!is.null(values$data)) { 
      values$data %>% select(-`Sample ID`)
    } else {
      tibble(a = c(1)) %>% filter(a == 2)
    }
  }, options = DataTableRenderOptions(), rownames = FALSE)

  values$selected <- reactive({ values$data[ input$DelArchSearchResultsTable_rows_selected, ] })

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
  
  observeEvent(input[[ui_elements$ui.input$ArchiveAction]], {
    output[[ui_elements$ui.output$DelArchMessage]] <- NULL
    values$operation <- "archive"
    showModal(dataModal(operation = values$operation, data = values$selected()))
  })
  
  observeEvent(input[[ui_elements$ui.input$DelArchVerification]], {
    return_message <- sampleDB::ArchiveAndDeleteSamples(operation = values$operation,
                                                        data = values$selected(),
                                                        comment = input$DelArchComment,
                                                        status = input$DelArchStatus,
                                                        verification = F)
    removeModal()
    output[[ui_elements$ui.output$DelArchMessage]] <- renderPrint(return_message)
  })
  
  # - delete item
  observeEvent(input[[ui_elements$ui.input$DeleteAction]], {
    output[[ui_elements$ui.output$DelArchMessage]] <- NULL
    values$operation <- "delete"
    showModal(dataModal(operation = values$operation, data = values$selected()))
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

  updateSelectizeInput(session, "DelArchSearchByState", "State", choices = c(Global$DefaultStateSearchTerm, sampleDB::CheckTable("state")$name))
  updateSelectizeInput(session, "DelArchSearchByStatus", "Status", choices = c(Global$DefaultStatusSearchTerm, sampleDB::CheckTable("status")$name))
    
  # popup window
  dataModal <- function(failed = FALSE, operation, data) {
    modalDialog(
      div(tags$b(HTML(paste0("<h3>Are you sure you would like to <b>", toupper(operation), "</b> the following sample?</h3>")), style = "color: #ce2029;")),
      hr(),
      DT::renderDataTable(data, 
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
