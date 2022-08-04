

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
    } else {
      values$data <- NULL
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
  
  # handle archive and deletions
  # - archive item
  
  observeEvent(input[[ui_elements$ui.input$ArchiveAction]], {
    output[[ui_elements$ui.output$DelArchMessage]] <- NULL
    values$operation <- "archive"
    req(input$DelArchStatus)

    showModal(dataModal(operation = values$operation, data = values$selected()))
  })
  
  observeEvent(input[[ui_elements$ui.input$DelArchVerification]], {
    shinyjs::disable("DelArchVerification")
    showNotification("Working...", id = "ArchDelNotification", type = "message", action = NULL, duration = 5, closeButton = FALSE)
    return_message <- sampleDB::ArchiveAndDeleteSamples(operation = values$operation,
                                                        data = values$selected(),
                                                        comment = input$DelArchComment,
                                                        status = input$DelArchStatus,
                                                        verification = F)

    database <- file.path(Sys.getenv("SDB_PATH"))
    if (values$operation %in% "archive") {
      updated_values <- values$data %>%
        select(`Sample ID`) %>%
        filter(`Sample ID` %in% values$selected()$`Sample ID`) %>%
        inner_join(CheckTable(database = database, table = "storage_container"), by = c("Sample ID" = "id")) %>%
        inner_join(CheckTable(database = database, table = "state"), by = c("state_id" = "id")) %>%
        inner_join(CheckTable(database = database, table = "status"), by = c("status_id" = "id")) %>%
        rename(
          State = name.x,
          Status = name.y,
          Comment = comment
        )

      values$data <- values$data %>%
        mutate(
          State = as.factor(replace(as.character(State), `Sample ID` %in% updated_values$`Sample ID`, updated_values$State)),
          Status = as.factor(replace(as.character(Status), `Sample ID` %in% updated_values$`Sample ID`, updated_values$Status)),
          Comment = as.factor(replace(as.character(Comment), `Sample ID` %in% updated_values$`Sample ID`, updated_values$Comment))
        )
    } else if (values$operation %in% "delete") {
      values$data <- values$data[!values$data$`Sample ID` %in% values$selected()$`Sample ID`, ]
      if (nrow(values$data) == 0) {
        values$data <- NULL
      }
    }
    removeNotification(id = "ArchDelNotification")
    removeModal()
    output[[ui_elements$ui.output$DelArchMessage]] <- renderPrint(return_message)
  })
  
  # - delete item
  observeEvent(input[[ui_elements$ui.input$DeleteAction]], {
    output[[ui_elements$ui.output$DelArchMessage]] <- NULL
    values$operation <- "delete"
    showModal(dataModal(operation = values$operation, data = values$selected()))
  })

  observe({
    updateSelectInput(session, selected = input$DelArchSearchByPlate, "DelArchSearchByPlate", label = "Plate Name", choices = c("", dbUpdateEvent()$plate_name))
    updateSelectInput(session, selected = input$DelArchSearchByBox, "DelArchSearchByBox", label = "Box Name", choices = c("", dbUpdateEvent()$box_name))
    updateSelectInput(session, selected = input$DelArchSearchByRDTBag, "DelArchSearchByRDTBag", label = "Bag Name", choices = c("", dbUpdateEvent()$rdt_bag_name))
    updateSelectInput(session, selected = input$DelArchSearchByPaperBag, "DelArchSearchByPaperBag", label = "Bag Name", choices = c("", dbUpdateEvent()$paper_bag_name))

    updateSelectizeInput(session, selected = input$DelArchSearchByStudy, "DelArchSearchByStudy", "Study", choices = c("", names(dbUpdateEvent()$study)))
    updateSelectizeInput(session, selected = input$DelArchSearchBySpecimenType, "DelArchSearchBySpecimenType", "Specimen Type", choices = c("", dbUpdateEvent()$specimen_type))
    updateSelectizeInput(session, selected = input$DelArchSearchByLocation, "DelArchSearchByLocation", "Storage Location", choices = c("", dbUpdateEvent()$location))

    updateSelectizeInput(session, selected = input$DelArchSearchByState, "DelArchSearchByState", "State", choices = c(dbUpdateEvent()$state))
  
    # subject uid should be updated when db updates + when studies are selected
    .updateSubjectUID(session, input)
  })

  observeEvent(input$DelArchSearchByState, {
    choices <- NULL
    if (input$DelArchSearchByState %in% "Archived") {
      choices <- sampleDB:::.ViewArchiveStatuses(database = database)$name
    } else {
      choices <- "In Use"
    }
    selected <- choices[1]

    updateSelectizeInput(session, selected = selected, "DelArchSearchByStatus", "Status", choices = choices) 
  })

  observeEvent(input$DelArchSearchByStudy, { .updateSubjectUID(session, input) })
    
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

.updateSubjectUID <- function(session, input) {
  study_id <- match(input$DelArchSearchByStudy, names(dbUpdateEvent()$study))
  req(study_id)
  subject_indexes <- which(unname(dbUpdateEvent()$subject) == study_id)

  updateSelectizeInput(session,
    "DelArchSearchBySubjectUID",
    "Study Subject",
    selected = "",
    choices = names(dbUpdateEvent()$subject[subject_indexes]),
    server = TRUE)
}


.SearchReset <- function(input){
  observeEvent(input$ClearSearchBarcodes, ({shinyjs::reset("SearchByBarcode")}))
  observeEvent(input$ClearSearchUIDFile, ({shinyjs::reset("SearchBySubjectUIDFile")})) 
}
