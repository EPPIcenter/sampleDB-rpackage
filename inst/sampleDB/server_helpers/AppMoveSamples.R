
# App Function for Moving Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

MoveSamples <- function(session, input, database, output) {
  
  # 1. create variable for storing formatted_move_file_list once it is created
  rv <- reactiveValues(users_move_file = NULL)

  observe({
    req(input$MoveDataSet)
    rv$users_move_file <- input[["MoveDataSet"]]
  })  

  # 2. get path to user provided file(s), if path exists perform checks and reformat items in list
  
  observeEvent(input$MoveAction, {

    b_use_wait_dialog <- FALSE
    output$MoveReturnMessage2  <- renderText({
      tryCatch({
          users_move_file <- isolate({ rv$users_move_file })
          move_data_list <- list()
          for(i in 1:length(users_move_file[,1])) {
            container_name <- users_move_file[[i, 'name']] %>% gsub("\\.csv","",.)
            formatted_move_file <- ProcessCSV(users_move_file[[i, "datapath"]], user_action = "move", sample_storage_type = input$MoveSampleType, file_type = input$MoveFileType, container_name = container_name)
            move_data_list[[container_name]] <- formatted_move_file
          }
          
          # always be true for now
          b_use_wait_dialog <- TRUE
          rv$users_move_file <- NULL
          shinyjs::reset("MoveAction")

          if (b_use_wait_dialog) {
            show_modal_spinner(
              spin = "double-bounce",
              color = "#00bfff",
              text = paste("Moving samples from", length(move_data_list), "file, please be patient...")
            )
          }

          sampleDB::MoveSamples(sample_type = input$MoveSampleType,
                                move_data = move_data_list)
          
        },
        warning = function(w) {
          message(w)
          w$message
        },
        error = function(e) {
          message(e)
          e$message
        },
        finally = {
          if (b_use_wait_dialog) {
            remove_modal_spinner()
          }
        }
      )
    }) # rendertext
  }) # observe
  
  # allow user to reset ui
  MoveReset(input, output)
  
  # present move examples
  MoveUploadExamples(database = database, output = output, sample_type = "micronix")
  
  # add blank plate to database
  CreateEmptyManifest(input = input, output = output, database = database)

  # auto-filter freezer addresses in dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = "CreateEmptyManifestLocation", 
                             levelI_ui = "CreateEmptyManifestLevelI", 
                             levelII_ui = "CreateEmptyManifestLevelII")
}

MoveUploadExamples <- function(database, output, sample_type){
  
  if(sample_type == "micronix"){
    ui.output <- list(
      InDatabasePlateOne = "InDatabasePlateOne",
      InDatabasePlateTwo = "InDatabasePlateTwo",
      PlateOneMove = "PlateOneMove",
      PlateTwoMove = "PlateTwoMove")
  }
  
  output[[ui.output$PlateOneMove]] <- renderTable({.ExamplePlateOneMove()}, striped = T, bordered = T)
  output[[ui.output$PlateTwoMove]] <- renderTable({.ExamplePlateTwoMove()}, striped = T, bordered = T)
  output[[ui.output$InDatabasePlateOne]] <- renderTable({.ExampleInDatabasePlateOne()}, striped = T, bordered = T)
  output[[ui.output$InDatabasePlateTwo]] <- renderTable({.ExampleInDatabasePlateTwo()}, striped = T, bordered = T)
}

CreateEmptyManifest <- function(input, output, database){

  vals <- reactiveValues(data = NULL)
  
  # Show modal when button is clicked.
  observeEvent(input$CreateEmptyManifest, {
    showModal(dataModal(database = database))
  })
  
  observeEvent(input$CreateEmptyManifestFormOk, {
    # create empty micronix plate using user input
    # use a "req" to require "CreateEmptyManifestID", "CreateEmptyManifestLocation", etc
    # throw error if user uses name that is already in the database
    req(
      input$CreateEmptyManifestID,
      input$CreateEmptyManifestLocation,
      input$CreateEmptyManifestLevelI,
      input$CreateEmptyManifestLevelII
    )

    manifest_table <- switch(input$MoveSampleType,
      "cryovial" = "cryovial_box",
      "micronix" = "micronix_plate"
    )
      
    if (CheckTable(database = database, table = manifest_table) %>%
      filter(input$CreateEmptyManifestID == name | barcode == input$CreateEmptyMicronixPlateBarcode) %>%
      nrow(.) > 0) {
      showNotification("Value would have created a duplicate!", id = "MoveNotification", type = "error", action = NULL, duration = 3, closeButton = TRUE)
    } else if (nchar(input$CreateEmptyMicronixPlateBarcode) > 0 && nchar(input$CreateEmptyMicronixPlateBarcode) != 10) {
      showNotification("Barcode must be 10 digits!", id = "MoveNotification", type = "error", action = NULL, duration = 3, closeButton = TRUE)
    } else {
      sampleDB:::UploadSamples(container_name = input[["CreateEmptyManifestID"]],
                                      sample_type = input$MoveSampleType,
                                      upload_data = NULL,
                                      container_barcode = input[["CreateEmptyMicronixPlateBarcode"]],
                                      freezer_address = list(location_name = input[["CreateEmptyManifestLocation"]],
                                                             level_I = input[["CreateEmptyManifestLevelI"]],
                                                             level_II = input[["CreateEmptyManifestLevelII"]]))
    
      vals$data <- ""
      removeModal()
    }

  })
  
  output$CreateEmptyMicronixPlateMessage <- renderPrint({
    if(!is.null(vals$data)){
      "Created Empty Matrix Plate"
    }
  })
}

dataModal <- function(failed = FALSE, database) {
  modalDialog(
    HTML("<h2>Create a Blank Micronix Plate</h2>"),
    HTML("<h4>Fill out the section below</h4>"),
    br(),
    fluidRow(column(width = 6, HTML("<p>Human Readable Name</p>"), textInput("CreateEmptyManifestID", label = NULL, placeholder = "PRISM-2022-001")),
             column(width = 6,  HTML("<p>Barcode (Optional)</p>"), textInput("CreateEmptyMicronixPlateBarcode", label = NULL))),
    HTML("<p>Freezer Name</p>"), selectInput("CreateEmptyManifestLocation", label = NULL, width = '47%', choices = c("", sampleDB::CheckTable(database = database, "location")$location_name) %>% sort()),
    HTML("<p>Shelf Name</p>"), selectInput("CreateEmptyManifestLevelI", label = NULL, width = '47%', choices = NULL),
    HTML("<p>Basket Name</p>"), selectInput("CreateEmptyManifestLevelII", label = NULL, width = '47%', choices = NULL),
    if(failed){
      div(tags$b("ERROR", style = "color: red;")) 
    },
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("CreateEmptyManifestFormOk", "OK")
    )
    
  )
}