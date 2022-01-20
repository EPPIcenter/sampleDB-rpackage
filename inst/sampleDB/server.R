library(dplyr)
library(sampleDB)
library(shinyFeedback)
library(shiny)
library(readr)
library(markdown)
library(lubridate)
library(shinyjs)
library(DT)

function(input, output, session) {

    #SET PATH TO SQLITE DATABASE
    # database <- Sys.getenv("SAMPLEDB_DATABASE") #use the aragorn env var set at boot
    database <- "/databases/example_19-Oct-21.sample_db.sqlite"

    #SERVER-SIDE DROPDOWN -- SAVES LOADING TIME
    updateSelectizeInput(session, 'SearchBySubjectUID', choices = c("", sampleDB::CheckTable(database = database, "study_subject")$uid %>% unique()), server = TRUE)

    ##################
    # Upload Samples #
    ##################
    
    # ~~~~~~ #
    # Checks #
    # ~~~~~~ #
    
    #CHECK PLATE_ID IS UNIQUE
    CheckUploadPlateDuplication <- reactive({
        toggle <- input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)
        shinyFeedback::feedbackWarning("UploadPlateID", toggle, "Plate IDs must be unique")
    })

    #CHECK IF BARCODES ARE ALREADY IN DATABASE - TEXT
    CheckUploadPlateUniqBarcodeConstraintA <- reactive({
      if(!is.null(input$UploadDataSet$datapath)){

        if("TubeCode" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
          upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$TubeCode
        }else{
          upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$"Tube ID"
        }

        validate(
          need(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)),
               "Failed: Barcode Unique Constraint")
        )
      }
    })

    #CHECK IF BARCODES ARE ALREADY IN DATABASE - FILEINPUT
    CheckUploadPlateUniqBarcodeConstraint <- reactive({
      if(!is.null(input$UploadDataSet$datapath)){

        if("TubeCode" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
          upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$TubeCode
        }else{
          upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$"Tube ID"
        }

        upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$TubeCode
        toggle <- upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)

        shinyFeedback::feedbackWarning("UploadDataSet", toggle, "Failed: Barcode Unique Constraint")
      }
    })

    #CHECK THAT COLNAMES ARE CORRECT
    CheckUploadColnames <- reactive({
      if(!is.null(input$UploadDataSet$datapath)){
        if("TubeCode" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
          upload_names <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% names()
        }else{
          upload_names <- read_csv(input$UploadDataSet$datapath, col_types = cols()) %>% names()
        }

        validate(
          need(upload_names == c("LocationRow", "LocationColumn", "TubeCodes", "study_subject_id", "specimen_type") ||
                 upload_names == c("LocationRow", "LocationColumn", "TubeCodes", "study_subject_id", "specimen_type", "collection_date") ||
                 upload_names == c("Position", "Tube ID", "study_subject_id", "specimen_type") ||
                 upload_names == c("Position", "Tube ID", "study_subject_id", "specimen_type", "collection_date"),
               "Failed: Required Column Names")
        )
      }
    })

    #CHECK THAT USR SPECIMEN TYPE IS VALID
    CheckUploadSpecimenTypes <- reactive({
      if(!is.null(input$UploadDataSet$datapath)){
        specimen_types <- read_csv(input$UploadDataSet$datapath, col_types = cols())$specimen_type

        validate(
          need(all(specimen_types %in% CheckTable(database = database, table = "specimen_type")$label),
               "Failed: Specimen Types are Not found")
        )
      }
    })

    #CHECK THAT DATE IS IN CORRECT FORMAT
    CheckUploadDateFormat <- reactive({
      if(!is.null(input$UploadDataSet$datapath)){
        if("collection_date" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
          collection_dates <- read_csv(input$UploadDataSet$datapath, col_types = cols())$collection_date

          validate(
            need(all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE), #ALL ITEMS IN COLLECTION DATE MUST HAVE THE PROPER FORMAT
                 "Failed: All Collection Dates are Not in YMD format")
          )
        }
      }
    })

    output$WarningUploadPlate <- renderText(CheckUploadPlateDuplication())
    output$WarningUploadBarcodeA <- renderText(CheckUploadPlateUniqBarcodeConstraintA())
    output$WarningUploadBarcode <- renderText(CheckUploadPlateUniqBarcodeConstraint())
    output$WarningUploadColnames <- renderText(CheckUploadColnames()) #OUTPUT DOES NOT EXIST
    output$WarningUploadSpecimenTypes <- renderText(CheckUploadSpecimenTypes()) #OUTPUT DOES NOT EXIST
    output$WarningUploadDateFormat <- renderText(CheckUploadDateFormat()) #OUTPUT DOES NOT EXIST

    # ~~~~~~ #
    # Upload #
    # ~~~~~~ #
    
    #UPLOAD PLATE
    
    observeEvent(
        input$UploadAction,
        ({
          
          #handle traxer v. visionmate
          if("TubeCode" %in% names(read_csv(input$UploadDataSet$datapath, col_types = cols()))){
            upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$TubeCode
          }else{
            upload_barcodes <- read_csv(input$UploadDataSet$datapath, col_types = cols())$"Tube ID"
          }

          #set requirements
          req(input$UploadDataSet$datapath,
              input$UploadPlateID,
              input$UploadLocation,
              input$UploadStudyShortCode,
              !(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)),
              !(input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)))
          
          # maxi <- 50
          # for (i in 1:maxi) {
          #   updateProgressBar(session = session, id = "pb4", value = (i/maxi)*100)
          #   Sys.sleep(0.2)
          # }
          
          #upload
          output$UploadReturnMessage2 <- renderText({sampleDB::UploadSamples(database = database,
                                            barcode_file = input$UploadDataSet$datapath,
                                            plate_id = input$UploadPlateID,
                                            location = input$UploadLocation,
                                            study_short_code = input$UploadStudyShortCode,
                                            session = session)})
          }))

    # ~~~~~~~~ #
    # Examples #
    # ~~~~~~~~ #
    output$ExampleUploadCSVNoDate <- renderPrint({
      tibble(LocationRow = rep("A", 10),
             LocationColumn = c(1:10),
             TubeCodes = CheckTable(database = database, "matrix_tube")$barcode %>% head(10),
             study_subject_id = CheckTable(database = database, "study_subject")$uid %>% head(10),
             specimen_type = "PLASMA") %>% as.data.frame()
    })

    output$ExampleUploadCSVDate <- renderPrint({
      tibble(LocationRow = rep("A", 10),
             LocationColumn = c(1:10),
             TubeCodes = CheckTable(database = database, "matrix_tube")$barcode %>% head(10),
             study_subject_id = CheckTable(database = database, "study_subject")$uid %>% head(10),
             specimen_type = "PLASMA",
             collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% as.data.frame()
    })
    
    observeEvent(input$ClearUploadForm,
                 ({
                   
                   reset("UploadDataSet")
                   reset("UploadStudyShortCode")
                   reset("UploadPlateID")
                   reset("UploadLocation")
                   output$UploadReturnMessage <- renderText({NULL})
                   
                 }))

    ##################
    # Search Samples #
    ##################

    #SEARCH SAMPLES

    # showNotification(ui = "ABCDE")
    
    #CHECK THAT UID FILE IS PROPERLY FORMED
    CheckSubjectBarcodeFileColnames <- reactive({
      
      if(!is.null(input$SearchByBarcode$datapath)){
        
        names.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% colnames()
        ncols.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% ncol()
        
        toggle <- ncols.barcode_file == 1 & names.barcode_file == "barcode"
        
        validate(
          need(toggle, #ALL ITEMS IN COLLECTION DATE MUST HAVE THE PROPER FORMAT
               "Failed: Barcode File is Malformed")
        )
      }
    })
    
    #CHECK IF UID FILE IS PROPERLY FORMED - FILEINPUT
    CheckSubjectBarcodeFileColnames2 <- reactive({
      if(!is.null(input$SearchByBarcode$datapath)){
        
        names.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% colnames()
        ncols.barcode_file <- read_csv(input$SearchByBarcode$datapath) %>% ncol()
        
        toggle <- !(ncols.barcode_file == 1 & names.barcode_file == "barcode")
        
        shinyFeedback::feedbackWarning("SearchByBarcode", toggle, "Failed: Barcode File is Malformed")
      }
    })
    
    #CHECK THAT UID FILE IS PROPERLY FORMED
    CheckSubjectUIDFileColnames <- reactive({
      
      if(!is.null(input$SearchBySubjectUIDFile$datapath)){

        names.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% colnames()
        ncols.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% ncol()
        
        toggle <- ncols.subject_uid_file == 1 & names.subject_uid_file == "subject_uid"

          validate(
            need(toggle, #ALL ITEMS IN COLLECTION DATE MUST HAVE THE PROPER FORMAT
                 "Failed: Subject UID File is Malformed")
          )
      }
    })
    
    #CHECK IF UID FILE IS PROPERLY FORMED - FILEINPUT
    CheckSubjectUIDFileColnames2 <- reactive({
      if(!is.null(input$SearchBySubjectUIDFile$datapath)){

        names.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% colnames()
        ncols.subject_uid_file <- read_csv(input$SearchBySubjectUIDFile$datapath) %>% ncol()

        toggle <- !(ncols.subject_uid_file == 1 & names.subject_uid_file == "subject_uid")

        shinyFeedback::feedbackWarning("SearchBySubjectUIDFile", toggle, "Failed: Subject UID File is Malformed")
      }
    })
    
    output$WarningSubjectUIDFileColnames2 <- renderText(CheckSubjectUIDFileColnames2())
    output$WarnSubjectUIDFileColnames <- renderText(CheckSubjectUIDFileColnames())
    output$WarnSubjectBarcodeFileColnames2 <- renderText(CheckSubjectBarcodeFileColnames2())
    output$WarnSubjectBarcodeFileColnames <- renderText(CheckSubjectBarcodeFileColnames())
        
    observeEvent(input$ClearSearchBarcodes,
                 ({
                   
                   reset("SearchByBarcode")

                 }))
    
    observeEvent(input$ClearSearchUIDFile,
                 ({
                   
                   reset("SearchBySubjectUIDFile")
                   
                 }))
    #update plate selection when study is specified
    observeEvent(input$SearchByStudy,({
      
      if(input$SearchByStudy != ""){
        
        study_ref_id <- filter(sampleDB::CheckTable(database = database, "study"), short_code %in% input$SearchByStudy)$id
        study_subject_ref_id <- filter(sampleDB::CheckTable(database = database, "study_subject"), study_id %in% study_ref_id)$id
        specimen_ref_id <- filter(sampleDB::CheckTable(database = database, "specimen"), study_subject_id %in% study_subject_ref_id)$id
        storage_container_id <- filter(sampleDB::CheckTable(database = database, "storage_container"), specimen_id %in% specimen_ref_id)$id
        matrix_tube_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), id %in% storage_container_id)$id
        
        plate_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), id %in% matrix_tube_ids)$plate_id %>% unique()
        plate_names <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), id %in% plate_ids)$uid
        
        updateSelectizeInput(session = session,
                             "SearchByPlateID",
                             choices = c("", plate_names))        
      }

    }))

    observe({

      if(is.null(input$SearchByBarcode$datapath)){
        barcode_search_file <- ""
      }else{
        barcode_search_file <- input$SearchByBarcode$datapath
      }

      if(is.null(input$SearchBySubjectUIDFile$datapath)){
        subjectuid_search_uids <- ""
      }else{
        subjectuid_search_uids <- read_csv(input$SearchBySubjectUIDFile$datapath)$subject_uid
        subjectuid_search_uids <- subjectuid_search_uids[subjectuid_search_uids != ""] # remove any blank entries that may be in vector
      }

      if(input$SubjectUIDSearchType == "one_at_a_time"){
        search_results <- sampleDB::SearchSamples(database = database,
                                                  barcode_search_file = barcode_search_file,
                                                  search_plate_uid = input$SearchByPlateID,
                                                  search_subject_uid = input$SearchBySubjectUID,
                                                  search_study = input$SearchByStudy,
                                                  search_location = input$SearchByLocation,
                                                  search_specimen_type = input$SearchBySpecimenType)
      }else{
        search_results <- sampleDB::SearchSamples(database = database,
                                                  barcode_search_file = barcode_search_file,
                                                  search_plate_uid = input$SearchByPlateID,
                                                  search_subject_uid = subjectuid_search_uids,
                                                  search_study = input$SearchByStudy,
                                                  search_location = input$SearchByLocation,
                                                  search_specimen_type = input$SearchBySpecimenType)

      }

      # DOWNLOAD SEARCH RESULTS
      output$SearchResultsTable <- DT::renderDataTable({
        search_results},
        options = list(
          searching = T,
          paging = T,
          language = list(zeroRecords = "No samples match filters given")))

      output$downloadData <- downloadHandler(
                  filename = function() {
                    paste('data-', Sys.Date(), '.csv', sep='')
                  },
                  content = function(con) {
                    write.csv(search_results, con)
                  }
                )

    })

    ##############
    # Move Tubes #
    ##############
    
    #~~~~~~~~#
    # Checks #
    #~~~~~~~~#

    # #CHECK PLATE_ID IS UNIQUE
    # CheckMovePlateDuplication <- reactive({
    #   toggle <- input$MovePlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)
    #   shinyFeedback::feedbackWarning("MovePlateID", toggle, "Plate IDs must be unique")
    # })
    # 
    # #CHECK IF BARCODES ARE ALREADY IN DATABASE
    # CheckMovePlateUniqBarcodeConstraintA <- reactive({
    #   if(!is.null(input$MoveDataSet$datapath)){
    # 
    #     if("TubeCode" %in% names(read_csv(input$MoveDataSet$datapath))){
    #       upload_barcodes <- read_csv(input$MoveDataSet$datapath)$TubeCode
    #     }else{
    #       upload_barcodes <- read_csv(input$MoveDataSet$datapath)$"Tube ID"
    #     }
    # 
    #     validate(
    #       need(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)),
    #            "Failed: Barcode Unique Constraint")
    #     )
    #   }
    # })
    # 
    # CheckMovePlateUniqBarcodeConstraint <- reactive({
    #   if(!is.null(input$MoveDataSet$datapath)){
    # 
    #     if("TubeCode" %in% names(read_csv(input$MoveDataSet$datapath))){
    #       upload_barcodes <- read_csv(input$MoveDataSet$datapath)$TubeCode
    #     }else{
    #       upload_barcodes <- read_csv(input$MoveDataSet$datapath)$"Tube ID"
    #     }
    # 
    #     upload_barcodes <- read_csv(input$MoveDataSet$datapath)$TubeCode
    #     toggle <- upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)
    #     shinyFeedback::feedbackWarning("MoveDataSet", toggle, "Failed: Barcode Unique Constraint")
    #   }
    # })
    # 
    # #CHECK THAT MOVE IS TO A DIFFERENT PLATE
    # CheckMoveToSamePlate <- reactive({
    #   if(!is.null(input$MoveDataSet$datapath)){
    #     if(input$MovePlateType == existing_plate){
    #       # get all plates asso w the tubes
    #       barcodes <- read_csv(input$MoveDataSet$datapath)$barcode
    #       uniq_plate_ids <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode %in% barcodes)$plate_id %>% unique()
    #       plate_names <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), id %in% uniq_plate_id)$uid
    #       # input$MoveExistingPlateID cannot be any of the plates ass w the tubes
    #       validate(
    #         need(!(input$MoveExistingPlateID %in% plate_names),
    #              "Failed: Samples cannot be moved to the same plate")
    #       )
    #     }
    #   }
    # })
    # 
    # #CHECK THAT BARCODES IN MOVE EXIST IN DATABASE
    # CheckMoveBarcodesExist <- reactive({
    #   if(!is.null(input$MoveDataSet$datapath)){
    #     validate(
    #       need(all(read_csv(input$MoveDataSet$datapath)$TubeCode %in% CheckTable(database = database, "matrix_tube")$barcode),
    #            "Failed: Barcodes for move do Not exist")
    #     )
    #   }
    # })
    # 
    # output$WarningMoveBarcodeA <- renderText(CheckMovePlateUniqBarcodeConstraintA())
    # output$WarningMoveBarcode <- renderText(CheckMovePlateUniqBarcodeConstraint())
    # output$WarningMovePlateDuplication <- renderText(CheckMovePlateDuplication())
    # output$WarningMoveToSamePlate <- renderText(CheckMoveToSamePlate())
    # output$WarningMoveBarcodesExist <- renderText(CheckMoveBarcodesExist())

    #~~~~~~#
    # Move #
    #~~~~~~#
    
    observeEvent(
      input$MoveAction,
      ({
        
        # #MAKE SURE BARCODES ARE ALREAD IN THE DATABASE
        # #pls use the same micronix scanner for all the csvs (if uploading multiple)
        # 
        # # - PUT ALL THE BARCODE CSVS INTO A LIST
        # barcode_upload <- list()
        # for(i in 1:length(input$MoveDataSet[,1])){
        #   barcode_upload[[i]] <- input$MoveDataSet[[i, 'datapath']]
        # }
        # 
        # # - MAKE A VECTOR CONTAINING ALL THE BARCODES IN MOVE
        # upload_barcodes <- c()
        # for(csv in barcode_upload){
        #   if("TubeCode" %in% names(read_csv(csv, col_types = cols()))){
        #     upload_barcodes <- c(upload_barcodes, read_csv(csv, col_types = cols())$TubeCode)
        #   }else{
        #     upload_barcodes <- c(upload_barcodes, read_csv(csv, col_types = cols())$"Tube ID")
        #   }
        # }
        # 
        # # - CHECK BARCODE IS IN DATABASE
        # req(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)),
        #     # !(input$MovePlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)
        #     )
        # 
        # # PERFORM OTHER CHECKS
        # req(input$MoveDataSet$datapath,
        #     input$MovePlateType,
        #     input$MoveLocation)

        # CARRY OUT MOVE
        output$MoveReturnMessage <- renderText({

          sampleDB::MoveTubes(database = database,
                              barcode_file = input$MoveDataSet,
                              plate_type = input$MovePlateType,
                              new_plate_uid = input$MovePlateID,
                              existing_plate_uid = input$MoveExistingPlateID,
                              location = input$MoveLocation,
                              session = session)})
        }))


    observeEvent(input$ClearMoveForm,
                 ({

                   reset("MoveDataSet")
                   reset("MovePlateType")
                   reset("MoveExistingPlateID")
                   reset("MovePlateID")
                   reset("MoveLocation")

                 }))

    observeEvent(
      input$MoveAction,
      ({
        updateSelectizeInput(session = session,
                             "SearchByPlateID",
                             choices = sampleDB::CheckTable(database = database, "matrix_plate")$uid,
                             label = NULL)}))

    output$ExampleMoveSamplesCSV <- renderPrint({
      tibble(LocationRow = rep("A", 10),
             LocationColumn = c(1:10),
             TubeCodes = CheckTable(database = database, "matrix_tube")$barcode %>% head(10)) %>% as.data.frame()})

    #REFERENCES#################################################################################

    ############
    # FREEZERS #
    ############

    #PROTECT AGAINST REDUNDANT FREEZER NAMES
    add_freezer_duplication_check <- reactive({
      toggle <- input$AddFreezer %in% sampleDB::CheckTable(database = database, "location")$description
      shinyFeedback::feedbackWarning("AddFreezer", toggle, "Freezer names must be unique")})
    output$add_freezer_warning <- renderText(add_freezer_duplication_check())

    #ADD FREEZER TO DATABASE
    observeEvent(
      input$.AddFreezerAction,
      ({
        sampleDB::AddToTable(database = database, "location",
                             list(created = lubridate::now("UTC"),
                                  last_updated = lubridate::now("UTC"),
                                  description = input$AddFreezer))

        output$TableFreezer <- DT::renderDataTable({
          sampleDB::CheckTable(database = database, "location") %>%
            dplyr::select(created, description) %>%
            rename(`Date Created` = created, Name = description) %>%
            relocate(Name, `Date Created`)})

        updateTextInput(session, "AddFreezer", value = "", placeholder = "New Name")
        updateSelectInput(session, ".RenameFreezer1", choices = sampleDB::CheckTable(database = database, "location")$description)
        updateSelectInput(session, "DeleteFreezer", choices =  sampleDB::CheckTable(database = database, "location")$description)

        output$FreezerReturnMessage <- renderText({paste("Freezer Added", emoji('tada'))})
      }))

    #PREVENT DUPLICATION OF FREEZER NAMES
    modify_freezer_duplication_check <- reactive({
      toggle <- input$RenameFreezer2 %in% c(sampleDB::CheckTable(database = database, "location")$description)
      shinyFeedback::feedbackWarning("RenameFreezer2", toggle, "Freezer names must be unique")})
    output$modify_freezer_warning <- renderText(modify_freezer_duplication_check())

    #MODIFY FREEZER NAMES
    observeEvent(
      input$.RenameFreezerAction,
      ({

        #NOTE: GET THE ENTRY'S "CREATED" DATE SO THAT IT CAN BE PASSED ON IN THE MODIFICATION
        sampleDB::ModifyTable(database = database, table_name = "location",
                              info_list = list(created = as.character(filter(sampleDB::CheckTable(database = database, "location"), description == input$.RenameFreezer1)$created),
                                               last_updated = lubridate::now("UTC"),
                                               description = input$RenameFreezer2),
                              id = as.character(filter(sampleDB::CheckTable(database = database, "location"), description == input$.RenameFreezer1)$id))

        output$TableFreezer <- DT::renderDataTable({
          sampleDB::CheckTable(database = database, "location") %>%
            dplyr::select(created, description) %>%
            rename(`Date Created` = created, Name = description) %>%
            relocate(Name, `Date Created`)})

        updateTextInput(session = session, "RenameFreezer2", value = "", placeholder = "New Name")
        updateSelectInput(session = session, inputId = ".RenameFreezer1", choices = sampleDB::CheckTable(database = database, "location")$description)
        updateSelectInput(session = session, inputId = "DeleteFreezer", choices = sampleDB::CheckTable(database = database, "location")$description)

        output$FreezerReturnMessage <- renderText({paste("Modified Freezer", input$RenameFreezer1, "to", input$.RenameFreezer2, emoji('tada'))})
        }))

    #PREVENT DELETION OF FREEZER THAT IS IN USE
    delete_freezer_delete_warning_check <- reactive({
      freezer_id <- CheckTable(database = database, "location") %>% filter(description == input$DeleteFreezer) %>% pull(id)
      toggle <- freezer_id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id
      shinyFeedback::feedbackWarning("DeleteFreezer", toggle, "Freezer is currently is use")})
    output$delete_freezer_delete_warning <- renderText(delete_freezer_delete_warning_check())

    #REMOVE FREEZER FROM DATABASE
    observeEvent(
      input$.DeleteFreezerAction,
      ({
        sampleDB::DeleteFromTable(database = database, table_name = "location",
                                  id = as.character(filter(sampleDB::CheckTable(database = database, "location"), description == input$DeleteFreezer)$id))

        output$TableFreezer <- DT::renderDataTable({
          sampleDB::CheckTable(database = database, "location") %>%
            dplyr::select(created, description) %>%
            rename(`Date Created` = created, Name = description) %>%
            relocate(Name, `Date Created`)})

        updateSelectInput(session, inputId = ".RenameFreezer1", choices = sampleDB::CheckTable(database = database, "location")$description)
        updateSelectInput(session, inputId = "DeleteFreezer", choices =  sampleDB::CheckTable(database = database, "location")$description)

        output$FreezerReturnMessage <- renderText({paste("Deleted", input$DeleteFreezer, emoji('tada'))})
      })
    )

    #IDLY PRESENT FREEZERS IN DATATABLE
    output$TableFreezer <- DT::renderDataTable({

      sampleDB::CheckTable(database = database, "location") %>%
        dplyr::select(created, description) %>%
        rename(`Date Created` = created, Name = description) %>%
        relocate(Name, `Date Created`)})

    ##################
    # SPECIMEN TYPES #
    ##################

    #PROTECT AGAINST SPECIMEN TYPE NAME DUPLICATION
    add_specimen_type_duplication_check <- reactive({
        toggle <- input$AddSpecimenType %in% c(sampleDB::CheckTable(database = database, "specimen_type") %>% dplyr::select(label) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("AddSpecimenType", toggle, "Specimen Type names must be unique")})
    output$add_specimen_type_warning <- renderText(add_specimen_type_duplication_check())

    #ADD A SPECIMEN TYPE TO THE DATABASE
    observeEvent(
        input$.AddSpecimenTypeAction,
        ({
            sampleDB::AddToTable(database = database, "specimen_type",
                                        list(created = lubridate::now("UTC"),
                                             last_updated = lubridate::now("UTC"),
                                             label = input$AddSpecimenType))

            output$TableSpecimenType <- DT::renderDataTable({
                sampleDB::CheckTable(database = database, "specimen_type") %>%
                    rename(`Date Created` = created) %>%
                    relocate(`Date Created`)})

            updateTextInput(session = session, "AddSpecimenType", value = "", placeholder = "New Name")
            updateSelectInput(session = session, inputId = ".RenameSpecimenType1", choices = sampleDB::CheckTable(database = database, "specimen_type")$label)
            updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = sampleDB::CheckTable(database = database, "specimen_type")$label)

            output$SpecimenReturnMessage <- renderText({paste("Added Specimen Type", emoji('tada'))})
            }))

    #PREVENT REPLICATE SPECIMEN TYPE NAMES
    modify_specimen_type_duplication_check <- reactive({
      toggle <- input$RenameSpecimenType2 %in% c(sampleDB::CheckTable(database = database, "specimen_type") %>% dplyr::select(label) %>% dplyr::pull())
      shinyFeedback::feedbackWarning("RenameSpecimenType2", toggle, "Specimen Type names must be unique")})
    output$modify_specimen_type_warning <- renderText(modify_specimen_type_duplication_check())

    #MODIFY A SPECIMEN TPYE
    observeEvent(
      input$.RenameSpecimenTypeAction,
      ({
        #NOTE: GET THE ENTRY'S "CREATED" DATE SO THAT IT CAN BE PASSED ON IN THE MODIFICATION
        sampleDB::ModifyTable(database = database, table_name = "specimen_type",
                              info_list = list(created = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$.RenameSpecimenType1)$created),
                                               last_updated = lubridate::now("UTC"),
                                               label = input$RenameSpecimenType2),
                              id = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$.RenameSpecimenType1)$id))

        output$TableSpecimenType <- DT::renderDataTable({
          sampleDB::CheckTable(database = database, "specimen_type") %>%
            rename(`Date Created` = created) %>%
            relocate(`Date Created`)})

        updateTextInput(session = session, "RenameSpecimenType2", value = "", placeholder = "New Name")
        updateSelectInput(session = session, inputId = ".RenameSpecimenType1", choices = sampleDB::CheckTable(database = database, "specimen_type")$label)
        updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = sampleDB::CheckTable(database = database, "specimen_type")$label)

        output$SpecimenReturnMessage <- renderText({paste("Modified Specimen Type", input$.RenameSpecimenType1, "to", input$.RenameSpecimenType2, emoji('tada'))})
        }))

    #PROTECT AGAINST DELETION OF SPECIMEN TYPE IN USE
    delete_specimen_delete_warning_check <- reactive({
      specimen_type_id <- CheckTable(database = database, "specimen_type") %>% filter(label == input$DeleteSpecimenType) %>% pull(id)
      toggle <- specimen_type_id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id
      shinyFeedback::feedbackWarning("DeleteSpecimenType", toggle, "Specimen Type is currently is use")})
    output$delete_specimen_delete_warning <- renderText(delete_specimen_delete_warning_check())

    #DELETE A SPECIMEN TYPE
    observeEvent(
        input$.DeleteSpecimenTypeAction,
        ({
            sampleDB::DeleteFromTable(database = database, table_name = "specimen_type",
                                             id = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$DeleteSpecimenType)$id))

            output$TableSpecimenType <- DT::renderDataTable({
                sampleDB::CheckTable(database = database, "specimen_type") %>%
                    rename(`Date Created` = created) %>%
                    relocate(`Date Created`)})

            updateSelectInput(session = session, inputId = ".RenameSpecimenType1", choices = sampleDB::CheckTable(database = database, "specimen_type")$label)
            updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = sampleDB::CheckTable(database = database, "specimen_type")$label)

            output$SpecimenReturnMessage <- renderText({paste("Deleted Specimen Type", input$DeleteSpecimenType, emoji('tada'))})
            }))

    #IDLY PRESENT SPECIMEN TYPES
    output$TableSpecimenType <- DT::renderDataTable({

      sampleDB::CheckTable(database = database, "specimen_type") %>%
        rename(`Date Created` = created) %>%
        relocate(`Date Created`)})


    ###########
    # STUDIES #
    ###########

    #PROTECT AGAINST STUDY NAME DUPLICATION
    add_study_title_duplication_check <- reactive({
      toggle <- input$AddStudyTitle %in% c(sampleDB::CheckTable(database = database, "study")$title)
      shinyFeedback::feedbackWarning("AddStudyTitle", toggle, "Study titles must be unique")})
    output$add_study_title_warning <- renderText(add_study_title_duplication_check())

    #PROTECT AGAINST STUDY SHORT CODE DUPLICATION
    add_study_short_code_duplication_check <- reactive({
      toggle <- input$AddStudyShortCode %in% c(sampleDB::CheckTable(database = database, "study")$short_code)
      shinyFeedback::feedbackWarning("AddStudyShortCode", toggle, "Study short codes must be unique")})
    output$add_study_short_code_warning <- renderText(add_study_short_code_duplication_check())

    #ADD A STUDY TO THE DATABASE
    observeEvent(
        input$.AddStudyAction,
        ({
            sampleDB::AddToTable(database = database, "study",
                                        list(created = lubridate::now("UTC"),
                                             last_updated = lubridate::now("UTC"),
                                             title = input$AddStudyTitle,
                                             description = input$AddStudyDescription,
                                             short_code = input$AddStudyShortCode,
                                             is_longitudinal = input$AddStudyIsLongitudinal,
                                             lead_person = input$AddStudyLeadPerson,
                                             hidden = input$AddStudyIsHidden))

            output$TableStudy <- DT::renderDataTable({
                sampleDB::CheckTable(database = database, "study") %>%
                    dplyr::select(-c(id, created, last_updated, hidden))})

            updateTextInput(session = session, "AddStudyTitle", value = "", placeholder = "New Title")
            updateTextInput(session = session, "AddStudyDescription", value = "", placeholder = "New Description")
            updateTextInput(session = session, "AddStudyLeadPerson", value = "", placeholder = "New Lead Person")
            updateTextInput(session = session, "AddStudyShortCode", value = "", placeholder = "New Short Code")
            updateCheckboxInput(session = session, "AddStudyIsLongitudinal", value = FALSE)
            updateCheckboxInput(session = session, "AddStudyIsHidden", value = FALSE)

            output$StudyReturnMessage <- renderText({paste("Added Study to the Database", emoji('tada'))})
        })
    )

    #PROTECT AGAINST STUDY NAME DUPLICATION
    rename_study_title_duplication_check <- reactive({
      toggle <- input$RenameStudyTitle %in% c(sampleDB::CheckTable(database = database, "study")$title)
        shinyFeedback::feedbackWarning("RenameStudyTitle", toggle, "Study titles must be unique")})
    output$rename_study_title_warning <- renderText(rename_study_title_duplication_check())

    #PROTECT AGAINST STUDY SHORT CODE DUPLICATION
    rename_study_short_code_duplication_check <- reactive({
      toggle <- input$RenameStudyShortCode %in% c(sampleDB::CheckTable(database = database, "study")$short_code)
        shinyFeedback::feedbackWarning("RenameStudyShortCode", toggle, "Study short codes must be unique")})
    output$rename_study_short_code_warning <- renderText(rename_study_short_code_duplication_check())

    #RENAME A STUDY
    observe({
        observeEvent(
            input$.RenameStudyAction,
            ({
              #NOTE: NEED TO HANDLE HIDDEN AND LONGITUDINAL MODIFICATIONS
              #CREATE ENTRY THAT WILL REPLACE THE CURRENT ENTRY
              entry <- keep(as.list(sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]), function(x) x != "id")
              new_entry <- list(title = input$RenameStudyTitle, description = input$RenameStudyDescription, short_code = input$RenameStudyShortCode, lead_person = input$RenameStudyLeadPerson)  %>% discard(function(x) x == "")
              for (i in names(new_entry)){
                entry[[i]] <- new_entry[[i]]
              }

              sampleDB::ModifyTable(database = database, table_name = "study",
                                           info_list = entry,
                                           id = as.character(sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]$"id"))

              updateTextInput(session = session, "RenameStudyTitle", value = "", placeholder = "New Title")
              updateTextInput(session = session, "RenameStudyDescription", value = "", placeholder = "New Description")
              updateTextInput(session = session, "RenameStudyLeadPerson", value = "", placeholder = "New Lead Person")
              updateTextInput(session = session, "RenameStudyShortCode", value = "", placeholder = "New Short Code")
              updateCheckboxInput(session = session, "RenameStudyIsLongitudinal", value = FALSE)
              updateCheckboxInput(session = session, "RenameStudyIsHidden", value = FALSE)

              output$TableStudy <- DT::renderDataTable({
                  sampleDB::CheckTable(database = database, "study") %>%
                      dplyr::select(-c(id, created, last_updated, hidden))})

              output$StudyReturnMessage <- renderText({paste("Modified Study", emoji('tada'))})
              }))})

        #REMOVE A STUDY FROM THE DATABASE
        observe({
            observeEvent(
                input$DeleteStudyAction,
                ({
                    id <- sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]$"id"
                    output$DeleteStudy <- renderPrint({
                      if(id %in% sampleDB::CheckTable(database = database, "study_subject")$study_id){
                        print("*ERROR: Study is currently in use*")}})

                    sampleDB::DeleteFromTable(database = database, table_name = "study", id = as.character(id))

                    output$TableStudy <- DT::renderDataTable({
                        sampleDB::CheckTable(database = database, "study") %>%
                            dplyr::select(-c(id, created, last_updated, hidden))})

                    output$StudyReturnMessage <- renderText({paste("Deleted Study", emoji('tada'))})
                    })
              )})

        #IDLY SHOW STUDIES
        output$TableStudy <- DT::renderDataTable({
          sampleDB::CheckTable(database = database, "study") %>%
            dplyr::select(-c(id, created, last_updated, hidden))}, selection = 'single')

}
