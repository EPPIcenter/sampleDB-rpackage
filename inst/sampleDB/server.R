library(dplyr)
library(sampleDB)
library(shinyFeedback)
library(shiny)
library(readr)
library(markdown)
library(lubridate)
library(emojifont)
library(shinyjs)
library(DT)
library(purrr)
for(helper in list.files(path = "helpers", full.names = T)){source(helper, local = TRUE)}

function(input, output, session) {

    #SET PATH TO SQLITE DATABASE - WOULD PREFER DATABASE TO BE AT Sys.getenv("SAMPLEDB_DATABASE")
    database <- "/databases/sampledb_database.sqlite"

    #SERVER-SIDE DROPDOWN -- SAVES LOADING TIME
    updateSelectizeInput(session, 'SearchBySubjectUID', choices = c("", sampleDB::CheckTable(database = database, "study_subject")$uid %>% unique()), server = TRUE)

    ##################
    # Upload Samples #
    ##################
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Perform upload checks... Prints good user messages #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
    # CHECK PLATE_ID IS UNIQUE
    CheckUploadPlateDuplication <- reactive({helper.CheckUploadPlateDuplication(input, database)})
    output$WarningUploadPlate <- renderText(CheckUploadPlateDuplication())

    # CHECK THAT COLNAMES ARE CORRECT
    CheckUploadColnames <- reactive({helper.CheckUploadColnames(input, database)})
    output$WarningUploadColnames <- renderText(CheckUploadColnames())
    
    # CHECK THAT DATE IS IN CORRECT FORMAT
    CheckUploadDateFormat <- reactive({helper.CheckUploadDateFormat(input, database)})
    output$WarningUploadDateFormat <- renderText(CheckUploadDateFormat())

    # CHECK THAT USR SPECIMEN TYPES ARE VALID
    CheckUploadSpecimenTypes <- reactive({helper.CheckUploadSpecimenTypes(input, database)})
    output$WarningUploadSpecimenTypes <- renderText(CheckUploadSpecimenTypes())
    
    # CHECK THAT USR STUDY SHORT CODES ARE VALID
    CheckUploadStudyShortCode <- reactive({helper.CheckUploadStudyShortCodes(input, database)})
    output$WarningUploadStudyShortCodes <- renderText(CheckUploadStudyShortCode())

    # CHECK THAT BARCODES ARE NOT IN DATABASE
    CheckUploadPlateUniqBarcodeConstraint <- reactive({helper.CheckUploadPlateUniqBarcodeConstraint(input, database)})
    output$WarningUploadBarcodeA <- renderText(CheckUploadPlateUniqBarcodeConstraint())

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Add new plate to the database #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
    # LINK ACTION BUTTON TO TRIGGER CASCADE
    observeEvent(
      input$UploadAction,
      ({
        # TRIGGER UI CHANGE FOR REACTIVITY - RECYCLE RENAMESTUDYLEADPERSON
        updateTextInput(session = session, "RenameStudyLeadPerson", value = "@RBRLdB?GtnJ4kce")
        
        # PAUSE FOR EFFECT AND PRINT WORKING
        Sys.sleep(.75)
        output$UploadReturnMessage1 <- renderText({"Working..."})
      }))
    
    # UPLOAD SAMPLES
    observe({
      
      # WHEN REACTIVE UI IS CHANGED TO INDICATE AN UPLOAD
      if(input$RenameStudyLeadPerson == "@RBRLdB?GtnJ4kce"){
        
        #CHECK REQUIREMENTS
        UploadRequirements(input, database)
        sampleDB::UploadSamples(database = database,
                                barcode_file = input$UploadDataSet$datapath,
                                plate_id = input$UploadPlateID,
                                location = input$UploadLocation,
                                # study_short_code = input$UploadStudyShortCode,
                                session = session,
                                output = output)
        
        # PRINT UPLOAD MSG
        output$UploadReturnMessage2 <- renderText({"Upload Complete"})
        
        # RESET UI VALUE
        updateTextInput(session = session, "RenameStudyLeadPerson", value = "")
      }
    })
    
    # CLEAR FORM
    observeEvent(
      input$ClearUploadForm,
                 ({
                   reset("UploadDataSet")
                   reset("UploadStudyShortCode")
                   reset("UploadPlateID")
                   reset("UploadLocation")
                   output$UploadReturnMessage1 <- renderText({""})
                   output$UploadReturnMessage2 <- renderText({""})
                   }))

    # ~~~~~~~~~~~~~~~ #
    # Upload Examples #
    # ~~~~~~~~~~~~~~~ #
    
    output$ExampleUploadCSVNoDate <- renderPrint({helper.ExampleUploadCSVNoDate(database)})
    output$ExampleUploadCSVDate <- renderPrint({helper.ExampleUploadCSVDate(database)})

    ##################
    # Search Samples #
    ##################

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Checks for searching the database... Check that files uploaded for searching are not malformed #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
    #CHECK THAT UID FILE IS PROPERLY FORMED
    CheckSubjectBarcodeFileColnames <- reactive({helper.CheckSubjectBarcodeFileColnames(input, database)})
    output$WarnSubjectBarcodeFileColnames <- renderText(CheckSubjectBarcodeFileColnames())
    
    #CHECK IF UID FILE IS PROPERLY FORMED - FILEINPUT
    CheckSubjectUIDFileColnames2 <- reactive({CheckSubjectUIDFileColnames2(input, database)})
    output$WarningSubjectUIDFileColnames2 <- renderText(CheckSubjectUIDFileColnames2())
    
    #CHECK THAT UID FILE IS PROPERLY FORMED
    CheckSubjectUIDFileColnames <- reactive({helper.CheckSubjectUIDFileColnames(input, database)})
    output$WarnSubjectUIDFileColnames <- renderText(CheckSubjectUIDFileColnames())
    
    #CHECK IF UID FILE IS PROPERLY FORMED - FILEINPUT
    CheckSubjectBarcodeFileColnames2 <- reactive({helper.CheckSubjectUIDFileColnames2(input, database)})
    output$WarnSubjectBarcodeFileColnames2 <- renderText(CheckSubjectBarcodeFileColnames2())
    
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # # If as study is selected from the dropdown, subset plate names filter to only display plate names asso. w. the chosen study #
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # 
    # # SUBSET PLATE NAMES IF STUDY IS SELECTED
    # observeEvent(
    #   input$SearchByStudy, ({
    # 
    #     if(input$SearchByStudy != ""){
    #       updateSelectizeInput(session = session,
    #                            "SearchByPlateID",
    #                            choices = c("", helper.SubsetPlateNames(input, database)))        
    #     }
    # }))
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Actively use the UI filters to render a table with search results #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

    observe({

      # GET BARCODES IF BARCODE FILE IS PROVIDED
      if(is.null(input$SearchByBarcode$datapath)){
        barcode_search_file <- ""
      }else{
        barcode_search_file <- input$SearchByBarcode$datapath
      }

      # GET STUDY SUBJECT ID IF STUDY SUBJECT ID FILE IS PROVIDED
      if(is.null(input$SearchBySubjectUIDFile$datapath)){
        subjectuid_search_uids <- ""
      }else{
        subjectuid_search_uids <- read_csv(input$SearchBySubjectUIDFile$datapath)$subject_uid
        subjectuid_search_uids <- subjectuid_search_uids[subjectuid_search_uids != ""] # remove any blank entries that may be in vector
      }
      
      # RETRIEVE SEARCH RESULTS

      # - SEARCH USING A SINGLE STUDY SUBJECT ID
      if(input$SubjectUIDSearchType == "one_at_a_time"){
        search_results <- sampleDB::SearchSamples(database = database,
                                                  barcode_search_file = barcode_search_file,
                                                  search_plate_uid = input$SearchByPlateID,
                                                  search_subject_uid = input$SearchBySubjectUID,
                                                  search_study = input$SearchByStudy,
                                                  search_location = input$SearchByLocation,
                                                  search_specimen_type = input$SearchBySpecimenType)
      }else{
        
        # - SEARCH USING MULTIPLE STUDY SUBJECT IDs
        search_results <- sampleDB::SearchSamples(database = database,
                                                  barcode_search_file = barcode_search_file,
                                                  search_plate_uid = input$SearchByPlateID,
                                                  search_subject_uid = subjectuid_search_uids,
                                                  search_study = input$SearchByStudy,
                                                  search_location = input$SearchByLocation,
                                                  search_specimen_type = input$SearchBySpecimenType)

      }

      # PRINT SEARCH RESULTS
      output$SearchResultsTable <- DT::renderDataTable({
          
        search_results
        
      },
        options = list(
          searching = T,
          paging = T,
          language = list(zeroRecords = "No samples match filters given")))

      # DOWNLOAD SEARCH RESULTS
      output$downloadData <- downloadHandler(
                  filename = function() {
                    paste('data-', Sys.Date(), '.csv', sep='')
                  },
                  content = function(con) {
                    write.csv(search_results, con)
                  }
                )
    })
    
    observeEvent(
      input$ClearSearchBarcodes,
      ({
        reset("SearchByBarcode")}))
    
    observeEvent(
      input$ClearSearchUIDFile,
      ({
        reset("SearchBySubjectUIDFile")}))

    ##############
    # Move Tubes #
    ##############
    
    #~~~~~~~~#
    # Checks #
    #~~~~~~~~#

    # CHECK THAT COLNAMES ARE FORMED CORRECTLY
    CheckMoveColnames <- reactive({helper.CheckMoveColnames(input, database)})
    output$WarningMoveColnames <- renderText(CheckMoveColnames())
    
    #~~~~~~#
    # Move #
    #~~~~~~#
    
    observeEvent(
      input$MoveAction,
      ({
        
        # TRIGGER UI CHANGE FOR REACTIVITY - RECYCLE RENAMESTUDYLEADPERSON
        updateTextInput(session = session, "RenameStudyLeadPerson", value = "a6sFH$DKdsbgGLY9")
        
        # PAUSE FOR EFFECT AND PRINT WORKING
        Sys.sleep(.75)
        output$MoveReturnMessage1 <- renderText({"Working..."})
        
      }))
    
      # UPLOAD SAMPLES
      observe({
          
        # WHEN REACTIVE UI IS CHANGED TO INDICATE AN UPLOAD
        if(input$RenameStudyLeadPerson == "a6sFH$DKdsbgGLY9"){
            
          #CHECK REQUIREMENTS
          # MoveRequirements(input, database)
            
          # MOVE SAMPLES
          message <- sampleDB::MoveTubes(database = database,
                              barcode_file = input$MoveDataSet,
                              plate_type = input$MovePlateType,
                              new_plate_uid = input$MovePlateID,
                              existing_plate_uid = input$MoveExistingPlateID,
                              location = input$MoveLocation,
                              session = session)
          
          # PRINT UPLOAD MSG
          output$MoveReturnMessage2 <- renderText({message})
          
          # RESET UI VALUE
          updateTextInput(session = session, "RenameStudyLeadPerson", value = "")
        }
      })
        
      # CLEAR FORM
      observeEvent(
        input$ClearMoveForm,
        ({
          reset("MoveDataSet")
          output$MoveReturnMessage1 <- renderText({""})
          output$MoveReturnMessage2 <- renderText({""})}))
      
      # MOVE EXAMPLES
      output$ExampleMoveSamplesCSV <- renderPrint({helper.ExampleMoveCSVDate(database)})

    ######################
    # Delete Empty Plate #
    ######################
    
    WarningDeleteEmptyPlate <- reactive({helper.CheckDeleteEmptyPlate(input, database)})
    output$WarningDeletePlateMessage <- renderText({WarningDeleteEmptyPlate()})
    
    observeEvent(
      input$DeletePlateAction,
      ({
        
        # SET REQUIREMENT FOR DELETEING PLATE
        DeleteEmptyPlateRequirement(input, database)
        
        # DELETE PLATE
        plate_name <- input$DeletePlateName
        output$DeletePlateMessage <- renderText({sampleDB::DeleteEmptyPlates(database = database, plate_name = plate_name)})
        
        #UPDATE DELETE PLATE DROPDOWN
        updateSelectizeInput(session = session,
                             "DeletePlateName",
                             choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid))
        
      }))
      
    #REFERENCES#################################################################################

    ############
    # FREEZERS #
    ############
    
    # ~~~~~ #
    # Check #
    # ~~~~~ #
    
    #PROTECT AGAINST REDUNDANT FREEZER NAMES
    add_freezer_duplication_check <- reactive({
      toggle <- input$AddFreezer %in% sampleDB::CheckTable(database = database, "location")$description
      shinyFeedback::feedbackWarning("AddFreezer", toggle, "Freezer names must be unique")})
    
    #PREVENT DUPLICATION OF FREEZER NAMES
    modify_freezer_duplication_check <- reactive({
      toggle <- input$RenameFreezer2 %in% c(sampleDB::CheckTable(database = database, "location")$description)
      shinyFeedback::feedbackWarning("RenameFreezer2", toggle, "Freezer names must be unique")})
    
    # #CHECK THAT UID FILE IS PROPERLY FORMED
    # modify_freezer_duplication_check2 <- reactive({
    # 
    #     validate(
    #       need(!(input$RenameFreezer2 %in% c(sampleDB::CheckTable(database = database, "location")$description)),
    #            "Failed: Barcode File is Malformed")
    #     )
    # })
    
    #PREVENT DELETION OF FREEZER THAT IS IN USE
    delete_freezer_delete_warning_check <- reactive({
      freezer_id <- CheckTable(database = database, "location") %>% filter(description == input$DeleteFreezer) %>% pull(id)
      toggle <- freezer_id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id
      shinyFeedback::feedbackWarning("DeleteFreezer", toggle, "Freezer is currently is use")})
    
    output$add_freezer_warning <- renderText(add_freezer_duplication_check())
    output$modify_freezer_warning <- renderText(modify_freezer_duplication_check())
    # output$modify_freezer_warning2 <- renderText(modify_freezer_duplication_check2())
    output$delete_freezer_delete_warning <- renderText(delete_freezer_delete_warning_check())
    
    # ~~~ #
    # Add #
    # ~~~ #

    #ADD FREEZER TO DATABASE
    observeEvent(
      input$AddFreezerAction,
      ({
        
        #SAVE FREEZER NAMES INVOLVED
        new.freezer <- input$AddFreezer
        
        #ADD FREEZER IF IT HAS A UNIQUE NAME
        if(!(new.freezer %in% c(sampleDB::CheckTable(database = database, "location")$description))){
          sampleDB::AddToTable(database = database, "location",
                               list(created = as.character(lubridate::now("UTC")),
                                    last_updated = as.character(lubridate::now("UTC")),
                                    description = input$AddFreezer))          
          #PRINT MESSAGE
          output$FreezerReturnMessage <- renderText({paste("Added Freezer", new.freezer, emoji('tada'))})
        }else{
          #PRINT MESSAGE
          output$FreezerReturnMessage <- renderText({paste("Error")})
        }

        #MODIFY TABLE
        ShowFreezers(output, database)

        #UPDATE DROPDOWNS
        updateTextInput(session, "AddFreezer", value = "", placeholder = "New Name")
        updateSelectInput(session, "RenameFreezer1", choices = c("", sampleDB::CheckTable(database = database, "location")$description))
        updateSelectInput(session, "DeleteFreezer", choices =  c("", sampleDB::CheckTable(database = database, "location")$description))

      }))

    # ~~~~~~ #
    # Modify #
    # ~~~~~~ #
    
    #MODIFY FREEZER NAMES
    observeEvent(
      input$RenameFreezerAction,
      ({

        #SAVE FREEZER NAMES INVOLVED
        old.name <- input$RenameFreezer1
        new.name <- input$RenameFreezer2
        
        #MODIFY TABLE IF NEW FREEZER NAME IS UNIQUE
        if(!(new.name %in% c(sampleDB::CheckTable(database = database, "location")$description))){
          sampleDB::ModifyTable(database = database, table_name = "location",
                                info_list = list(created = as.character(filter(sampleDB::CheckTable(database = database, "location"), 
                                                                               description == input$RenameFreezer1)$created),
                                                 last_updated = as.character(lubridate::now("UTC")),
                                                 description = input$RenameFreezer2),
                                id = as.character(filter(sampleDB::CheckTable(database = database, "location"), description == input$RenameFreezer1)$id))
          #PRINT EXIT MESSAGE
          output$FreezerReturnMessage <- renderText({paste("Renamed Freezer", old.name, "to", new.name, emoji('tada'))})
        }else{
          #PRINT EXIT MESSAGE
          output$FreezerReturnMessage <- renderText({paste("Error")})
        }
        
        #REFRESH REFERENCES
        ShowFreezers(output, database)

        #UPDATE DROPDOWNS
        updateTextInput(session = session, "RenameFreezer2", value = "", placeholder = "New Name")
        updateSelectInput(session = session, inputId = "RenameFreezer1", choices = c("", sampleDB::CheckTable(database = database, "location")$description))
        updateSelectInput(session = session, inputId = "DeleteFreezer", choices = c("", sampleDB::CheckTable(database = database, "location")$description))
        
        }))

    # ~~~~~~ #
    # Delete #
    # ~~~~~~ #
    
    #REMOVE FREEZER FROM DATABASE
    observeEvent(
      input$DeleteFreezerAction,
      ({
        
        #SAVE FREEZER NAMES INVOLVED
        delete.freezer <- input$DeleteFreezer
        
        #DELETE FREEZER IF FREEZER IS NOT IN USE
        if(!(filter(sampleDB::CheckTable(database = database, "location"), description == delete.freezer)$id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id)){
          sampleDB::DeleteFromTable(database = database, table_name = "location",
                                    id = as.character(filter(sampleDB::CheckTable(database = database, "location"),
                                                             description == input$DeleteFreezer)$id))          
          #PRINT EXIT MESSAGE
          output$FreezerReturnMessage <- renderText({paste("Deleted Freezer", delete.freezer, emoji('tada'))})
        }else{
          #PRINT EXIT MESSAGE
          output$FreezerReturnMessage <- renderText({paste("Error")})
        }

        #REFRESH REFERENCES
        ShowFreezers(output, database)

        #UPDATE DROPDOWNS
        updateSelectInput(session, inputId = "RenameFreezer1", choices = c("", sampleDB::CheckTable(database = database, "location")$description))
        updateSelectInput(session, inputId = "DeleteFreezer", choices =  c("", sampleDB::CheckTable(database = database, "location")$description))
      })
    )

    #IDLY PRESENT FREEZERS IN DATATABLE
    ShowFreezers(output, database)

    ##################
    # SPECIMEN TYPES #
    ##################
    
    # ~~~~~ #
    # Check #
    # ~~~~~ #

    #PROTECT AGAINST SPECIMEN TYPE NAME DUPLICATION
    add_specimen_type_duplication_check <- reactive({
        toggle <- input$AddSpecimenType %in% c(sampleDB::CheckTable(database = database, "specimen_type") %>% dplyr::select(label) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("AddSpecimenType", toggle, "Specimen Type names must be unique")})
    
    #PREVENT REPLICATE SPECIMEN TYPE NAMES
    modify_specimen_type_duplication_check <- reactive({
      toggle <- input$RenameSpecimenType2 %in% c(sampleDB::CheckTable(database = database, "specimen_type") %>% dplyr::select(label) %>% dplyr::pull())
      shinyFeedback::feedbackWarning("RenameSpecimenType2", toggle, "Specimen Type names must be unique")})
    
    
    #PROTECT AGAINST DELETION OF SPECIMEN TYPE IN USE
    delete_specimen_delete_warning_check <- reactive({
      specimen_type_id <- CheckTable(database = database, "specimen_type") %>% filter(label == input$DeleteSpecimenType) %>% pull(id)
      toggle <- specimen_type_id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id
      shinyFeedback::feedbackWarning("DeleteSpecimenType", toggle, "Specimen Type is currently is use")})
    
    output$delete_specimen_delete_warning <- renderText(delete_specimen_delete_warning_check())
    output$add_specimen_type_warning <- renderText(add_specimen_type_duplication_check())
    output$modify_specimen_type_warning <- renderText(modify_specimen_type_duplication_check())

    # ~~~ #
    # Add #
    # ~~~ #
    
    #ADD A SPECIMEN TYPE TO THE DATABASE
    observeEvent(
        input$AddSpecimenTypeAction,
        ({
          
          #SAVE SPECIMEN_TYPE NAMES INVOLVED
          new.specimen_type <- input$AddSpecimenType
          
          #ADD SPECIMEN TYPE IF IT IS UNIQUE
          if(!(input$AddSpecimenType %in% c(sampleDB::CheckTable(database = database, "specimen_type")$label))){
            
            sampleDB::AddToTable(database = database, "specimen_type",
                                 list(created = as.character(lubridate::now("UTC")),
                                      last_updated = as.character(lubridate::now("UTC")),
                                      label = input$AddSpecimenType))
            
            output$SpecimenReturnMessage <- renderText({paste("Added Specimen Type", new.specimen_type, emoji('tada'))})
            
          }else{
            output$SpecimenReturnMessage <- renderText({paste("Error")})
          }
          
          #REFRESH REFERENCES
          ShowSpecimenTypes(output, database)

          #UPDATE DROPDOWNS
          updateTextInput(session = session, "AddSpecimenType", value = "", placeholder = "New Name")
          updateSelectInput(session = session, inputId = "RenameSpecimenType1", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
          updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
        }))

    # ~~~~~~ #
    # Modify #
    # ~~~~~~ #
    
    #RENAME A SPECIMEN TPYE
    observeEvent(
      input$RenameSpecimenTypeAction,
      ({
        
        #SAVE SPECIMEN_TYPE NAMES INVOLVED
        old.name <- input$RenameSpecimenType1
        new.name <- input$RenameSpecimenType2
              
        #RENAME SPECIMEN_TYPE IF IT IS UNIQUE
        if(!(new.name %in% c(sampleDB::CheckTable(database = database, "specimen_type")$label))){
          
          sampleDB::ModifyTable(database = database, table_name = "specimen_type",
                                info_list = list(created = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$RenameSpecimenType1)$created),
                                                 last_updated = as.character(lubridate::now("UTC")),
                                                 label = input$RenameSpecimenType2),
                                id = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$RenameSpecimenType1)$id))
          
          output$SpecimenReturnMessage <- renderText({paste("Renamed Specimen Type", old.name, "to", new.name, emoji('tada'))})
          
        }else{
          output$SpecimenReturnMessage <- renderText({paste("Error")})
        }

        #REFRESH REFERENCES
        ShowSpecimenTypes(output, database)

        #UPDATE DROPDOWNS
        updateTextInput(session = session, "RenameSpecimenType2", value = "", placeholder = "New Name")
        updateSelectInput(session = session, inputId = "RenameSpecimenType1", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
        updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
        }))

    # ~~~~~~ #
    # Delete #
    # ~~~~~~ #
    
    #DELETE A SPECIMEN TYPE
    observeEvent(
        input$DeleteSpecimenTypeAction,
        ({
          
          #SAVE SPECIMEN_TYPE NAMES INVOLVED
          delete.specimen_type <- input$DeleteSpecimenType
            
          if(!(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == delete.specimen_type)$id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id)){
            sampleDB::DeleteFromTable(database = database, table_name = "specimen_type",
                                      id = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$DeleteSpecimenType)$id))
            output$SpecimenReturnMessage <- renderText({paste("Deleted Specimen Type", input$DeleteSpecimenType, emoji('tada'))}) 
          }else{
            output$SpecimenReturnMessage <- renderText({paste("Error")})
          }

          #REFRESH REFERENCES
          ShowSpecimenTypes(output, database)

          #UPDATE DROPDOWNS
          updateSelectInput(session = session, inputId = "RenameSpecimenType1", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
          updateSelectInput(session = session, inputId = "DeleteSpecimenType", choices = c("", sampleDB::CheckTable(database = database, "specimen_type")$label))
        }))

    #IDLY PRESENT SPECIMEN TYPES
    ShowSpecimenTypes(output, database)

    ###########
    # STUDIES #
    ###########
    
    # ~~~~~ #
    # Check #
    # ~~~~~ #

    #PROTECT AGAINST STUDY NAME DUPLICATION
    add_study_title_duplication_check <- reactive({
      toggle <- input$AddStudyTitle %in% c(sampleDB::CheckTable(database = database, "study")$title)
      shinyFeedback::feedbackWarning("AddStudyTitle", toggle, "Study titles must be unique")})

    #PROTECT AGAINST STUDY SHORT CODE DUPLICATION
    add_study_short_code_duplication_check <- reactive({
      toggle <- input$AddStudyShortCode %in% c(sampleDB::CheckTable(database = database, "study")$short_code)
      shinyFeedback::feedbackWarning("AddStudyShortCode", toggle, "Study short codes must be unique")})
    
    #PROTECT AGAINST STUDY NAME DUPLICATION
    rename_study_title_duplication_check <- reactive({
      toggle <- input$RenameStudyTitle %in% c(sampleDB::CheckTable(database = database, "study")$title)
      shinyFeedback::feedbackWarning("RenameStudyTitle", toggle, "Study titles must be unique")})
    
    #PROTECT AGAINST STUDY SHORT CODE DUPLICATION
    rename_study_short_code_duplication_check <- reactive({
      toggle <- input$RenameStudyShortCode %in% c(sampleDB::CheckTable(database = database, "study")$short_code)
      shinyFeedback::feedbackWarning("RenameStudyShortCode", toggle, "Study short codes must be unique")})
    
    # #CHECK IF BARCODES ARE ALREADY IN DATABASE
    CheckActiveStudyDelete <- reactive({
      if(!is.null(input$TableStudy_rows_selected)){
        id <- sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]$"id"
        validate(
          need(!(id %in% sampleDB::CheckTable(database = database, "study_subject")$study_id),
               "Study currently in use. Cannot Delete.")
        )
      }
    })
    
    output$add_study_title_warning <- renderText(add_study_title_duplication_check())
    output$add_study_short_code_warning <- renderText(add_study_short_code_duplication_check())
    output$rename_study_title_warning <- renderText(rename_study_title_duplication_check())
    output$rename_study_short_code_warning <- renderText(rename_study_short_code_duplication_check())
    output$WarnActiveStudyDelete <- renderText(CheckActiveStudyDelete())

    # ~~~ #
    # Add #
    # ~~~ #
    
    #ADD A STUDY TO THE DATABASE 
    observeEvent(
        input$AddStudyAction,
        ({
          
          #SAVE STUDY NAMES INVOLVED
          new.title <- input$AddStudyTitle
          new.short_code <- input$AddStudyShortCode
          
          info_list <- list(created = as.character(lubridate::now("UTC")),
                            last_updated = as.character(lubridate::now("UTC")),
                            title = input$AddStudyTitle,
                            description = input$AddStudyDescription,
                            short_code = input$AddStudyShortCode,
                            is_longitudinal = input$AddStudyIsLongitudinal,
                            lead_person = input$AddStudyLeadPerson,
                            hidden = input$AddStudyIsHidden) %>%
            discard(function(x) x == "")
          
          # - CHECK THAT ALL ENTRIES ARE USED
          if(all(c("created", "last_updated", "title", "description", "short_code", "is_longitudinal", "lead_person", "hidden") %in% names(info_list))){
            # - CHECK THAT NEW STUDY HAS UNIQUE TITLE & SHORT CODE
            if(!(new.title %in% sampleDB::CheckTable(database = database, "study")$title) & !(new.short_code %in% sampleDB::CheckTable(database = database, "study")$short_code)){
              sampleDB::AddToTable(database = database, "study",
                                   info_list = info_list)
              output$StudyReturnMessage <- renderText({paste("Added Study to the Database", emoji('tada'))})
            }else{
              output$StudyReturnMessage <- renderText({paste("Error")})
            } 
          }else{
            output$StudyReturnMessage <- renderText({paste("Missing Entries")})
          }

          #REFRESH REFERENCES
          ShowStudies(output, database)

          #UPDATE DROPDOWNS
          StudyUpdateAddDropdowns(session)
        })
    )
    
    # ~~~~~~ #
    # Modify #
    # ~~~~~~ #

    #RENAME A STUDY
    observe({
        observeEvent(
            input$RenameStudyAction,
            ({
              
              #SAVE STUDY NAMES INVOLVED
              new.short_code <- input$RenameStudyShortCode
              new.title <- input$RenameStudyTitle

              #MUST SELECT A ROW FOR MODIFICATION
              if(!is.null(input$TableStudy_rows_selected)){
                  
                  #SET LONGITUDINAL VARIABLE TO WORK WITH TABLE CONSTRAINTS
                  if(input$AddStudyIsLongitudinal == F){
                    is_longitudinal <- 0
                  }else{
                    is_longitudinal <- 1
                  }
                  
                  #SET HIDDEN VARIABLE TO WORK WITH TABLE CONSTRAINTS
                  if(input$AddStudyIsHidden == F){
                    is_hidden <- 0
                  }else{
                    is_hidden <- 1
                  }
                  
                  #CREATE ENTRY THAT WILL REPLACE THE CURRENT ENTRY
                  
                  # - GET OLD ENTRY
                  old.entry <- as.list(sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,])
                  
                  # - CREATE A CONDENSED NEW ENTRY (EXCLUDE EMPTY ITEMS IN LIST)
                  new.entry <- list(title = new.title, 
                                    description = input$RenameStudyDescription, 
                                    short_code = new.short_code, 
                                    lead_person = input$RenameStudyLeadPerson,
                                    is_longitudinal = is_longitudinal,
                                    hidden = is_hidden,
                                    last_updated = as.character(lubridate::now("UTC"))) %>% 
                    discard(function(x) x == "")
                  
                  # - REPLACE ITEMS IN NEW ENTRY WITH THEIR MATES IN OLD ENTRY
                  update.entry <- old.entry
                  for (i in names(new.entry)){
                    update.entry[[i]] <- new.entry[[i]]
                  }
                  
                  #RENAME ENTRY
                  
                  # - SEE IF SHORT CODE OR TITLE ARE BEING RENAMED
                  if("short_code" %in% names(new.entry) | "title" %in% names(new.entry)){
                    # - IF SHORT CODE OR TITLE ARE BEING RENAMED, CHECK THAT THEY ARE UNIQUE
                    if(!(new.title %in% sampleDB::CheckTable(database = database, "study")$title) & !(new.short_code %in% sampleDB::CheckTable(database = database, "study")$short_code)){
                      sampleDB::ModifyTable(database = database, table_name = "study",
                                            info_list = update.entry,
                                            id = as.character(sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]$"id"))
                      
                      output$StudyReturnMessage <- renderText({paste("Modified Study", emoji('tada'))})   
                    }else{
                      
                      output$StudyReturnMessage <- renderText({paste("Error")}) 
                    }
                      
                  }else{
                    # - IF SHORT CODE OR TITLE ARE NOT BEING RENAMED, THEN PROCEED WITH THE RENAME
                    sampleDB::ModifyTable(database = database, table_name = "study",
                                          info_list = update.entry,
                                          id = as.character(sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]$"id"))
                    
                    output$StudyReturnMessage <- renderText({paste("Modified Study", emoji('tada'))})
                  }
                
                #REFRESH REFERENCES
                ShowStudies(output, database)
                
                #UPDATE DROPDOWNS
                StudyUpdateRenameDropdowns(session)
              }
            }))})
    
    # ~~~~~~ #
    # Delete #
    # ~~~~~~ #

        #REMOVE A STUDY FROM THE DATABASE
        observe({
            observeEvent(
                input$DeleteStudyAction,
                ({
                  
                  #MUST SELECT A ROW FOR MODIFICATION
                  if(length(input$TableStudy_rows_selected) ==1 & !is.null(input$TableStudy_rows_selected)){
                    
                    #DELETE STUDY
                    id <- sampleDB::CheckTable(database = database, "study")[input$TableStudy_rows_selected,]$"id"
                    
                    if(!(id %in% sampleDB::CheckTable(database = database, "study_subject")$study_id)){
                      sampleDB::DeleteFromTable(database = database, table_name = "study", id = as.character(id))
                      output$StudyReturnMessage <- renderText({paste("Deleted Study", emoji('tada'))})
                      
                    }else{
                      
                      output$StudyReturnMessage <- renderText({paste("Error")})
                    }
                    
                    #REFRESH REFERENCES
                    ShowStudies(output, database)
                    }
                  })
              )})

        #IDLY SHOW STUDIES
        ShowStudies(output, database)

}
