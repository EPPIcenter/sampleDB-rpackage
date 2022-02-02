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
    
    CheckStudySubjectLongitudinal <- reactive({helper.CheckStudySubjectLongitudinal(input, database)})
    output$WarningStudySubjectLongitudinal <- renderText(CheckStudySubjectLongitudinal())

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
      
      # PLAN:
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      # Search function should be able to accept subject_ids as a vector or as a string #
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      
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
    
    # CLEAR FILES
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
    
    # PLAN:
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Move function should take, as the `barcode_file` arg, a list of paths/to/file/platename.csv  #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
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
            
          # CHECK REQUIREMENTS
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
      
    # PLAN:
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Is this deleting plate function straightforward #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
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
    
    # PLAN:
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Make one or multiple functions for adding, deleting and modifying references #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
    
    # ~~~~~ #
    # Check #
    # ~~~~~ #

    #PROTECT AGAINST REDUNDANT FREEZER NAMES
    CheckFreezerNameAddUnique <- reactive({helper.CheckFreezerNameUnique("AddFreezer", type.dup = "names", input, database)})
    output$WarningFreezerNameAddUnique <- renderText(CheckFreezerNameAddUnique())
    
    #PREVENT DUPLICATION OF FREEZER NAMES
    CheckFreezerNameChangeUnique <- reactive({helper.CheckFreezerNameUnique("RenameFreezer2", type.dup = "names", input, database)})
    output$WarningFreezerNameChangeUnique <- renderText(CheckFreezerNameChangeUnique())
    
    #PREVENT DELETION OF FREEZER THAT IS IN USE
    CheckFreezerDeletion <- reactive({helper.CheckFreezerDeletion(input, database)})
    output$WarningFreezerDeletion <- renderText(CheckFreezerDeletion())
    
    # ~~~ #
    # Add #
    # ~~~ #

    #ADD FREEZER TO DATABASE
    observeEvent(
      input$AddFreezerAction,
      ({
        
        #SAVE FREEZER NAMES INVOLVED
        new.freezer <- input$AddFreezer
        
        #SET REQUIREMENTS
        req(input$AddFreezer,
            !(new.freezer %in% c(sampleDB::CheckTable(database = database, "location")$description)))
        
        #ADD FREEZER NAME
        sampleDB::AddToTable(database = database, "location",
                             list(created = as.character(lubridate::now("UTC")),
                                  last_updated = as.character(lubridate::now("UTC")),
                                  description = input$AddFreezer))          
        
        #PRINT EXIT MESSAGE
        output$FreezerReturnMessage <- renderText({paste("Added Freezer", new.freezer, emoji('tada'))})

        #MODIFY TABLE
        ShowFreezers(output, database)

        #UPDATE DROPDOWNS
        UpdateFreezerDropdowns(database, session)
      }))

    # ~~~~~~ #
    # CHANGE #
    # ~~~~~~ #
    
    #CHANGE FREEZER NAMES
    observeEvent(
      input$RenameFreezerAction,
      ({

        #SAVE FREEZER NAMES INVOLVED
        old.name <- input$RenameFreezer1
        new.name <- input$RenameFreezer2
        
        #MODIFY TABLE IF NEW FREEZER NAME IS UNIQUE
        req(input$RenameFreezer1,
            input$RenameFreezer2,
            !(new.name %in% c(sampleDB::CheckTable(database = database, "location")$description)))
        
        #CHANGE FREEZER NAME
        sampleDB::ModifyTable(database = database, table_name = "location",
                              info_list = list(created = as.character(filter(sampleDB::CheckTable(database = database, "location"), 
                                                                             description == input$RenameFreezer1)$created),
                                               last_updated = as.character(lubridate::now("UTC")),
                                               description = input$RenameFreezer2),
                              id = as.character(filter(sampleDB::CheckTable(database = database, "location"), description == input$RenameFreezer1)$id))
        
        #PRINT EXIT MESSAGE
        output$FreezerReturnMessage <- renderText({paste("Renamed Freezer", old.name, "to", new.name, emoji('tada'))})
        
        #REFRESH REFERENCES
        ShowFreezers(output, database)

        #UPDATE DROPDOWNS
        UpdateFreezerDropdowns(database, session)
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
        
        #SET REQUIREMENTS
        req(input$DeleteFreezer,
            !(filter(sampleDB::CheckTable(database = database, "location"), description == delete.freezer)$id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id))
        
        #DELETE FREEZER
        sampleDB::DeleteFromTable(database = database, table_name = "location",
                                  id = as.character(filter(sampleDB::CheckTable(database = database, "location"), description == input$DeleteFreezer)$id))          
        #PRINT EXIT MESSAGE
        output$FreezerReturnMessage <- renderText({paste("Deleted Freezer", delete.freezer, emoji('tada'))})
        
        #REFRESH REFERENCES
        ShowFreezers(output, database)

        #UPDATE DROPDOWNS
        UpdateFreezerDropdowns(database, session)
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
    CheckAddSpecimenTypeUnique <- reactive({helper.CheckSpecimenTypeUnique("AddSpecimenType", type.dup = "names", input, database)})
    output$WaringAddSpecimenTypeUnique <- renderText(CheckAddSpecimenTypeUnique())
    
    #PREVENT REPLICATE SPECIMEN TYPE NAMES
    CheckChangeSpecimenTypeUnique <- reactive({helper.CheckSpecimenTypeUnique("RenameSpecimenType2", type.dup = "names", input, database)})
    output$WarningChangeSpecimenTypeUnique <- renderText(CheckChangeSpecimenTypeUnique())
    
    #PROTECT AGAINST DELETION OF SPECIMEN TYPE IN USE
    CheckSpecimenTypeDeletion <- reactive({helper.CheckSpecimenTypeDeletion(input, database)})
    output$WarningSpecimenTypeDeletion <- renderText(CheckSpecimenTypeDeletion())

    # ~~~ #
    # Add #
    # ~~~ #
    
    #ADD A SPECIMEN TYPE TO THE DATABASE
    observeEvent(
        input$AddSpecimenTypeAction,
        ({
          
          #SAVE SPECIMEN_TYPE NAMES INVOLVED
          new.specimen_type <- input$AddSpecimenType
          
          #SET REQUIREMENT
          req(input$AddSpecimenType,
              !(new.specimen_type %in% c(sampleDB::CheckTable(database = database, "specimen_type")$label)))
          
          #ADD SPECIMEN TYPE
          sampleDB::AddToTable(database = database, "specimen_type",
                               list(created = as.character(lubridate::now("UTC")),
                                    last_updated = as.character(lubridate::now("UTC")),
                                    label = input$AddSpecimenType))
          
          #PRINT EXIT MESSAGE
          output$SpecimenReturnMessage <- renderText({paste("Added Specimen Type", new.specimen_type, emoji('tada'))})
          
          #REFRESH REFERENCES
          ShowSpecimenTypes(output, database)

          #UPDATE DROPDOWNS
          UpdateSpecimenTypeDropdowns(database, session)
        }))

    # ~~~~~~ #
    # Change #
    # ~~~~~~ #
    
    #CHANGE A SPECIMEN TPYE
    observeEvent(
      input$RenameSpecimenTypeAction,
      ({
        
        #SAVE SPECIMEN_TYPE NAMES INVOLVED
        old.name <- input$RenameSpecimenType1
        new.name <- input$RenameSpecimenType2
              
        #CHANGE SPECIMEN_TYPE IF IT IS UNIQUE
        req(input$RenameSpecimenType1,
            input$RenameSpecimenType2,
            !(new.name %in% c(sampleDB::CheckTable(database = database, "specimen_type")$label)))
        
        #CHANGE SPECIMEN TYPE NAME
        sampleDB::ModifyTable(database = database, table_name = "specimen_type",
                              info_list = list(created = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$RenameSpecimenType1)$created),
                                               last_updated = as.character(lubridate::now("UTC")),
                                               label = input$RenameSpecimenType2),
                              id = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$RenameSpecimenType1)$id))
        
        #PRINT EXIT MESSAGE
        output$SpecimenReturnMessage <- renderText({paste("Renamed Specimen Type", old.name, "to", new.name, emoji('tada'))})

        #REFRESH REFERENCES
        ShowSpecimenTypes(output, database)

        #UPDATE DROPDOWNS
        UpdateSpecimenTypeDropdowns(database, session)
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
          
          #SET REQUIREMENT
          req(input$DeleteSpecimenTypeAction,
              !(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == delete.specimen_type)$id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id))
          
          #DELETE SPECIMEN TYPE
          sampleDB::DeleteFromTable(database = database, 
                                    table_name = "specimen_type",
                                    id = as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == input$DeleteSpecimenType)$id))
          
          #PRINT EXIT MESSAGE
          output$SpecimenReturnMessage <- renderText({paste("Deleted Specimen Type", input$DeleteSpecimenType, emoji('tada'))})

          #REFRESH REFERENCES
          ShowSpecimenTypes(output, database)

          #UPDATE DROPDOWNS
          UpdateSpecimenTypeDropdowns(database, session)
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
    CheckStudyAddTitleUnique <- reactive({helper.CheckStudyUnique("AddStudyTitle", type.dup = "title", input, database)})
    output$WarningStudyAddTitleUnique <- renderText(CheckStudyAddTitleUnique())
    
    #PROTECT AGAINST STUDY NAME DUPLICATION
    CheckStudyChangeTitleUnique <- reactive({helper.CheckStudyUnique("RenameStudyTitle", type.dup = "title", input, database)})
    output$WarningStudyChangeTitleUnique <- renderText(CheckStudyChangeTitleUnique())

    #PROTECT AGAINST STUDY SHORT CODE DUPLICATION
    CheckStudyAddShortCodeUnique <- reactive({helper.CheckStudyUnique("AddStudyShortCode", type.dup = "short code", input, database)})
    output$WarningStudyAddShortCodeUnique <- renderText(CheckStudyAddShortCodeUnique())
    
    #PROTECT AGAINST STUDY SHORT CODE DUPLICATION
    CheckStudyChangeShortCodeUnique <- reactive({helper.CheckStudyUnique("RenameStudyShortCode", type.dup = "short code", input, database)})
    output$WarningStudyChangeShortCodeUnique <- renderText(CheckStudyChangeShortCodeUnique())
    
    # #CHECK IF BARCODES ARE ALREADY IN DATABASE
    CheckStudyDeletion <- reactive({helper.CheckStudyDeletion(input, database)})
    output$WarnStudyDeletion <- renderText(CheckStudyDeletion())

    # ~~~ #
    # Add #
    # ~~~ #
    
    #NEED TO TEST
    #ADD A STUDY TO THE DATABASE 
    observeEvent(
        input$AddStudyAction,
        ({
          
          # SET REQUIREMENTS
          AddStudyRequirements(input)
          
          #SAVE STUDY NAMES INVOLVED
          info_list <- list(created = as.character(lubridate::now("UTC")),
                            last_updated = as.character(lubridate::now("UTC")),
                            title = input$AddStudyTitle,
                            description = input$AddStudyDescription,
                            short_code = input$AddStudyShortCode,
                            is_longitudinal = input$AddStudyIsLongitudinal,
                            lead_person = input$AddStudyLeadPerson,
                            hidden = input$AddStudyIsHidden)
          
          #ADD STUDY
          sampleDB::AddToTable(database = database, "study", info_list = info_list)
          
          #PRINT EXIT MESSAGE
          output$StudyReturnMessage <- renderText({paste("Added Study to the Database", emoji('tada'))})

          #REFRESH REFERENCES
          ShowStudies(output, database)

          #UPDATE DROPDOWNS
          UpdateStudyDropdowns(session)
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
                UpdateStudyDropdowns(input, session)
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
