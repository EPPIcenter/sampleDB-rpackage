library(dplyr)
library(sampleDB)
library(DT)
library(shinyFeedback)
library(shiny)
library(markdown)
library(lubridate)

function(input, output, session) {

    #SET PATH TO SQLITE DATABASE
    database <- "example_19-Oct-21.sample_db.sqlite"

    #SERVER-SIDE DROPDOWN -- SAVES LOADING TIME
    updateSelectizeInput(session, 'SearchBySubjectUID', choices = c("", sampleDB::CheckTable(database = database, "study_subject")$uid %>% unique()), server = TRUE)

    ##################
    # Upload Samples #
    ##################

    #CHECK PLATE_ID IS UNIQUE
    upload_plate_dup_check <- reactive({
        toggle <- input$UploadPlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)
        shinyFeedback::feedbackWarning("UploadPlateID", toggle, "Plate IDs must be unique")})
    output$upload_plate_dup_warning <- renderText(upload_plate_dup_check())

    #UPLOAD PLATE
    observeEvent(
        input$.UploadAction,
        ({
          output$UploadReturnMessage <- renderText({

              sampleDB::UploadSamples(database = database,
                                      barcode_file = input$UploadDataSet$datapath,
                                      plate_id = input$UploadPlateID,
                                      location = input$UploadLocation,
                                      study_short_code = input$UploadStudyShortCode,
                                      session = session)})}))

    observeEvent(
        input$.UploadAction,
        ({
            updateSelectizeInput(session = session,
                                 "SearchByPlateID",
                                 choices = sampleDB::CheckTable(database = database, "matrix_plate")$uid,
                                 label = NULL)}))

    output$uploadcsv_nodate_example <- renderPrint({
      tibble(LocationRow = rep("A", 10),
             LocationColumn = c(1:10),
             TubeCodes = CheckTable(database = database, "matrix_tube")$barcode %>% head(10),
             study_subject_id = CheckTable(database = database, "study_subject")$uid %>% head(10),
             specimen_type = "PLASMA") %>% as.data.frame()
    })

    output$uploadcsv_date_example <- renderPrint({
      tibble(LocationRow = rep("A", 10),
             LocationColumn = c(1:10),
             TubeCodes = CheckTable(database = database, "matrix_tube")$barcode %>% head(10),
             study_subject_id = CheckTable(database = database, "study_subject")$uid %>% head(10),
             specimen_type = "PLASMA",
             collection_date = paste("2022", "1", c(1,1,1,2,2,2,3,3,3,4), sep = "-")) %>% as.data.frame()
    })

    ##################
    # Search Samples #
    ##################

    #CHECK PLATE_ID IS UNIQUE
    move_plate_dup_check <- reactive({
      toggle <- input$MovePlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)
      shinyFeedback::feedbackWarning("MovePlateID", toggle, "Plate IDs must be unique")
      })

    output$move_plate_dup_warning <- renderText(move_plate_dup_check())

    #SEARCH SAMPLES
    observe({
      if(is.null(input$SearchByBarcode$datapath)){
        barcode_search_file <- ""
      }else{
        barcode_search_file <- input$SearchByBarcode$datapath
      }

      search_results <- sampleDB::SearchSamples(database = database,
                                                barcode_search_file = barcode_search_file,
                                                search_plate_uid = input$SearchByPlateID,
                                                search_subject_uid = input$SearchBySubjectUID,
                                                search_study = input$SearchByStudy,
                                                search_location = input$SearchByLocation,
                                                search_specimen_type = input$SearchBySpecimenType)

      output$SearchResultsTable <- DT::renderDataTable({
        search_results
      }, options =
        list(searching = T, paging = T,
             language = list(
               zeroRecords = "No samples match filters given")))

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

    #CHECK PLATE_ID IS UNIQUE
    move_plate_dup_check <- reactive({
      toggle <- input$MovePlateID %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)
      shinyFeedback::feedbackWarning("MovePlateID", toggle, "Plate IDs must be unique")})
    output$move_plate_dup_warning <- renderText(move_plate_dup_check())

    observeEvent(
      input$.MoveAction,
      ({

        output$UploadReturnMessage <- renderText({

          sampleDB::MoveTubes(database = database,
                            barcode_file = input$MoveDataSet$datapath,
                            new_plate_uid = input$MovePlateID,
                            existing_plate_uid = input$MoveExistingPlateID,
                            location = input$MoveLocation,
                            session = session)})}))

    observeEvent(
      input$.MoveAction,
      ({
        updateSelectizeInput(session = session,
                             "SearchByPlateID",
                             choices = sampleDB::CheckTable(database = database, "matrix_plate")$uid,
                             label = NULL)}))

    output$movetubescsv_example <- renderPrint({
      tibble(LocationRow = rep("A", 10),
             LocationColumn = c(1:10),
             TubeCodes = CheckTable(database = database, "matrix_tube")$barcode %>% head(10)) %>% as.data.frame()
    })

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
