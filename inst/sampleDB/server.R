

function(input, output, session) {

    #server-side dropdown saves loading time
    updateSelectizeInput(session, 'SearchBySubjectUID', choices = c("", sampleDB::CheckTable("study_subject")$uid %>% unique()), server = TRUE)

    ##################
    # Upload Samples #
    ##################
    upload_plate_dup_check <- reactive({
        even <- input$UploadPlateID %in% c(sampleDB::CheckTable("matrix_plate") %>% dplyr::select(uid) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("UploadPlateID",
                                       even,
                                       "Plate IDs must be unique")
    })
    output$upload_plate_dup_warning <- renderText(upload_plate_dup_check())

    observeEvent(
        input$.UploadAction,
        ({

            output$UploadReturnMessage <- renderText({

                sampleDB::UploadSamples(barcode_file = input$UploadDataSet$datapath,
                                        barcode_type = input$CSVUploadType,
                                        longitudinal = input$LongitudinalUpload,
                                        plate_id = input$UploadPlateID,
                                        location = input$UploadLocation,
                                        study_short_code = input$UploadStudyShortCode,
                                        session = session)

            })
        })
    )

    observeEvent(
        input$.UploadAction,
        ({

            updateSelectizeInput(session = session,
                                 "SearchByPlateID",
                                 choices = sampleDB::CheckTable("matrix_plate")$uid,
                                 label = NULL)

        })
    )

    ##################
    # Search Samples #
    ##################

    output$SearchResultsTable <- DT::renderDataTable({
        # mtcars
    })

    observeEvent(
        input$.SearchAction,
        ({
            # updateSelectizeInput(session = session,
            #                 "SearchByBarcode",
            #                 selected = "")
            #
            # updateSelectizeInput(session = session,
            #                      "SearchByPlateID",
            #                      selected = NULL,
            #                      choices = c())

            # updateSelectizeInput(session = session,
            #                 "SearchBySubjectUID",
            #                 value = "",
            #                 placeholder = "New Name")
            #
            # updateSelectizeInput(session = session,
            #                 "SearchByStudy",
            #                 value = "",
            #                 placeholder = "New Name")
            #
            # updateSelectizeInput(session = session,
            #                 "SearchByLocation",
            #                 value = "",
            #                 placeholder = "New Name")
            #
            # updateSelectizeInput(session = session,
            #                 "SearchBySpecimenType",
            #                 value = "",
            #                 placeholder = "New Name")

            if(is.null(input$SearchByBarcode$datapath)){
                barcode_search_file <- ""
            }else{
                barcode_search_file <- input$SearchByBarcode$datapath
            }

            search_results <- sampleDB::SearchSamples(barcode_search_file = barcode_search_file,
                                                      search_plate_uid = input$SearchByPlateID,
                                                      search_subject_uid = input$SearchBySubjectUID,
                                                      search_study = input$SearchByStudy,
                                                      search_location = input$SearchByLocation,
                                                      search_specimen_type = input$SearchBySpecimenType)

            output$SearchResultsTable <- DT::renderDataTable({
                search_results
            })

            output$downloadData <- downloadHandler(
              filename = function() {
                paste('data-', Sys.Date(), '.csv', sep='')
              },
              content = function(con) {
                write.csv(search_results, con)
              }
            )

        })
    )


    ##############
    # References #
    ##############

    # --- SPECIMEN TYPES

    #show specimen types
    output$TableSpecimenType <- DT::renderDataTable({

        sampleDB::CheckTable("specimen_type") %>%
            rename(`Date Created` = created) %>%
            relocate(`Date Created`)

    })

    # add specimen types
    # prevent specimen type name duplication
    add_specimen_type_duplication_check <- reactive({
        even <- input$AddSpecimenType %in% c(sampleDB::CheckTable("specimen_type") %>% dplyr::select(label) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("AddSpecimenType",
                                       even,
                                       "Specimen Type names must be unique")
    })
    output$add_specimen_type_warning <- renderText(add_specimen_type_duplication_check())

    #add specimen type
    observeEvent(
        input$.AddSpecimenTypeAction,
        ({

            updateTextInput(session = session,
                            "AddSpecimenType",
                            value = "",
                            placeholder = "New Name")

            sampleDB::AddToTable("specimen_type",
                                        list(created = "dummy",
                                             last_updated = "dummy",
                                             label = input$AddSpecimenType))
            output$TableSpecimenType <- DT::renderDataTable({

                sampleDB::CheckTable("specimen_type") %>%
                    rename(`Date Created` = created) %>%
                    relocate(`Date Created`)

            })
            specimen_type_names <- sampleDB::CheckTable("specimen_type") %>%
                dplyr::select(label) %>%
                dplyr::pull()
            updateSelectInput(session = session,
                              inputId = ".RenameSpecimenType1",
                              choices = specimen_type_names)
            updateSelectInput(session = session,
                              inputId = "DeleteSpecimenType",
                              choices = specimen_type_names)
        })
    )


    # remove specimen type
    observeEvent(
        input$.DeleteSpecimenTypeAction,
        ({
            id <- sampleDB::CheckTable("specimen_type") %>%
                filter(label == input$DeleteSpecimenType) %>%
                dplyr::select(id) %>% dplyr::pull()
            sampleDB::DeleteFromTable(table_name = "specimen_type",
                                             id = as.character(id))
            output$TableSpecimenType <- DT::renderDataTable({

                sampleDB::CheckTable("specimen_type") %>%
                    rename(`Date Created` = created) %>%
                    relocate(`Date Created`)
            })
            specimen_type_names <- sampleDB::CheckTable("specimen_type") %>%
                dplyr::select(label) %>%
                dplyr::pull()
            updateSelectInput(session = session,
                              inputId = ".RenameSpecimenType1",
                              choices = specimen_type_names)
            updateSelectInput(session = session,
                              inputId = "DeleteSpecimenType",
                              choices = specimen_type_names)
        })
    )

    modify_specimen_type_duplication_check <- reactive({
        even <- input$RenameSpecimenType2 %in% c(sampleDB::CheckTable("specimen_type") %>% dplyr::select(label) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("RenameSpecimenType2",
                                       even,
                                       "Specimen Type names must be unique")
    })
    output$modify_specimen_type_warning <- renderText(modify_specimen_type_duplication_check())

    # modify specimen type

    delete_specimen_delete_warning_check <- reactive({
      specimen_type_id <- CheckTable("specimen_type") %>% filter(label == input$DeleteSpecimenType) %>% pull(id)
      even <- specimen_type_id %in% sampleDB::CheckTable("specimen")$specimen_type_id
      shinyFeedback::feedbackWarning("DeleteSpecimenType",
                                     even,
                                     "Specimen Type is currently is use")
    })
    output$delete_specimen_delete_warning <- renderText(delete_specimen_delete_warning_check())

    observeEvent(
        input$.RenameSpecimenTypeAction,
        ({

            updateTextInput(session = session,
                            "RenameSpecimenType2",
                            value = "",
                            placeholder = "New Name")

            id <- sampleDB::CheckTable("specimen_type") %>%
                filter(label == input$.RenameSpecimenType1) %>%
                dplyr::select(id) %>% dplyr::pull()

            sampleDB::ModifyTable(table_name = "specimen_type",
                                         info_list = list(created = "dummy",
                                                          last_updated = "dummy",
                                                          label = input$RenameSpecimenType2),
                                         id = as.character(id))

            output$TableSpecimenType <- DT::renderDataTable({

                sampleDB::CheckTable("specimen_type") %>%
                    rename(`Date Created` = created) %>%
                    relocate(`Date Created`)

            })
            specimen_type_names <- sampleDB::CheckTable("specimen_type") %>%
                dplyr::select(label) %>%
                dplyr::pull()
            updateSelectInput(session = session,
                              inputId = ".RenameSpecimenType1",
                              choices = specimen_type_names)
            updateSelectInput(session = session,
                              inputId = "DeleteSpecimenType",
                              choices = specimen_type_names)
        })
    )

    # --- FREEZERS

    # show freezers
    output$TableFreezer <- DT::renderDataTable({

        sampleDB::CheckTable("location") %>%
            dplyr::select(created, description) %>%
            rename(`Date Created` = created, Name = description) %>%
            relocate(Name, `Date Created`)

        })

    # add freezers
        # prevent freezer name duplication
        add_freezer_duplication_check <- reactive({
            even <- input$AddFreezer %in% sampleDB::CheckTable("location")$description
            shinyFeedback::feedbackWarning("AddFreezer",
                                           even,
                                           "Freezer names must be unique")
        })
        output$add_freezer_warning <- renderText(add_freezer_duplication_check())

    observeEvent(
        input$.AddFreezerAction,
        ({

            updateTextInput(session = session,
                            "AddFreezer",
                            value = "",
                            placeholder = "New Name")

            sampleDB::AddToTable("location",
                                        list(created = "dummy",
                                             last_updated = "dummy",
                                             description = input$AddFreezer))
            output$TableFreezer <- DT::renderDataTable({

                sampleDB::CheckTable("location") %>%
                    dplyr::select(created, description) %>%
                    rename(`Date Created` = created, Name = description) %>%
                    relocate(Name, `Date Created`)

            })
            freezer_names <- sampleDB::CheckTable("location") %>%
                dplyr::select(description) %>%
                dplyr::pull()
            updateSelectInput(session = session,
                              inputId = ".RenameFreezer1",
                              choices = freezer_names)
            updateSelectInput(session = session,
                              inputId = "DeleteFreezer",
                              choices = freezer_names)
        })
    )

    # remove freezers

    delete_freezer_delete_warning_check <- reactive({
      freezer_id <- CheckTable("location") %>% filter(description == input$DeleteFreezer) %>% pull(id)
      even <- freezer_id %in% sampleDB::CheckTable("matrix_plate")$location_id
      shinyFeedback::feedbackWarning("DeleteFreezer",
                                     even,
                                     "Freezer is currently is use")
    })
    output$delete_freezer_delete_warning <- renderText(delete_freezer_delete_warning_check())

    observeEvent(
        input$.DeleteFreezerAction,
        ({
            id <- sampleDB::CheckTable("location") %>%
                filter(description == input$DeleteFreezer) %>%
                dplyr::select(id) %>% dplyr::pull()
            sampleDB::DeleteFromTable(table_name = "location",
                                             id = as.character(id))
            output$TableFreezer <- DT::renderDataTable({

                sampleDB::CheckTable("location") %>%
                    dplyr::select(created, description) %>%
                    rename(`Date Created` = created, Name = description) %>%
                    relocate(Name, `Date Created`)

            })
            freezer_names <- sampleDB::CheckTable("location") %>%
                dplyr::select(description) %>%
                dplyr::pull()
            updateSelectInput(session = session,
                              inputId = ".RenameFreezer1",
                              choices = freezer_names)
            updateSelectInput(session = session,
                              inputId = "DeleteFreezer",
                              choices = freezer_names)
        })
    )

    # modify freezers
        modify_freezer_duplication_check <- reactive({
            even <- input$RenameFreezer2 %in% c(sampleDB::CheckTable("location") %>% dplyr::select(description) %>% dplyr::pull())
            shinyFeedback::feedbackWarning("RenameFreezer2",
                                           even,
                                           "Freezer names must be unique")
        })
        output$modify_freezer_warning <- renderText(modify_freezer_duplication_check())

    observeEvent(
        input$.RenameFreezerAction,
        ({

            updateTextInput(session = session,
                            "RenameFreezer2",
                            value = "",
                            placeholder = "New Name")

            id <- sampleDB::CheckTable("location") %>%
                filter(description == input$.RenameFreezer1) %>%
                dplyr::select(id) %>% dplyr::pull()

            sampleDB::ModifyTable(table_name = "location",
                                         info_list = list(created = "dummy",
                                                          last_updated = "dummy",
                                                          description = input$RenameFreezer2),
                                         id = as.character(id))

            output$TableFreezer <- DT::renderDataTable({

                sampleDB::CheckTable("location") %>%
                    dplyr::select(created, description) %>%
                    rename(`Date Created` = created, Name = description) %>%
                    relocate(Name, `Date Created`)

            })
            freezer_names <- sampleDB::CheckTable("location") %>%
                dplyr::select(description) %>%
                dplyr::pull()
            updateSelectInput(session = session,
                              inputId = ".RenameFreezer1",
                              choices = freezer_names)
            updateSelectInput(session = session,
                              inputId = "DeleteFreezer",
                              choices = freezer_names)
        })
    )

    # --- STUDIES

    # show studies
    output$TableStudy <- DT::renderDataTable({

        sampleDB::CheckTable("study") %>%
            dplyr::select(-c(id, created, last_updated, hidden))

    },
    selection = 'single')

    # add study

    # prevent freezer name duplication
    add_study_title_duplication_check <- reactive({
        even <- input$AddStudyTitle %in% c(sampleDB::CheckTable("study") %>% dplyr::select(title) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("AddStudyTitle",
                                       even,
                                       "Study titles must be unique")
    })
    output$add_study_title_warning <- renderText(add_study_title_duplication_check())

    # prevent freezer name duplication
    add_study_short_code_duplication_check <- reactive({
        even <- input$AddStudyShortCode %in% c(sampleDB::CheckTable("study") %>% dplyr::select(short_code) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("AddStudyShortCode",
                                       even,
                                       "Study short codes must be unique")
    })
    output$add_study_short_code_warning <- renderText(add_study_short_code_duplication_check())

    observeEvent(
        input$.AddStudyAction,
        ({
            updateTextInput(session = session,
                            "AddStudyTitle",
                            value = "",
                            placeholder = "New Title")

            updateTextInput(session = session,
                            "AddStudyDescription",
                            value = "",
                            placeholder = "New Description")

            updateTextInput(session = session,
                            "AddStudyLeadPerson",
                            value = "",
                            placeholder = "New Lead Person")

            updateTextInput(session = session,
                            "AddStudyShortCode",
                            value = "",
                            placeholder = "New Short Code")

            updateCheckboxInput(session = session,
                                "AddStudyIsLongitudinal",
                                value = FALSE)

            updateCheckboxInput(session = session,
                                "AddStudyIsHidden",
                                value = FALSE)

            sampleDB::AddToTable("study",
                                        list(created = "dummy",
                                             last_updated = "dummy",
                                             title = input$AddStudyTitle,
                                             description = input$AddStudyDescription,
                                             short_code = input$AddStudyShortCode,
                                             is_longitudinal = input$AddStudyIsLongitudinal,
                                             lead_person = input$AddStudyLeadPerson,
                                             hidden = input$AddStudyIsHidden))

            output$TableStudy <- DT::renderDataTable({
                sampleDB::CheckTable("study") %>%
                    dplyr::select(-c(id, created, last_updated, hidden))

            })
        })
    )


    # rename study

    # prevent freezer name duplication
    rename_study_title_duplication_check <- reactive({
        even <- input$RenameStudyTitle %in% c(sampleDB::CheckTable("study") %>% dplyr::select(title) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("RenameStudyTitle",
                                       even,
                                       "Study titles must be unique")
    })
    output$rename_study_title_warning <- renderText(rename_study_title_duplication_check())

    # prevent freezer name duplication
    rename_study_short_code_duplication_check <- reactive({
        even <- input$RenameStudyShortCode %in% c(sampleDB::CheckTable("study") %>% dplyr::select(short_code) %>% dplyr::pull())
        shinyFeedback::feedbackWarning("RenameStudyShortCode",
                                       even,
                                       "Study short codes must be unique")
    })
    output$rename_study_short_code_warning <- renderText(rename_study_short_code_duplication_check())

    observe({

        observeEvent(
            input$.RenameStudyAction,
            ({


                updateTextInput(session = session,
                                "RenameStudyTitle",
                                value = "",
                                placeholder = "New Title")

                updateTextInput(session = session,
                                "RenameStudyDescription",
                                value = "",
                                placeholder = "New Description")

                updateTextInput(session = session,
                                "RenameStudyLeadPerson",
                                value = "",
                                placeholder = "New Lead Person")

                updateTextInput(session = session,
                                "RenameStudyShortCode",
                                value = "",
                                placeholder = "New Short Code")

                updateCheckboxInput(session = session,
                                    "RenameStudyIsLongitudinal",
                                    value = FALSE)

                updateCheckboxInput(session = session,
                                    "RenameStudyIsHidden",
                                    value = FALSE)

                info_list <- sampleDB::CheckTable("study")[input$TableStudy_rows_selected,] %>% as.list()
                # id <- info_list[[id]]
                id <- info_list$id
                info_list <- within(info_list, rm(id))

                if(input$RenameStudyTitle != ""){
                    info_list$title <- input$RenameStudyTitle
                }
                if(input$RenameStudyDescription != ""){
                    info_list$description <- input$RenameStudyDescription
                }
                if(input$RenameStudyShortCode != ""){
                    info_list$short_code <- input$RenameStudyShortCode
                }
                if(input$RenameStudyLeadPerson != ""){
                info_list$lead_person <- input$RenameStudyLeadPerson
                }
                print(info_list)
                # if(input$RenameStudyIsLongitudinal != ""){
                # info_list$is_longitudinal <- input$RenameStudyIsLongitudinal
                # }
                # if(input$RenameStudyIsHidden != ""){
                # info_list$is_hidden <- input$RenameStudyIsHidden
                # }

                sampleDB::ModifyTable(table_name = "study",
                                             info_list = info_list,
                                             id = as.character(id))

                output$TableStudy <- DT::renderDataTable({

                    sampleDB::CheckTable("study") %>%
                        dplyr::select(-c(id, created, last_updated, hidden))

                })
            })
        )
    })


        #remove study
        observe({

            observeEvent(
                input$DeleteStudyAction,

                ({
                    info_list <- sampleDB::CheckTable("study")[input$TableStudy_rows_selected,] %>% as.list()
                    id <- info_list$id
                    info_list <- within(info_list, rm(id))

                    output$DeleteStudy <- renderPrint({
                      if(id %in% sampleDB::CheckTable("study_subject")$study_id){
                        print("*ERROR: Study is currently in use*")
                      }})

                    sampleDB::DeleteFromTable(table_name = "study", id = as.character(id))

                    output$TableStudy <- DT::renderDataTable({

                        sampleDB::CheckTable("study") %>%
                            dplyr::select(-c(id, created, last_updated, hidden))

                    })
                })
            )
    })



}
