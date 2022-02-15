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
for(helper in list.files(path = "helpers", full.names = T, recursive = T)){source(helper, local = TRUE)}

function(input, output, session) {
  
    # SET PATH TO SQLITE DATABASE - WOULD PREFER DATABASE TO BE AT Sys.getenv("SAMPLEDB_DATABASE")
    database <- "/databases/new.sampleDB.db"

    # SERVER-SIDE DROPDOWN -- SAVES LOADING TIME
    updateSelectizeInput(session, 'SearchBySubjectUID', choices = c("", sampleDB::CheckTable(database = database, "study_subject")$subject %>% unique()), server = TRUE)

    # --------- Upload Samples -------------
    
    # Upload Micronix Samples
    MatrixUpload(session = session, output = output, input = input, database = databse, ref.clear_action = "ClearMicronixUploadForm")
    
    # Upload Cryo Samples
    CryoUpload(session, output, input, database, ref.clear_action = "ClearCryoUploadForm")

    # Upload RDT Samples
    RDTUpload(session, output, input, database, ref.clear_action = "ClearRDTUploadForm")

    # Upload Paper Samples
    PaperUpload(session, output, input, database, ref.clear_action = "ClearPaperUploadForm")
    
    # -------- Search Samples -------------

    # SEARCH CHECKS... CHECK THAT SEARCH FILES ARE NOT MALFORMED
    # SearchChecks(input, database, output)
    
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

    # ACTIVELY USE UI FILTERS TO RENDER A TABLE WITH SEARCH RESULTS
    # observe({
    # 
    #   filters <- list(file.barcodes = input$SearchByBarcode$datapath,
    #                   name.plate = input$SearchByPlateID,
    #                   name.study = input$SearchByStudy,
    #                   name.location = input$SearchByLocation,
    #                   name.specimen_type = input$SearchBySpecimenType)
    #   
    #   # RETRIEVE SEARCH RESULTS
    #   if(input$SubjectUIDSearchType == "individual"){
    #     filters$name.study_subject <- input$SearchBySubjectUID
    #     search_results <- sampleDB::SearchSamples(discard(filters, function(x) "" %in% x), study_subject.file = F)
    #   }else{
    #     filters$name.study_subject <- input$SearchBySubjectUIDFile$datapath
    #     search_results <- sampleDB::SearchSamples(discard(filters, function(x) "" %in% x), study_subject.file = T)
    #   }
    # 
    #   # PRINT SEARCH RESULTS
    #   output$SearchResultsTable <- DT::renderDataTable({
    #     if(is.null(search_results)){
    #       tibble(`Well Position` = NA,
    #              `Barcode` = NA,
    #              `Study Subject` = NA,
    #              `Study Code` = NA,
    #              `Specimen Type` = NA,
    #              `Storage Location` = NA,
    #              `Plate Name` = NA,
    #              `Collected Date` = NA) %>% 
    #         filter(`Well Position` == 0)
    #     }else{
    #       search_results 
    #     }
    #     
    #   },
    #     options = list(
    #       searching = T,
    #       paging = T,
    #       pageLength = 20,
    #       lengthMenu = c(10, 20, 50, 100),
    #       language = list(zeroRecords = "There are no EPPIcenter Wetlab Samples that match this search.")))
    # 
    #   # DOWNLOAD SEARCH RESULTS
    #   output$downloadData <- downloadHandler(
    #               filename = function() {
    #                 paste('data-', Sys.Date(), '.csv', sep='')
    #               },
    #               content = function(con) {
    #                 write.csv(search_results, con)
    #               }
    #             )
    # })
    # 
    # CLEAR FILES
    # SearchReset(input)
    
    ##############
    # Move Tubes #
    ##############

    # Run Checks
    # MoveChecks(input,database, output)
    
    # PLAN:
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Move function should take, as the `barcode_file` arg, a list of paths/to/file/platename.csv  #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
    # observeEvent(
    #   input$MoveAction,
    #   ({
    #     
    #     # TRIGGER UI CHANGE FOR REACTIVITY - RECYCLE RENAMESTUDYLEADPERSON
    #     updateTextInput(session = session, "RenameStudyLeadPerson", value = "a6sFH$DKdsbgGLY9")
    #     
    #     # PAUSE FOR EFFECT AND PRINT WORKING
    #     Sys.sleep(.75)
    #     output$MoveReturnMessage1 <- renderText({"Working..."})
    #     
    #   }))
    # 
    #   # UPLOAD SAMPLES
    #   observe({
    #       
    #     # WHEN REACTIVE UI IS CHANGED TO INDICATE AN UPLOAD
    #     if(input$RenameStudyLeadPerson == "a6sFH$DKdsbgGLY9"){
    #         
    #       # CHECK REQUIREMENTS
    #       # MoveRequirements(input, database)
    #       
    #       #CREATE LIST FOR MOVE
    #       eval.file.barcode <- list()
    #       for(i in 1:length(input$MoveDataSet[,1])){
    #         plate.name <- input$MoveDataSet[[i, 'name']] %>% gsub("\\.csv","",.)
    #         eval.file.barcode[[plate.name]] <- input$MoveDataSet[[i, 'datapath']]
    #       }
    #       
    #       # MOVE SAMPLES -- FUN INPUT SHOULD PROBABLY BE A LIST OF FILES
    #       message <- sampleDB::MoveTubes(file.barcode = eval.file.barcode)
    # 
    #       # PRINT UPLOAD MSG
    #       output$MoveReturnMessage2 <- renderText({message})
    # 
    #       # RESET UI VALUE
    #       updateTextInput(session = session, "RenameStudyLeadPerson", value = "")
    #     }
    #   })
    #     
    #   # CLEAR FORM
    #   MoveReset(input, output)
    #   
    #   # EXAMPLES
    #   MoveExamples(input, database, output)

    ######################
    # Delete Empty Plate #
    ######################
      
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # CAN THIS FUN BE USED IN THE CONSOLE? #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
    # DeletePlateChecks(input, database, output)
    # 
    # observeEvent(
    #   input$DeletePlateAction,
    #   ({
    #     
    #     # SET REQUIREMENT FOR DELETEING PLATE
    #     DeleteEmptyPlateRequirement(input, database)
    #     
    #     # DELETE PLATE
    #     plate_name <- input$DeletePlateName
    #     output$DeletePlateMessage <- renderText({sampleDB::DeleteEmptyPlates(database = database, plate_name = plate_name)})
    #     
    #     # RESET PLATE NAMES DROPDOWN
    #     DeleteEmptyPlateReset(session, database)
    #   }))
    #   
    #REFERENCES#################################################################################

    ############
    # FREEZERS #
    ############
    
    # PLAN:
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Make one or multiple functions for adding, deleting and modifying references #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
    # FreezerChangesChecks(input, database, output)
    # 
    # #ADD FREEZER TO DATABASE
    # observeEvent(
    #   input$AddFreezerAction,
    #   ({
    #     
    #     #SAVE FREEZER NAMES INVOLVED
    #     new.freezer <- input$AddFreezer
    #     
    #     #SET REQUIREMENTS
    #     req(input$AddFreezer,
    #         !(new.freezer %in% c(sampleDB::CheckTable(database = database, "location")$location_name)))
    # 
    #     #ADD FREEZER NAME
    #     sampleDB::UpdateReferences(reference = "freezer",
    #                                operation = "add",
    #                                information = list(NewFreezerName = new.freezer))
    #     
    #     #PRINT EXIT MESSAGE
    #     output$FreezerReturnMessage <- renderText({paste("Added Freezer", new.freezer, emoji('tada'))})
    # 
    #     #MODIFY TABLE
    #     ShowFreezers(output, database)
    # 
    #     #UPDATE DROPDOWNS
    #     UpdateFreezerDropdowns(database, session)
    #   }))
    
    #CHANGE FREEZER NAMES
    # observeEvent(
    #   input$RenameFreezerAction,
    #   ({
    # 
    #     #SAVE FREEZER NAMES INVOLVED
    #     old.name <- input$RenameFreezer1
    #     new.name <- input$RenameFreezer2
    #     
    #     #MODIFY TABLE IF NEW FREEZER NAME IS UNIQUE
    #     req(input$RenameFreezer1,
    #         input$RenameFreezer2,
    #         !(new.name %in% c(sampleDB::CheckTable(database = database, "location")$location_name)))
    #     
    #     #CHANGE FREEZER NAME
    #     sampleDB::UpdateReferences(reference = "freezer",
    #                                operation = "modify",
    #                                information = list(NewFreezerName = new.name,
    #                                                   OldFreezerName = old.name))
    #     
    #     #PRINT EXIT MESSAGE
    #     output$FreezerReturnMessage <- renderText({paste("Renamed Freezer", old.name, "to", new.name, emoji('tada'))})
    #     
    #     #REFRESH REFERENCES
    #     ShowFreezers(output, database)
    # 
    #     #UPDATE DROPDOWNS
    #     UpdateFreezerDropdowns(database, session)
    #   }))
    
    #REMOVE FREEZER FROM DATABASE
    # observeEvent(
    #   input$DeleteFreezerAction,
    #   ({
    #     
    #     #SAVE FREEZER NAMES INVOLVED
    #     delete.freezer <- input$DeleteFreezer
    #     
    #     #SET REQUIREMENTS
    #     req(input$DeleteFreezer,
    #         !(filter(sampleDB::CheckTable(database = database, "location"), location_name == delete.freezer)$id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id))
    #     
    #     #DELETE FREEZER
    #     sampleDB::UpdateReferences(reference = "freezer",
    #                                operation = "delete",
    #                                information = list(DeleteFreezerName = delete.freezer))
    #     #PRINT EXIT MESSAGE
    #     output$FreezerReturnMessage <- renderText({paste("Deleted Freezer", delete.freezer, emoji('tada'))})
    #     
    #     #REFRESH REFERENCES
    #     ShowFreezers(output, database)
    # 
    #     #UPDATE DROPDOWNS
    #     UpdateFreezerDropdowns(database, session)
    #   })
    # )

    #IDLY PRESENT FREEZERS IN DATATABLE
    # ShowFreezers(output, database)

    ##################
    # SPECIMEN TYPES #
    ##################
    
    # SpecimenTypeChangesChecks(input, database, output)
    
    #ADD A SPECIMEN TYPE TO THE DATABASE
#     observeEvent(
#         input$AddSpecimenTypeAction,
#         ({
#           
#           #SAVE SPECIMEN_TYPE NAMES INVOLVED
#           new.specimen_type <- input$AddSpecimenType
#           
#           #SET REQUIREMENT
#           req(input$AddSpecimenType,
#               !(new.specimen_type %in% c(sampleDB::CheckTable(database = database, "specimen_type")$label)))
#           
#           #ADD SPECIMEN TYPE
#           sampleDB::UpdateReferences(reference = "specimen_type",
#                                      operation = "add",
#                                      information = list(NewSpecimenTypeName = new.specimen_type))
# 
#           #PRINT EXIT MESSAGE
#           output$SpecimenReturnMessage <- renderText({paste("Added Specimen Type", new.specimen_type, emoji('tada'))})
#           
#           #REFRESH REFERENCES
#           ShowSpecimenTypes(output, database)
# 
#           #UPDATE DROPDOWNS
#           UpdateSpecimenTypeDropdowns(database, session)
#         }))
#     
#     #CHANGE A SPECIMEN TPYE
#     observeEvent(
#       input$RenameSpecimenTypeAction,
#       ({
#         
#         #SAVE SPECIMEN_TYPE NAMES INVOLVED
#         old.name <- input$RenameSpecimenType1
#         new.name <- input$RenameSpecimenType2
#               
#         #CHANGE SPECIMEN_TYPE IF IT IS UNIQUE
#         req(input$RenameSpecimenType1,
#             input$RenameSpecimenType2,
#             !(new.name %in% c(sampleDB::CheckTable(database = database, "specimen_type")$label)))
#         
#         #CHANGE SPECIMEN TYPE NAME
#         sampleDB::UpdateReferences(reference = "specimen_type",
#                                    operation = "modify",
#                                    information = list(NewSpecimenTypeName = new.name,
#                                                       OldSpecimenTyleName = old.name))
#         
#         #PRINT EXIT MESSAGE
#         output$SpecimenReturnMessage <- renderText({paste("Renamed Specimen Type", old.name, "to", new.name, emoji('tada'))})
# 
#         #REFRESH REFERENCES
#         ShowSpecimenTypes(output, database)
# 
#         #UPDATE DROPDOWNS
#         UpdateSpecimenTypeDropdowns(database, session)
#       }))
#     
#     #DELETE A SPECIMEN TYPE
#     observeEvent(
#         input$DeleteSpecimenTypeAction,
#         ({
#           
#           #SAVE SPECIMEN_TYPE NAMES INVOLVED
#           delete.specimen_type <- input$DeleteSpecimenType
#           
#           #SET REQUIREMENT
#           req(input$DeleteSpecimenTypeAction,
#               !(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == delete.specimen_type)$id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id))
#           
#           #DELETE SPECIMEN TYPE
#           sampleDB::UpdateReferences(reference = "specimen_type",
#                                      operation = "delete",
#                                      information = list(DeleteSpecimenType = delete.specimen_type))
#           
#           #PRINT EXIT MESSAGE
#           output$SpecimenReturnMessage <- renderText({paste("Deleted Specimen Type", input$DeleteSpecimenType, emoji('tada'))})
# 
#           #REFRESH REFERENCES
#           ShowSpecimenTypes(output, database)
# 
#           #UPDATE DROPDOWNS
#           UpdateSpecimenTypeDropdowns(database, session)
#         }))
# 
#     #IDLY PRESENT SPECIMEN TYPES
#     ShowSpecimenTypes(output, database)
# 
#     ###########
#     # STUDIES #
#     ###########
#     
#     StudyChangesChecks(input, database, output)
#     
#     #NEED TO TEST
#     #ADD A STUDY TO THE DATABASE 
#     observeEvent(
#         input$AddStudyAction,
#         ({
#           
#           # SET REQUIREMENTS
#           AddStudyRequirements(input)
#           
#           #ADD STUDY
#           sampleDB::UpdateReferences(reference = "study",
#                                      operation = "add",
#                                      information = list(NewStudyTitle = input$AddStudyTitle,
#                                                         NewStudyDescription = input$AddStudyDescription,
#                                                         NewStudyShortCode = input$AddStudyShortCode,
#                                                         NewStudyLeadPerson = input$AddStudyLeadPerson,
#                                                         NewStudyLongitudinal = input$AddStudyIsLongitudinal,
#                                                         NewStudyHidden = input$AddStudyIsHidden))
#           
#           #PRINT EXIT MESSAGE
#           output$StudyReturnMessage <- renderText({paste("Added Study to the Database", emoji('tada'))})
# 
#           #REFRESH REFERENCES
#           ShowStudies(output, database)
# 
#           #UPDATE DROPDOWNS
#           UpdateStudyDropdowns(session)
#         })
#     )
#     
#     # CHANGE A STUDY
#     observeEvent(
#         input$RenameStudyAction,
#         ({
#           
#           # MODIFY STUDY
#           sampleDB::UpdateReferences(reference = "study",
#                                      operation = "modify",
#                                      information = list(OldStudyShortCode = input$ChangeStudyShortCode,
#                                                         NewStudyTitle = input$RenameStudyTitle,
#                                                         NewStudyDescription = input$RenameStudyDescription,
#                                                         NewStudyShortCode = input$RenameStudyShortCode,
#                                                         NewStudyLeadPerson = input$RenameStudyLeadPerson,
#                                                         NewStudyLongitudinal = sum(input$RenameStudyIsLongitudinal),
#                                                         NewStudyHidden = sum(input$RenameStudyIsHidden)))
#           #PRINT EXIT MESSAGE
#           output$StudyReturnMessage <- renderText({paste("Modified Study in the Database", emoji('tada'))})
#           
#           #REFRESH REFERENCES
#           ShowStudies(output, database)
#           
#           #UPDATE DROPDOWNS
#           UpdateStudyDropdowns(database, session)
#         }))
# 
#       # DELETE A STUDY FROM THE DATABASE
#       observeEvent(
#           input$DeleteStudyAction,
#           ({
#             #DELETE STUDY
#             sampleDB::UpdateReferences(reference = "study",
#                                        operation = "delete",
#                                        information = list(DeleteStudyShortCode = input$DeleteStudyShortCode))
#             
#             #PRINT EXIT MESSAGE
#             output$StudyReturnMessage <- renderText({paste("Deleted Study from the Database", emoji('tada'))})  
#             
#             #REFRESH REFERENCES
#             ShowStudies(output, database)
#             
#             })
#         )
# 
#       #IDLY SHOW STUDIES
#       ShowStudies(output, database)
# 
}
