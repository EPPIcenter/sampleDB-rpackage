library(RSQLite)
library(dplyr)

EditWetlabContainers <- function(session, input, database, output){

  rv <- reactiveValues(user_action_required = FALSE, error = NULL)

  error <- reactiveValues(
    title = "",
    message = ""
  )

  observeEvent(rv$error, ignoreInit = TRUE, {
    message("Running error workflow")


    showModal(
      modalDialog(
        title = error$title,
        error$message,
        footer = modalButton("Exit")
      )
    )
    rv$error <- NULL
  })

  observeEvent(input$Exit, ignoreInit = TRUE, {
    error$title = ""
    error$message = ""
    rv$error <- NULL
    removeModal()
  })

  observeEvent(input$ContainerSampleType, {

    shinyjs::reset("ContainerLocationRoot")
    shinyjs::reset("ContainerLocationLevelI")
    shinyjs::reset("ContainerLocationLevelII")

    manifest <- switch(
      input$ContainerSampleType,
      "1" = "micronix_plate",
      "2" = "cryovial_box",
      "3" = "dbs_paper"
    )

    database <- Sys.getenv("SDB_PATH")
    con <-  RSQLite::dbConnect(RSQLite::SQLite(), database)

    updateSelectInput(
      session,
      "ContainerManifestID",
      label = switch(
        input$ContainerSampleType,
        "1" = "Plate Name",
        "2" = "Box Name",
        "3" = "Paper Name"
      ),
      choices = c("", DBI::dbReadTable(con, manifest) %>% pull(name)),
      selected = character(0)
    )

    updateSelectInput(
      session, 
      "ContainerLocationRoot",
      choices = c("", tbl(con, "location") %>%
        collect() %>% 
        pull(name) %>%
        unique(.)
      ),
      selected = character(0)
    )

    updateSelectInput(
      session,
      "ContainerLocationLevelI",
      label = switch(
        input$ContainerSampleType,
        "1" = "Shelf Name", 
        "2" = "Rack Number",
        "3" = "Shelf Name"
      )
    )

    updateSelectInput(
      session,
      "ContainerLocationLevelII",
      label = switch(
        input$ContainerSampleType,
        "1" = "Basket Name",
        "2" = "Rack Position",
        "3" = "Shelf Position"
      )
    )

    DBI::dbDisconnect(con)
  })

  observeEvent(input$ContainerLocationRoot, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "ContainerLocationLevelI",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$ContainerLocationRoot)) %>%
        collect() %>% 
        pull(level_I)
      )
    )
    DBI::dbDisconnect(con)

    shinyjs::reset("ContainerLocationLevelI")
    shinyjs::reset("ContainerLocationLevelII")
  })

  observeEvent(input$ContainerLocationLevelI, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "ContainerLocationLevelII",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$ContainerLocationRoot) && level_I == local(input$ContainerLocationLevelI)) %>%
        collect() %>% 
        pull(level_II)
      )
    )
    DBI::dbDisconnect(con)
  })

  observeEvent(input$ContainerAction, ignoreInit = FALSE, {

    if (input$ContainerAction == "move") {
      shinyjs::show("ContainerLocationRoot")
      shinyjs::show("ContainerLocationLevelI")
      shinyjs::show("ContainerLocationLevelII")

      shinyjs::hide("ContainerManifestNewID")
      shinyjs::hide("ContainerManifestIDCheck")

      output$ContainerAction <- renderUI({
        actionButton("MoveContainerAction", label = "Move Container")
      })

    }
    else if (input$ContainerAction == "rename") {
      shinyjs::hide("ContainerLocationRoot")
      shinyjs::hide("ContainerLocationLevelI")
      shinyjs::hide("ContainerLocationLevelII")
      
      shinyjs::show("ContainerManifestNewID")
      shinyjs::show("ContainerManifestIDCheck")

      output$ContainerAction <- renderUI({
        actionButton("RenameContainerAction", label = "Rename Container")
      })

    } else {
      shinyjs::hide("ContainerLocationRoot")
      shinyjs::hide("ContainerLocationLevelI")
      shinyjs::hide("ContainerLocationLevelII")   

      shinyjs::hide("ContainerManifestNewID")
      shinyjs::hide("ContainerManifestIDCheck")

      output$ContainerAction <- renderUI({
        actionButton("DeleteContainerAction", label = "Delete Container")
      })
    } 
  })

  # Input validation occurs here
  observe({
    if (input$ContainerAction == "move") {
      rv$user_action_required <- any(c(input$ContainerManifestID, input$ContainerLocationRoot, input$ContainerLocationLevelI, input$ContainerLocationLevelII) == "")
    } else if (input$ContainerAction == "rename") {
      output$ContainerManifestIDCheck <- renderUI({

        html <- paste0("<span></span>")

        if (dbCanConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))) {

          con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

          if (input$ContainerManifestNewID != "") {
            manifest <- switch(
              input$ContainerSampleType,
              "1" = "micronix_plate",
              "2" = "cryovial_box",
              "3" = "dbs_paper"
            )

            result <- tbl(con, manifest) %>%
              filter(name %in% local(input$ContainerManifestNewID)) %>%
              count() %>%
              pull(n)

            if (result > 0) {
              html <- paste0("<span style=color:#0000ff>", "Name is in use!", "</span>")
              rv$user_action_required <- TRUE
            } else {
              rv$user_action_required <- input$ContainerManifestID == ""
            }
          }

          dbDisconnect(con)
        }

        HTML(html)
      })  
    } else {
      rv$user_action_required <- input$ContainerManifestID == ""
    }
  })

  observeEvent(input$MoveContainerAction, ignoreInit = FALSE, {

      if (rv$user_action_required) {
        error$title <- "User action required"
        error$message <- "There are invalid inputs that need to be addressed."
        rv$error <- TRUE
        return()
      }

      database <- Sys.getenv("SDB_PATH")
      conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
      RSQLite::dbBegin(conn)

      tryCatch(
        expr = {

          #move container
          return_message <- sampleDB::MoveContainers(
            sample_type = input$ContainerSampleType,
            container_name = input$ContainerManifestID,
            freezer = list(
              freezer.name = input$ContainerLocationRoot,
              freezer.levelI = input$ContainerLocationLevelI,
              freezer.levelII = input$ContainerLocationLevelII
            ),
            conn = conn)

          output$ContainerOutputConsole <- renderText(return_message)
        },
        warning = function(w) {
          output$ContainerOutputConsole <- renderText({ paste("ERROR:", w$message) })
          message(w)
        },
        error = function(e) {
          output$ContainerOutputConsole <- renderText({ paste("ABORT:", e$message) })
          message(e)
        },
        finally = {
          RSQLite::dbCommit(conn)
          RSQLite::dbDisconnect(conn)
        }
      )
    })
  
  observeEvent(input$RenameContainerAction, ignoreInit = FALSE, {

    if (rv$user_action_required) {
      error$title <- "User action required"
      error$message <- "There are invalid inputs that need to be addressed."
      rv$error <- TRUE
      return()
    }

    database <- Sys.getenv("SDB_PATH")
    conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)
    
    tryCatch(
      expr = {

        #rename container
        return_message <- sampleDB::RenameContainers(sample_type = input$ContainerSampleType, 
                                                     new_container_name = input$ContainerManifestNewID, 
                                                     current_container_name = input$ContainerManifestID,
                                                     conn = conn)

        output$ContainerOutputConsole <- renderText(return_message)
      },
      warning = function(w) {
        output$ContainerOutputConsole <- renderText({ paste("ERROR:", w$message) })
        message(w)
      },
      error = function(e) {
        output$ContainerOutputConsole <- renderText({ paste("ABORT:", e$message) })
        message(e)
      },
      finally = {
        RSQLite::dbCommit(conn)
        RSQLite::dbDisconnect(conn)
      }
    )
  })
  
  observeEvent(input$DeleteContainerAction, ignoreInit = FALSE, {

    if (rv$user_action_required) {
      error$title <- "User action required"
      error$message <- "There are invalid inputs that need to be addressed."
      rv$error <- TRUE
      return()
    }
    
    database <- Sys.getenv("SDB_PATH")
    conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)
    
    # delete plate
    tryCatch(
      expr = {
        return_message <- sampleDB::DeleteEmptyContainer(type = input$ContainerSampleType, container_name = input$ContainerManifestID, conn = conn)
        output$ContainerOutputConsole <- renderText({return_message})
      },
      warning = function(w) {
        output$ContainerOutputConsole <- renderText({ paste("ERROR:", w$message) })
        message(w)
      },
      error = function(e) {
        output$ContainerOutputConsole <- renderText({ paste("ABORT:", e$message) })
        message(e)
      },
      finally = {
        RSQLite::dbCommit(conn)
        RSQLite::dbDisconnect(conn)
      }
    )
  })
}
