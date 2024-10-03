library(RSQLite)
library(dplyr)
library(shinyjs)

EditWetlabContainers <- function(session, input, database, output, dbUpdateEvent){

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

  observeEvent(dbUpdateEvent(), {
    manifest <- switch(
      input$ContainerType,
      "samples" = switch(
        input$ContainerSampleType,
        "micronix" = "micronix_plate",
        "cryovial" = "cryovial_box",
        "whole_blood" = "whole_blood_tube"
      ),
      "controls" = switch(
        input$ContainerControlType,
        "dbs_sheet" = "dbs_bag",
        "whole_blood" = "whole_blood_tube"
      )
    )

    database <- Sys.getenv("SDB_PATH")
    con <- RSQLite::dbConnect(RSQLite::SQLite(), database)

    updateSelectizeInput(
      session,
      "ContainerManifestID",
      label = switch(
        input$ContainerType,
        "samples" = switch(
          input$ContainerSampleType,
          "micronix" = "Plate Name",
          "cryovial" = "Box Name",
          "whole_blood" = "Box Name"
        ),
        "controls" = switch(
          input$ContainerControlType,
          "dbs_sheet" = "Bag Name",
          "whole_blood" = "Box Name"
        )
      ),
      choices = c("", DBI::dbReadTable(con, manifest) %>% pull(name)),
      selected = input$ContainerManifestID,
      server = TRUE
    )

    updateSelectInput(
      session, 
      "ContainerLocationRoot",
      choices = c("", tbl(con, "location") %>%
        collect() %>%
        pull(location_root) %>%
        unique(.)
      ),
      selected = input$ContainerLocationRoot
    )

    updateSelectInput(
      session,
      "ContainerLocationLevelI",
      label = "Shelf Name",
      selected = input$ContainerLocationLevelI
    )

    updateSelectInput(
      session,
      "ContainerLocationLevelII",
      label = "Basket Name",
      selected = input$ContainerLocationLevelII
    )

    DBI::dbDisconnect(con)
  })

  observeEvent(input$ContainerType, {

    shinyjs::reset("ContainerLocationRoot")
    shinyjs::reset("ContainerLocationLevelI")
    shinyjs::reset("ContainerLocationLevelII")

    manifest <- switch(
      input$ContainerType,
      "samples" = switch(
        input$ContainerSampleType,
        "micronix" = "micronix_plate",
        "cryovial" = "cryovial_box",
        "whole_blood" = "whole_blood_tube"
      ),
      "controls" = switch(
        input$ContainerControlType,
        "dbs_sheet" = "dbs_bag",
        "whole_blood" = "whole_blood_tube"
      )
    )

    database <- Sys.getenv("SDB_PATH")
    con <- RSQLite::dbConnect(RSQLite::SQLite(), database)

    updateSelectizeInput(
      session,
      "ContainerManifestID",
      label = switch(
        input$ContainerType,
        "samples" = switch(
          input$ContainerSampleType,
          "micronix" = "Plate Name",
          "cryovial" = "Box Name",
          "whole_blood" = "Box Name"
        ),
        "controls" = switch(
          input$ContainerControlType,
          "dbs_sheet" = "Bag Name",
          "whole_blood" = "Box Name"
        )
      ),
      choices = c("", DBI::dbReadTable(con, manifest) %>% pull(name)),
      selected = "",
      server = TRUE
    )

    updateSelectInput(
      session,
      "ContainerLocationRoot",
      choices = c("", tbl(con, "location") %>%
        collect() %>%
        pull(location_root) %>%
        unique(.)
      ),
      selected = ""
    )

    updateSelectInput(
      session,
      "ContainerLocationLevelI",
      label = switch(
        input$ContainerSampleType,
        "micronix" = "Shelf Name",
        "cryovial" = "Rack Number"
      )
    )

    updateSelectInput(
      session,
      "ContainerLocationLevelII",
      label = switch(
        input$ContainerSampleType,
        "micronix" = "Basket Name",
        "cryovial" = "Rack Position"
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
        filter(location_root == local(input$ContainerLocationRoot)) %>%
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
        filter(location_root == local(input$ContainerLocationRoot) && level_I == local(input$ContainerLocationLevelI)) %>%
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

    } else if (input$ContainerAction == "rename") {
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
        con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

        if (input$ContainerManifestNewID != "") {
          manifest <- switch(
            input$ContainerType,
            "samples" = switch(
              input$ContainerSampleType,
              "micronix" = "micronix_plate",
              "cryovial" = "cryovial_box",
              "whole_blood" = "whole_blood_tube"
            ),
            "controls" = switch(
              input$ContainerControlType,
              "dbs_sheet" = "dbs_bag",
              "whole_blood" = "whole_blood_tube"
            )
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

        DBI::dbDisconnect(con)
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
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)

    tryCatch({
      return_message <- MoveContainers(
        sample_type = input$ContainerType,
        container_name = input$ContainerManifestID,
        freezer = list(
          freezer.name = input$ContainerLocationRoot,
          freezer.levelI = input$ContainerLocationLevelI,
          freezer.levelII = input$ContainerLocationLevelII
        ),
        conn = conn
      )
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
    })
  })

  observeEvent(input$RenameContainerAction, ignoreInit = FALSE, {
    if (rv$user_action_required) {
      error$title <- "User action required"
      error$message <- "There are invalid inputs that need to be addressed."
      rv$error <- TRUE
      return()
    }

    database <- Sys.getenv("SDB_PATH")
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)

    tryCatch({
      return_message <- RenameContainers(
        sample_type = input$ContainerType,
        new_container_name = input$ContainerManifestNewID,
        current_container_name = input$ContainerManifestID,
        conn = conn
      )
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
    })
  })

  observeEvent(input$DeleteContainerAction, ignoreInit = FALSE, {
    if (rv$user_action_required) {
      error$title <- "User action required"
      error$message <- "There are invalid inputs that need to be addressed."
      rv$error <- TRUE
      return()
    }

    database <- Sys.getenv("SDB_PATH")
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)

    tryCatch({
      return_message <- DeleteEmptyContainer(
        type = input$ContainerType,
        container_name = input$ContainerManifestID,
        conn = conn
      )
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
    })
  })
}
