ControlReference <- function(session, input, output, database) {
	rv <- reactiveValues(
		error = NULL,
		user_file = NULL,
		user_file_error_annotated = NULL,
		table = NULL,
    filters = NULL,
    dbmap = c(
      batch_creation_date = "Created", 
      strain = "Strain", 
      percentage = "Percentage", 
      density = "Density", 
      count = "Count", 
      bag_name = "BagName",
      batch = "Batch", 
      location_root = "Freezer", 
      level_I = "level_I",
      level_II = "level_II"
    ),
    update.table = c(
      "strain",
      "percentage",
      "density",
      "bag_name"
    ),
    user_selected_rows = NULL,
    last_selected_row = NULL,
    rt = NULL
	)

  error <- reactiveValues(
    title = "",
    type = "",
    message = "",
    list = NULL
  )

  observeEvent(rv$error, ignoreInit = TRUE, {
    message("Running error workflow")
    df <- error$list
    modal_size <- "m"
    if (!is.null(error$type) && error$type == "formatting") {
      df <- error$list %>%
        dplyr::rename(
          Column = column, 
          Reason = reason,
          `Triggered By` = trigger
        ) %>%
        reactable(.)

      showModal(
        modalDialog(
          size = "m",
          title = error$title,
          error$message,
          tags$hr(),
          renderReactable({ df }),
          footer = modalButton("Exit")
        )
      )
    } else if (!is.null(error$type) && error$type == "validation") {
      errors <- unique(names(error$list))
      errors <- data.frame(errors)
      colnames(errors) <- "Error"
      df <- reactable(errors, details = function(index) {
        data <- error$list[[index]]$Columns
        htmltools::div(style = "padding: 1rem",
          reactable(
            data, 
            outlined = TRUE, 
            striped = TRUE,
            # rownames = TRUE,
            theme = reactableTheme(
            headerStyle = list(
              "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
              "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
              borderColor = "#555"
            )),
            defaultColDef = colDef(na = "-", align = "center")
          )
        )
      })

      showModal(
        modalDialog(
          size = "l",
          title = error$title,
          tags$p("One or more rows had invalid or missing data. See the errors below and expand them to see which rows caused this error."),
          tags$p("Press the button below to download your file with annotations"),
          downloadButton("ErrorFileDownload"),
          tags$hr(),
          renderReactable({ df }),
          footer = modalButton("Exit")
        )
      )
  	}

    rv$error <- NULL
    error$title <- ""
    error$message <- ""
    error$type <- ""
    error$list <- NULL
	})

  observeEvent(input$InputCreateStrain, ignoreInit = TRUE, {

  	con <- NULL
 		tryCatch({

 			con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

      res=0
 			if (!is.null(input$InputControlNewStrain) && input$InputControlNewStrain != "") {
 				df.payload = data.frame(name = input$InputControlNewStrain)
 				res <- dbAppendTable(con, "strain", df.payload)
 			} 

 			msg<-sprintf("%d strain added...", res)
	    showNotification(msg, id = "ControlCreateStrain", type = "message", action = NULL, duration = 3, closeButton = FALSE)

 			updateSelectizeInput(
 				session,
 				"InputControlStrain",
 				selected = FALSE,
 				choices = tbl(con, "strain") %>% pull(id, name="name"),
        server = TRUE
 			)
    },
    error = function(e) {
      message(e)
      error$title = "Unknown Error"
      error$type = "unknown"
      error$message = e$message
      error$list = NULL
      rv$error <- TRUE
    },
    finally = {
      dbDisconnect(con)
    })
  })

  observe({
    rv$filters <- list(
      batch = input$InputControlSearchBatch,
      strain = input$InputControlSearchStrain,
      density = input$InputControlSearchDensity,
      percentage = input$InputControlSearchPercentage,
      dates = list(
        date.from = input$InputControlSearchDateRange[1],
        date.to = input$InputControlSearchDateRange[2]
      ), 
      location = list(
        location_root = input$InputControlSearchByLocation,
        level_I = input$InputControlSearchByLevelI,
        level_II = input$InputControlSearchByLevelII
      )
    )

    observe({
      output$DownloadControlData <- downloadHandler(
        filename = function() {
          paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          write.csv(rv$table, con, row.names = FALSE, quote = FALSE)
        }
      )
    })

    output$ControlTableOutput <- renderReactable({ 

      rt = NULL
      if (!is.null(rv$table)) {

        df = rv$table
        df = select(df, names(rv$dbmap))
        colnames(df) = unname(rv$dbmap)

        rt = reactable(
          df,
          defaultColDef = colDef(minWidth = 95, html = TRUE, sortable = TRUE, resizable = FALSE, na = "-", align = "center"),
          searchable = TRUE,
          selection = "multiple", onClick = "select",
          columns = list(
            .selection = colDef(
              headerStyle = list(pointerEvents = "none")
            )
          ),
          striped = TRUE,
          showPageSizeOptions = TRUE,
          theme = reactableTheme(
            headerStyle = list(
              "& input[type='checkbox']" = list(display = "none"),
              "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
              "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
              borderColor = "#555"
            )
          )
        )
        return (rt)
      }
    })
  })

  ###### Delarch specific functionality

  selected <- reactive(getReactableState("ControlTableOutput", "selected"))
  selected.updating.counts <- reactive(getReactableState("InputControlsSelectedRows", "selected"))

  observeEvent(input$InputControlArchiveAction, ignoreInit = TRUE, {

    con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    user.filtered.rows = rv$table
    user.selected.rows = user.filtered.rows[selected(), ]

    rt.select = rv$update.table[rv$update.table %in% colnames(user.selected.rows)]
    user.selected.rows.select = user.selected.rows %>% select(all_of(rt.select))
    colnames(user.selected.rows.select) <- unname(rv$dbmap[names(rv$dbmap) %in% rv$update.table])

    rv$user_selected_rows = user.selected.rows.select

    rt <- reactable(
      user.selected.rows.select,
      defaultColDef = colDef(
        minWidth = 95,
        html = TRUE,
        sortable = TRUE,
        resizable = FALSE,
        na = "-", 
        align = "center"
      ),
      selection = "single",
      onClick = "select",
      theme = reactableTheme(
        headerStyle = list(
          "& input[type='checkbox']" = list(display = "none"),
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "#555"
        ),
        rowSelectedStyle = list(backgroundColor = '#aafaff', boxShadow = 'inset 2px 0 0 0 #ffa62d')
      )
    )

    showModal(
      modalDialog(
        title = "Update Controls",
        size = "l",
        tags$em("Please review the following fields and your selected samples below.", style = "color: grey;font-size: 18px;"),
        hr(),
        tags$p("Please review your selected samples below before submitting. You may cancel by selecting", tags$em("Dismiss"), "below or by clicking outside of the dialog box."),
        renderReactable({ rt }),
        hr(),
        fluidRow( column(width = 6, numericInput(label = tags$strong("Number of DBS Controls Punched"), inputId = "ControlInputNumControls", value = 0, width = '75%')),
                  column(width = 6, tags$p("Please indicate the number of controls that were punched."))
        ),
        easyClose = TRUE,
        fade = TRUE,
        footer = tagList(actionButton("ArchiveControlAction", label = "Update"), modalButton("Dismiss"))
      )
    )

    DBI::dbDisconnect(con)
  })

  observeEvent(selected.updating.counts(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if (!is.null(rv$user_selected_rows)) { 

      ## if the row has changed 
      if (is.null(rv$last_selected_row) || (!is.null(rv$last_selected_row) && rv$last_selected_row != selected.updating.counts())) {
        updateNumericInput(
          session,
          "ControlInputNumControls",
          value = 0
        )

        rv$last_selected_row = selected.updating.counts()
      }
    }
  })

  observeEvent(input$ControlInputNumControls, ignoreInit = TRUE, {

  })

  observeEvent(input$InputUploadStrainAction, ignoreInit = TRUE, {

    dataset <- input$InputUploadStrains
    if (is.null(dataset) || is.null(dataset$datapath)) {
      message("Aborting upload - no file uploaded")
      return()
    }

    b_use_wait_dialog <- FALSE
    early_stop <- FALSE
    if (is.null(rv$user_file)) {

      tryCatch({

        ## format the file
        rv$user_file <- ProcessCSV(
          user_csv = dataset$datapath,
          user_action = "upload",
          file_type = "na",
          reference = "strain"
        )
      },
      formatting_error = function(e) {
        message("Caught formatting error")
        print(e$df)

        error$type <- "formatting"

        ## Read File Specification File
        error$title = "Invalid File Detected"
        error$message = e$message
        error$list = e$df

        rv$error <- TRUE
        early_stop <<- TRUE
      },
      validation_error = function(e) {

        message("Caught validation error")
        
        rv$error <- TRUE
        error$type <- "validation"
        error$title <- e$message
        error$list <- e$data
        early_stop <<- TRUE

        # TODO: breakup process csv into three stages(but keep calls in global process csv).
        # Just download the error data frame for now.
        errors <- names(e$data)
        df <- lapply(1:length(errors), function(idx) {
          e$data[[idx]]$CSV %>%
            mutate(Error = errors[idx]) %>%
            mutate(ErrCol = paste(e$data[[idx]]$Columns, collapse = ",")) %>%
            select(Error, colnames(e$data[[idx]]$CSV)) 
        })

        rv$user_file_error_annotated <- do.call("rbind", df) 
      },
      error = function(e) {
        print(e)

        early_stop <<- TRUE

        error$title = "Unknown Error"
        error$type = "unknown"
        error$message = e$message
        error$list = NULL
        rv$error = TRUE
      })
    }

    if (early_stop) return()

    message("Starting Upload...")

    tryCatch({

      # simple way to add a dialog or not
      b_use_wait_dialog <- nrow(rv$user_file) > 5

      if (b_use_wait_dialog) {
        show_modal_spinner(
          spin = "double-bounce",
          color = "#00bfff",
          text = paste("Uploading", nrow(rv$user_file), "strains, please be patient...")
        )
      }

      shinyjs::reset("InputUploadStrains")

      ## Upload strains

      con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
      dbBegin(con)
      res <- dbAppendTable(con, "strain", rv$user_file %>% select(strain) %>% dplyr::rename(name = strain))
      dbCommit(con)

      updateSelectizeInput(
        session,
        "InputControlNewStrain",
        selected = input$InputControlNewStrain,
        choices = tbl(con, "strain") %>% pull(id, name="name"),
        server = TRUE
      )
    },
    error = function(e) {
      message(e)
      error$title = "Unknown Error"
      error$type = "unknown"
      error$message = e$message
      error$list = NULL
      rv$error <- TRUE
    },
    finally = {
      if (b_use_wait_dialog)
        remove_modal_spinner()

      rv$user_file <- NULL
      if (!is.null(con)) {
        dbDisconnect(con)
      }
    })
  })

  observeEvent(input$InputControlStudyAction, ignoreInit=TRUE, {
    con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    dbBegin(con)
    now=as.character(lubridate::now())

    df.payload=data.frame(
      created=now,
      last_updated=now,
      title=input$InputControlNewStudy,
      short_code=input$InputControlNewStudy,
      description=input$InputControlStudyDesc,
      lead_person=input$InputControlBatchPerson,
      is_longitudinal=0
    )

    dbAppendTable(con, "study", df.payload)

    study_id=dbReadTable(con, "study") %>%
      filter(short_code==input$InputControlNewStudy) %>%
      pull(id)

    df.payload=data.frame(
      url=input$InputControlUrl,
      study_id=study_id
    )

    dbAppendTable(con, "control_collection", df.payload)

    dbCommit(con)
    dbDisconnect(con)
  })

  observeEvent(input$InputControlNewStrain, {

  	con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  	if (!is.null(input$InputControlNewStrain) && input$InputControlNewStrain != "") {
  		df = tbl(con, "strain") %>% 
  			filter(name %in% local(input$InputControlNewStrain)) %>%
        collect()

  		if (nrow(df) == 0) {
  			shinyjs::enable("InputCreateStrain")
  		} else {
  			shinyjs::disable("InputCreateStrain")
  		}
  	} else {
  		shinyjs::disable("InputCreateStrain")
  	}

    dbDisconnect(con)
  })

  observe({

    filters <- purrr::discard(rv$filters[!names(rv$filters) %in% c("location", "collection_date")], function(x) is.null(x) | "" %in% x | length(x) == 0)
    filters$location <- purrr::discard(rv$filters$location, function(x) is.null(x) | "" %in% x | length(x) == 0)
    filters$location <- if (length(filters$location) > 0) filters$location

    filters$dates <- purrr::discard(rv$filters$dates, function(x) is.null(x) | "" %in% x | length(x) == 0)
    filters$dates <- if (length(filters$dates) > 0) filters$dates

    rv$table <- SearchControls(control_type = 1, filters = filters, include_internal_sample_id = TRUE)
  })
}

