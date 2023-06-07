ControlReference <- function(session, input, output, database) {
	rv <- reactiveValues(
		error = NULL,
		user_file = NULL,
		user_file_error_annotated = NULL,
		table = NULL,
    filters = NULL
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


	observeEvent(input$InputUploadControls, ignoreInit = TRUE, {
    dataset <- input$InputUploadControls

    message(paste("Loaded", dataset$name))

    tryCatch({

      ## format the file
      rv$user_file <- ProcessCSV(
        user_csv = dataset$datapath,
        user_action = "upload",
        file_type = "na",
        reference = "control_keys"
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
    },
    validation_error = function(e) {

      message("Caught validation error")
      
      rv$error <- TRUE
      error$type <- "validation"
      error$title <- e$message
      error$list <- e$data

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

      error$title = "Unknown Error"
      error$type = "unknown"
      error$message = e$message
      error$list = NULL
      rv$error = TRUE
    })
  })


	observeEvent(input$InputUploadControlAction, ignoreInit = TRUE, {

    dataset <- input$InputUploadControls
    if (is.null(dataset) || is.null(dataset$datapath)) {
      message("Aborting upload - no file uploaded")
      return()
    }

    message("Starting Upload...")

    b_use_wait_dialog <- FALSE
    early_stop <- FALSE
    if (is.null(rv$user_file)) {

      tryCatch({

        ## format the file
        rv$user_file <- ProcessCSV(
          user_csv = dataset$datapath,
          user_action = "upload",
          file_type = "na",
          reference = "control_keys"
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

    tryCatch({

      # simple way to add a dialog or not
      b_use_wait_dialog <- nrow(rv$user_file) > 5

      if (b_use_wait_dialog) {
        show_modal_spinner(
          spin = "double-bounce",
          color = "#00bfff",
          text = paste("Uploading", nrow(rv$user_file), "control keys, please be patient...")
        )
      }

      shinyjs::reset("InputUploadControls")
      
      con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

      dbBegin(con)

      res <- dbAppendTable(con, "control_combination_key", rv$user_file %>% select(control_key) %>% dplyr::rename(name = control_key))

      user_file = rv$user_file %>%
        mutate(
          strain = strsplit(strain, ";"),
          percentage = strsplit(percentage, ";")
        ) %>%
        tidyr::unnest(cols = c("strain","percentage"))

      df.payload = user_file %>%
      	left_join(dbReadTable(con, "strain") %>% dplyr::rename(strain_id = id, strain = name), by = c("strain")) %>%
      	left_join(dbReadTable(con, "control_combination_key") %>% dplyr::rename(control_combination_key_id = id, control_key = name), by = c("control_key")) %>%
      	select(control_combination_key_id, strain_id, percentage)

      res <- dbAppendTable(con, "control_combination", df.payload)

      dbCommit(con)


      updateSelectizeInput(
        session,
        "InputControlKey",
        selected = input$InputControlKey,
        choices = tbl(con, "control_combination_key") %>% pull(id, name="name"),
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
      strain = input$InputControlStrain,
      uid = input$InputControlKey
    )

    output$ControlTableOutput <- renderReactable({ 

      rt = NULL
      if (!is.null(rv$table)) {

        df = rv$table
        colnames(df) = c("UID", "Strain", "Percentage")

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


  observeEvent(rv$filters, {

    con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    df = tbl(con, "control_combination") %>%
      left_join(tbl(con, "strain") %>% dplyr::rename(strain_id = id, strain = name), by = c("strain_id")) %>%
      left_join(tbl(con, "control_combination_key") %>% dplyr::rename(control_combination_key_id = id, uid = name), by = c("control_combination_key_id")) %>%
      select(control_combination_key_id, uid, strain_id, strain, percentage) %>%
      distinct()

    if (!is.null(rv$filters$strain) && rv$filters$strain != "") {
      df = df %>% filter(strain_id %in% local(rv$filters$strain))
    }

    if (!is.null(rv$filters$uid) && rv$filters$uid != "") {
      df = df %>% filter(control_combination_key_id %in% local(rv$filters$uid))
    }

    rv$table = df %>% select(uid, strain, percentage) %>% collect()

    dbDisconnect(con)
  })
}

