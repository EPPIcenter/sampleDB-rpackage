library(yaml)
library(RSQLite)
library(httr2)
library(reactable)
library(shinyTime)
library(lubridate)
library(cronR)

AppPreferencesPanel <- function(session, input, output, database) {

	config <- reactive({
		yaml::read_yaml(Sys.getenv("SDB_CONFIG"))
	})

	observe({
		opts <- config()
		updateTextInput(
			session = session,
			inputId = "PrefTraxerPositionOverride",
			label = opts$traxcer_position$name,
			value = ifelse(
				!is.null(opts$traxcer_position$override),
				opts$traxcer_position$override,
				NA),
			placeholder = opts$traxcer_position$default
		)
	})

	observeEvent(input$PrefSaveButton,
  ({
  	opts <- config()
  	opts$traxcer_position$override <- ifelse(
  		(is.na(input$PrefTraxerPositionOverride) || input$PrefTraxerPositionOverride == ""),
		NA,
  		input$PrefTraxerPositionOverride
  	)

  	yaml::write_yaml(opts, Sys.getenv("SDB_CONFIG"))
  	showNotification("Configuration Saved!", duration = 1, closeButton = FALSE)
  }))

  output$PrefVersionTable <- renderTable({
  	con <- RSQLite::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  	versions <- sort(tbl(con, "version") %>% pull('name'))

  	df <- data.frame(
  		Component = c("Database", "User Configuration"),
    	Version = c(versions[ length(versions) ], config()$version)
    )

  	return (df)
  })

	observeEvent(input$updateConfigBtn, {
	  # Removing the existing backup job if it exists
	  cron_rm(id = "sampledb_backup", ask = FALSE)

	  browser()

	  # Path to the .Renviron file
	  renviron_path <- normalizePath("~/.Renviron")

	  # Read current .Renviron contents
	  if (file.exists(renviron_path)) {
	    env_contents <- readLines(renviron_path)
	  } else {
	    env_contents <- character(0)
	  }

	  # Update or add the SFTP credentials
	  update_or_add <- function(contents, key, value) {
	    key_pattern <- paste0("^", key, "=")
	    key_exists <- grepl(key_pattern, contents)
	    if (any(key_exists)) {
	      contents[key_exists] <- paste0(key, "='", value, "'")
	    } else {
	      contents <- c(contents, paste0(key, "='", value, "'"))
	    }
	    contents
	  }

	  env_contents <- update_or_add(env_contents, "SDB_SFTP_USERNAME", input$sftpUser)
	  env_contents <- update_or_add(env_contents, "SDB_SFTP_PASSWORD", input$sftpPass)

	  # Write updated contents back to .Renviron
	  writeLines(env_contents, renviron_path)

	  # Path to the backup script
	  backup_script_path <- system.file(
	      "extdata", file.path("backup_xfer", "backup_xfer_script.R"), package = "sampleDB"
	  )

	  # Construct the command with arguments
	  script_args <- c(
	    Sys.getenv("SDB_PATH"),
	    input$sftpHost,
	    22, # just use port=22 for now, could expose this as an input in the future
	    input$sftpDir
	  )
	  command <- cron_rscript(backup_script_path, rscript_args = script_args)

	  # Construct the cron schedule based on user input
	  frequency <- switch(input$scheduleType,
	      "Daily" = "daily",
	      "Weekly" = "weekly",
	      "Monthly" = "monthly"
	  )

	  at_time <- format(as.POSIXct(input$scheduleTime), "%H:%M")
	  day_of_month <- if(input$scheduleType == "Monthly") as.character(input$scheduleDayOfMonth) else NULL
	  day_of_week <- if(input$scheduleType == "Weekly") as.character(match(input$scheduleDayOfWeek, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) - 1) else NULL

	  # Conditional addition to cron based on schedule type
	  if (input$scheduleType == "Daily") {
	    cron_add(command = command, 
	             frequency = frequency, 
	             at = at_time,
	             id = "sampledb_backup",
	             description = "Backup job for SampleDB",
	             ask = FALSE)
	  } else if (input$scheduleType == "Weekly") {
	    cron_add(command = command, 
	             frequency = frequency, 
	             at = at_time, 
	             days_of_week = day_of_week,
	             id = "sampledb_backup",
	             description = "Backup job for SampleDB",
	             ask = FALSE)
	  } else if (input$scheduleType == "Monthly") {
	    cron_add(command = command, 
	             frequency = frequency, 
	             at = at_time, 
	             days_of_month = day_of_month,
	             id = "sampledb_backup",
	             description = "Backup job for SampleDB",
	             ask = FALSE)
	  }

	  # Display a notification
	  showNotification("Backup schedule updated!", duration = 2, closeButton = FALSE)
	})



  observe({
	  output$scheduleInputs <- renderUI({
	    switch(input$scheduleType,
	      "Daily" = timeInput("scheduleTime", "Time of Day", value = "00:00", seconds = FALSE),

	      "Weekly" = tagList(
	        selectInput("scheduleDayOfWeek", "Day of Week", 
	                    choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
	        timeInput("scheduleTime", "Time of Day", value = "00:00", seconds = FALSE)
	      ),

	      "Monthly" = tagList(
	        numericInput("scheduleDayOfMonth", "Day of Month", value = 1, min = 1, max = 31),
	        timeInput("scheduleTime", "Time of Day", value = "00:00", seconds = FALSE)
	      )
	    )
	  })
	})
}


