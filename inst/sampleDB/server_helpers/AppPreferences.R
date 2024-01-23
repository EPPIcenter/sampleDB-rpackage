library(yaml)
library(RSQLite)
library(reactable)
library(shinyTime)
library(lubridate)
library(cronR)
library(openssl)
library(rappdirs)
library(base64enc)

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

		# Check if the key exists in the environment, and if not, generate and set it
	  encoded_key <- Sys.getenv("SDB_BACKUP_KEY")

	  if (nchar(encoded_key) == 0) {

			# Path to the .Renviron file
			site_install <- .sampleDB$site_install
		  renviron_path <- suppressWarnings(
	      normalizePath(
	        ifelse(site_install,
	          file.path(Sys.getenv("R_HOME"), "etc", "Renviron.site"),
	          file.path(Sys.getenv("HOME"), ".Renviron")
	        )
	      )
	    )

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

	    # Generate a random binary key
	    raw_key <- openssl::rand_bytes(32)
	    # Base64 encode for safe storage
	    encoded_key <- base64encode(raw_key)
	    env_contents <- update_or_add(env_contents, "SDB_BACKUP_KEY", encoded_key)
	    # Write updated contents back to .Renviron
	  	writeLines(env_contents, renviron_path)
	  	Sys.setenv(SDB_BACKUP_KEY = encoded_key)
	  }

	  # Always decode the key from base64 for use
	  encryption_key <- base64decode(encoded_key)

	  # Define path for encrypted credentials
	  config_dir <- rappdirs::user_config_dir("sampleDB")
	  if (!dir.exists(config_dir)) {
	    dir.create(config_dir, recursive = TRUE)
	  }
	  config_file <- file.path(config_dir, "sftp_credentials.enc")

	  # Encrypt and save credentials
	  creds <- list(username = input$sftpUser, password = input$sftpPass)
	  serialized_creds <- serialize(creds, NULL)
	  encrypted_creds <- openssl::aes_cbc_encrypt(serialized_creds, encryption_key)
	  saveRDS(encrypted_creds, config_file)

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
	  	label <- "Time Input (24-Hour Time)"
	  	value <- hms::as_hms("00:00:00")
	    switch(input$scheduleType,
	      "Daily" = timeInput("scheduleTime", label, value = value, seconds = FALSE),

	      "Weekly" = tagList(
	        selectInput("scheduleDayOfWeek", "Day of Week", 
	                    choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
	        timeInput("scheduleTime", label, value = value, seconds = FALSE)
	      ),

	      "Monthly" = tagList(
	        numericInput("scheduleDayOfMonth", "Day of Month", value = 1, min = 1, max = 31),
	        timeInput("scheduleTime", label, value = value, seconds = FALSE)
	      )
	    )
	  })
	})
}


