library(yaml)
library(RSQLite)
library(httr2)
library(reactable)
library(shinyTime)
library(lubridate)
library(cronR)
library(RCurl)

# Disable Progress bars
options(httr2_progress = FALSE)

AppPreferencesPanel <- function(session, input, output, database) {

	# Global variable to store the backup service URL
	backup_service_url <- reactiveVal("https://backup-service:8081")
	service_available <- reactiveVal(FALSE)
	last_health_check <- reactiveVal(NULL)

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

  observeEvent(input$healthBtn, {
		service_available(
			performHealthCheck(backup_service_url())
		)
    last_health_check(Sys.time())  # Update last health check time
    if (isTRUE(service_available())) {
			showNotification("Backup Service is available!", duration = 1, closeButton = FALSE)
		} else {
			showNotification("Backup Service is unavailable!", duration = 1, closeButton = FALSE)
		}
  })

  # Enable or disable actions based on service availability
  observe({
    enabled <- service_available()
    shinyjs::enable("startBtn", enabled)
    shinyjs::enable("stopBtn", enabled)
    shinyjs::enable("restartBtn", enabled)
    shinyjs::enable("updateConfigBtn", enabled)
  })

	# Shiny Server logic
	observeEvent(input$startBtn, {
		if (isFALSE(service_available())) return (NULL)
	  startBackup(backup_service_url())
	})

	observeEvent(input$stopBtn, {
		if (isFALSE(service_available())) return (NULL)
	  stopBackup(backup_service_url())
	})

	observeEvent(input$restartBtn, {
		if (isFALSE(service_available())) return (NULL)
	  restartBackup(backup_service_url())
	})

	observeEvent(input$updateConfigBtn, {
	  if (isFALSE(service_available())) return(NULL)

	  browser()
	  
	  cronSchedule <- switch(input$scheduleType,
		  "Daily" = {
		    scheduleTimeStr <- format(as.POSIXct(input$scheduleTime), "%H:%M:%S")
		    scheduleTime <- hms(scheduleTimeStr)
		    paste0(minute(scheduleTime), " ", hour(scheduleTime), " * * *")
		  },
		  "Weekly" = {
		    scheduleTimeStr <- format(as.POSIXct(input$scheduleTime), "%H:%M:%S")
		    scheduleTime <- hms(scheduleTimeStr)
		    dayOfWeekNum <- match(input$scheduleDayOfWeek, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) - 1
		    paste0(minute(scheduleTime), " ", hour(scheduleTime), " * * ", dayOfWeekNum)
		  },
		  "Monthly" = {
		    scheduleTimeStr <- format(as.POSIXct(input$scheduleTime), "%H:%M:%S")
		    scheduleTime <- hms(scheduleTimeStr)
		    paste0(minute(scheduleTime), " ", hour(scheduleTime), " ", input$scheduleDayOfMonth, " * *")
		  }
		)

	  updateConfiguration(
	    backup_service_url(), input$sftpHost, input$sftpUser, input$sftpPass, input$sftpDir, schedule = cronSchedule
	  )
	})

	observeEvent(input$setBackupServiceUrlBtn, {

    backup_service_url(input$backupServiceUrlInput)
    service_available(
    	performHealthCheck(backup_service_url())
    )
    last_health_check(Sys.time())  # Update last health check time
    if (isTRUE(service_available())) {
			showNotification("Backup Service is available!", duration = 1, closeButton = FALSE)
		} else {
			showNotification("Backup Service is unavailable!", duration = 1, closeButton = FALSE)
		}
  })

  observe({
    output$connectionStatusIcon <- renderUI({

      isConnected <- service_available()

      # Determine icon and tooltip text based on connection status
			icon <- if (isConnected) {
			  shiny::icon("check-circle", class = "text-success", style = "font-size: 24px; vertical-align: middle; padding-left: 10px;")
			} else {
			  shiny::icon("times-circle", class = "text-danger", style = "font-size: 24px; vertical-align: middle; padding-left: 10px;")
			}
      tooltipText <- if (isConnected) {
        paste("Connected to", backup_service_url(), "\nLast checked:", format(last_health_check(), "%Y-%m-%d %H:%M:%S"))
      } else {
        "Service Unavailable"
      }

      shiny::tags$span(
        `data-toggle` = "tooltip", `data-placement` = "top", title = tooltipText,
        icon
      )
    })

    # Initialize tooltips
    session$onFlushed(function() {
      shinyjs::runjs("$(function () { $('[data-toggle=\"tooltip\"]').tooltip() })")
    })
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

	observeEvent(service_available(), {
		output$backupFunctionality <- renderReactable({
		  status <- checkBackupServiceStatus(backup_service_url())

		  if (!is.null(status)) {
		    data <- data.frame(
		      Scheduled = ifelse(status$running, "Yes", "No"),
		      NextBackup = "Next scheduled time here",  # You need to extract this from status
		      Message = status$message
		    )
		    reactable(data, columns = list(
		      Scheduled = colDef(name = "Scheduled"),
		      NextBackup = colDef(name = "Next Backup"),
		      Message = colDef(name = "Message")
		    ))
		  } else {
		    reactable(data.frame(
		      Error = "Unavailable",
		      Message = "Status not available"
		    ))
		  }
		})
	})
}

# Function to perform a health check of the backup service
performHealthCheck <- function(url) {
  tryCatch({
    req <- request(paste0(url, "/get-status")) %>%
      req_retry(max_tries = 3)  # Silent retry to avoid console messages

    resp <- req_perform(req)
    return(!resp %>% resp_is_error())
  }, error = function(e) {
    # Handle error silently
    return(FALSE)
  })
}

# Function to get the status of scheduled backups
getScheduledBackupStatus <- function(url) {
  if (!performHealthCheck(url)) {
    return(NULL)  # If health check fails, return NULL
  }

  req <- request(paste0(url, "/get-status"))
  resp <- req_perform(req)

  if (!resp %>% resp_is_error()) {
    return(resp %>% resp_body_json(auto_unbox = TRUE))
  } else {
    return(NULL)
  }
}

# Function to check the status of the backup service
checkBackupServiceStatus <- function(url) {
  tryCatch({
    req <- request(paste0(url, "/get-status")) %>%
      req_retry(max_tries = 3)  # Retry up to 3 times in case of failure

    resp <- req_perform(req)
    if (!resp %>% resp_is_error()) {
      content <- resp %>% resp_body_json(auto_unbox = TRUE)
      return(content)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    # Handle error gracefully
    return(NULL)
  })
}

# Function to start the backup
startBackup <- function(url) {
  httr2::req_perform(
    httr2::request(paste0(url, "/update-action")) %>%
      httr2::req_method("POST") %>%
      httr2::req_body_json(list(action = "start"))
  )
}

# Function to stop the backup
stopBackup <- function(url) {
  httr2::req_perform(
    httr2::request(paste0(url, "/update-action")) %>%
      httr2::req_method("POST") %>%
      httr2::req_body_json(list(action = "stop"))
  )
}

# Function to restart the backup
restartBackup <- function(url) {
  httr2::req_perform(
    httr2::request(paste0(url, "/update-action")) %>%
      httr2::req_method("POST") %>%
      httr2::req_body_json(list(action = "restart"))
  )
}

# Function to update the configuration
updateConfiguration <- function(url, sftpHost, sftpUser, sftpPass, sftpDir, schedule, database = Sys.getenv("SDB_PATH")) {
	browser()
  config <- list(
    database_path = database,
    sftp_host = sftpHost,
    sftp_port = 22,  # Update if you use a different port
    sftp_user = sftpUser,
    sftp_destination = sftpDir,
    schedule = schedule  # Include the schedule parameter
  )

  # Ensure the URL uses HTTPS
  secure_url <- gsub("^http://", "https://", url)

  httr2::req_perform(
    httr2::request(paste0(url, "/update-config")) %>%
      httr2::req_method("POST") %>%
      httr2::req_body_json(config) %>%
      httr2::req_headers(`X-SFTP-Password` = sftpPass)
  )
}


