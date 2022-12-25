library(yaml)
library(RSQLite)

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
}