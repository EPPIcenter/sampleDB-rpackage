library(yaml)

AppPreferencesPanel <- function(session, input, output, database) {

	config <- reactive({
		print("reading..")
		yaml::read_yaml(Sys.getenv("SDB_CONFIG"))
	})

	observe({
		opts <- config()
		print(opts)
		# print(input$PrefTraxerPositionOverride)
		updateTextInput(
			session = session,
			inputId = "PrefTraxerPositionOverride",
			label = opts$traxcer_position$name,
			value = ifelse(
				!is.null(opts$traxcer_position$override),
				opts$traxcer_position$override,
				NULL),
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
    }))

}