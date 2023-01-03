library(DBI)
library(glue)
library(shinyjs)
library(shinyWidgets)

UpdateLabFreezers <- function(session, input, output, database) {

  rv <- reactiveValues(
    data = NULL, 
    selected = NULL,
    update = TRUE,
    error = FALSE
  )

  error <- reactiveValues(
    title = "",
    message = ""
  )

  observeEvent(rv$error, ignoreInit = TRUE, {
    showModal(
      modalDialog(
        title = error$title,
        error$message,
        footer = actionButton("exit", "Ok", class = "btn-primary")
      )
    )
  })

  observeEvent(input$exit, ignoreInit = TRUE, {
    removeModal()
  })

  observeEvent(input$LocationAction, {
    print(input$LocationAction)
    if(input$LocationAction == "create") {
      shinyjs::show("LocationNameText")
      shinyjs::hide("LocationName")
      shinyjs::show("LocationType")
      shinyjs::show("LocationNameLevelI")
      shinyjs::show("LocationNameLevelII")   
    } else if (input$LocationAction == "modify") {
      shinyjs::show("LocationNameText")
      shinyjs::show("LocationName")
      shinyjs::hide("LocationType")
      shinyjs::show("LocationNameLevelI")
      shinyjs::show("LocationNameLevelII") 
    } else if (input$LocationAction == "delete") {
      shinyjs::hide("LocationNameText")
      shinyjs::show("LocationName")
      shinyjs::hide("LocationType")
      shinyjs::show("LocationNameLevelI")
      shinyjs::show("LocationNameLevelII")
    }
  })

  observeEvent(rv$update, {
    con <- DBI::dbConnect(SQLite(), database)
    rv$data <- DBI::dbReadTable(con, "location")
    rv$update <- NULL

    updateSelectInput(session, "LocationName", label = "Sample storage location", unique(DBI::dbReadTable(con, "location") %>% pull(name)))

    DBI::dbDisconnect(con)
  })


  observeEvent(input$LocationType, {

    print(input$LocationType)

    shinyjs::reset("LocationName")
    shinyjs::reset("LocationNameLevelI")
    shinyjs::reset("LocationNameLevelII")

    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session, 
      "LocationName",
      selected = "",
      choices = (tbl(con, "location") %>%
        filter(storage_type == local(input$LocationType)) %>%
        collect() %>% 
        pull(id, name = "name")
      )
    )

    updateSelectInput(
      session,
      "UploadLocationLevelI",
      label = switch(input$LocationType,
        "Micronix" = "Shelf Name", 
        "Cryovial" = "Rack Number"
      ),
      selected = "",
      choices = (tbl(con, "location") %>%
        filter(storage_type == local(input$LocationType) & name == local(input$LocationName)) %>%
        collect() %>% 
        pull(name)
      )      
    )

    updateSelectInput(
      session,
      "UploadLocationLevelII",
      label = switch(input$LocationType,
        "Micronix" = "Basket Name",
        "Cryovial" = "Rack Position"
      ),
      selected = "",
      choices = (tbl(con, "location") %>%
        filter(storage_type == local(input$LocationType) & name == local(input$LocationName) & level_I == local(input$LocationNameLevelI)) %>%
        collect() %>% 
        pull(name)
      )
    )

    DBI::dbDisconnect(con)
  })

  observeEvent(input$LocationActionSubmit, {

    now = as.character(lubridate::now())

    con <- DBI::dbConnect(SQLite(), database)
    if (input$LocationAction %in% c("create", "modify")) {
      df <- tbl(con, "location") %>% filter(name == local(input$LocationNameText)) %>% collect()

      if (nrow(df) > 0) {
        error$title <- "Invalid Profile"
        error$message <- "Duplicate name."
        rv$error <- TRUE
        return()
      }

      if (input$LocationAction == "create") {
        df.payload <- data.frame(
          created = now,
          last_updated = now,
          name = input$LocationNameText,
          storage_type_id = input$LocationType
        )

        DBI::dbAppendTable(con, "location", df.payload)

      } else {

        location_id <- tbl(con, "location") %>% 
          filter(name == local(input$LocationName) & level_I == local(input$LocationNameLevelI) & level_II == local(input$LocationNameLevelII)) %>%
          collect() %>% pull(id)

        browser()
        sql <- SQL("UPDATE location SET name=?name, level_I=?level_I, level_II=?level_II WHERE id=?id")
        sqlInterpolate(con, sql,
          name = dbQuoteIdentifier(con, input$LocationName),
          level_I = dbQuoteIdentifier(con, input$LocationNameLevelI),
          level_II = dbQuoteIdentifier(con, input$LocationNameLevelII),
          id = dbQuoteIdentifier(con, location_id)
        )

        # dbExecute(con, glue::glue_sql(
        #   "UPDATE location SET name='", input$LocationNameText, "', storage_type='", input$LocationType, "' WHERE (id = '", location_id, "');"))
      }
    } else {
      df <- tbl(con, "location") %>% filter(id == local(input$LocationName)) %>% collect()
      if (nrow(df) == 0) {
        error$title <- "Invalid Profile"
        error$message <- "Location does not exist in the database."
        rv$error <- TRUE
        return()
      }

      dbExecute(con, glue::glue_sql("DELETE FROM location WHERE (id = '", input$LocationName, "');"))
    }

    rv$update <- TRUE 
    DBI::dbDisconnect(con)

  })

  output$LocationTable <- DT::renderDataTable({
    rv$data
  })

  rv$selected <- reactive({ rv$data[ input$DelArchSearchResultsTable_rows_selected, ] })
}