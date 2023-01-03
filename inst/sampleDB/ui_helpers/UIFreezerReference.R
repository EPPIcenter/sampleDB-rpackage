library(RSQLite)
UIFreezerReference <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  ui <- sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      radioButtons("LocationAction", width = "100%", label = "Location Action", c("Create" = "create", "Modify" = "modify", "Delete" = "delete"), select = "create", inline = TRUE),
      width = 3,
      hr(),
      shinyjs::hidden(selectInput("LocationType", label = "Sample storage type", choices = DBI::dbReadTable(con, "storage_type") %>% pull(name))),
      shinyjs::hidden(textInput("LocationNameText", label = "Name of sample storage location", placeholder = "Enter a new location name")),
      shinyjs::hidden(selectInput("LocationName", label = "Sample storage location", choices = unique(DBI::dbReadTable(con, "location") %>% pull(name)))),
      shinyjs::hidden(selectInput("LocationNameLevelI", label = "Sample storage level I", choices = c())),
      shinyjs::hidden(selectInput("LocationNameLevelII", label = "Sample storage level II", choices = c())),
      hr(),
      actionButton("LocationActionSubmit", label = "Submit")
    ),
    mainPanel(
      width = 9,
      h3("Sample storage"),
      DT::dataTableOutput("LocationTable"),
      hr()
    )
  )

  DBI::dbDisconnect(con)
  return(ui)
}