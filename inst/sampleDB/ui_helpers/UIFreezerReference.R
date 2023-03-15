UIFreezerReference <- function(){

  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  ui <- sidebarLayout(
    sidebarPanel(
      width = 3,
      HTML("<h4><b>Add a Freezer</b></h4>"),
      textInput("AddFreezerName", label = NULL, placeholder = "Give the location a unique name"),
      selectInput("AddFreezerType", label = NULL, choices = DBI::dbReadTable(con, "storage_type") %>% pull(id, name = "name")),
      textInput("AddFreezerDesc", label = NULL, placeholder = "Add a description for this location"),
      textInput("AddFreezerLevel_I", label = NULL, placeholder = "New Level I"),
      textInput("AddFreezerLevel_II", label = NULL, placeholder = "New Level II"),
      actionButton("AddFreezerAction", label = "Add", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      textOutput("WarningFreezerNameAddUnique"),
      hr(),
      HTML("<h4><b>Rename a Freezer</b></h4>"),
      selectInput("RenameFreezerName1", label = NULL, choices = c("", CheckTable(database = database, "location")$name)),
      selectInput("RenameFreezerLevelI1", label = NULL, choices = c("", CheckTable(database = database, "location")$name)),
      selectInput("RenameFreezerLevelII1", label = NULL, choices = c("", CheckTable(database = database, "location")$name)),
      textInput("RenameFreezerName2", label = NULL, placeholder = "New Name"),
      selectInput("RenameFreezerType", label = NULL, choices = DBI::dbReadTable(con, "storage_type") %>% pull(id, name = "name")),
      textInput("RenameFreezerLevelI2", label = NULL, placeholder = "New Level I"),
      textInput("RenameFreezerLevelII2", label = NULL, placeholder = "New Level II"),
      actionButton("RenameFreezerAction", label = "Rename", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      textOutput("WarningFreezerNameChangeUnique"),
      hr(),
      HTML("<h4><b>Remove a Freezer</b></h4>"),
      selectInput("DeleteFreezerName", label = NULL, choices = c("", CheckTable(database = database, "location")$name)),
      selectInput("DeleteFreezerLevelI", label = NULL, choices = NULL),
      selectInput("DeleteFreezerLevelII", label = NULL, choices = NULL),
      textOutput("WarningFreezerDeletion"),
      actionButton("DeleteFreezerAction", label = "Delete", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      HTML("<br></br>"),
      span(verbatimTextOutput("FreezerReturnMessage"), style="font-size: 28px")
    ),
    mainPanel(
      width = 9,
      HTML("<h4><b>Freezers</b></h4>"),
      DT::dataTableOutput("TableFreezer")))

  DBI::dbDisconnect(con)
  return(ui)
}