library(shiny)
library(shinyWidgets)
library(DBI)
library(dplyr)

# Moved to global scope
gray_600 <- "#6C757D"

# Create your theme
my_theme <- bs_theme(
  version = 5,
  gray_600 = gray_600
)

# Custom CSS moved to global scope
custom_css <- paste0(
  ".custom-dropdown, .dropdown-menu .dropdown-item {
      width: 100%;
  }
  .custom-dropdown {
      background-color: ", gray_600, ";
      color: white;
  }
  .custom-dropdown:hover {
      background-color: #5a6268;
      color: white;
  }
  .dropdown-menu {
      padding: 0;
  }"
)

UIControlsReference <- function() {

  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ui <- layout_sidebar(
    
    sidebar = sidebar(
      theme = my_theme,  # Applying your theme
      tags$div(
        class = "dropdown",
        tags$button(
          class = "btn dropdown-toggle custom-dropdown",
          type = "button",
          `data-bs-toggle` = "dropdown",
          "Create New Item"
        ),
        tags$ul(
          class = "dropdown-menu",
          tags$li(actionButton("BatchModalID", "Batch", class = "dropdown-item")),
          tags$li(actionButton("StrainModalID", "Strain", class = "dropdown-item")),
          tags$li(actionButton("CompositionModalID", "Composition", class = "dropdown-item"))
        )
      ),
      selectizeInput("InputControlSearchBatch", width = '100%', label = "Batch", choices = c()),
      selectizeInput("InputControlSearchStrain", width = '100%', label = "Strain", choices = c()),
      selectizeInput("InputControlSearchPercentage", width = '100%', label = "Percentage", choices = c()),
      selectizeInput("InputControlSearchCompositionTypes", width = '100%', label = "Composition", choices = c())
    ),
    card(
      title = "View Strains & Compositions",
      reactableOutput("OutputControlSearchResults"),
      fluidRow(
        column(
          width = 6,
          downloadButton("DownloadControlSearchResults", "Download")
        )
      )
    )
  )
  return(tagList(
    tags$style(HTML(custom_css)),
    ui
  ))
}
