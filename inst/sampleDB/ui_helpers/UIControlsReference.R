library(shiny)
library(shinyWidgets)
library(DBI)
library(dplyr)


UIControlsReference <- function() {

  ui <- layout_sidebar(
    
    sidebar = sidebar(
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
  
  return(ui)
}
