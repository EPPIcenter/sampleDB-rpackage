# Load required libraries
library(dplyr)
library(sampleDB)
library(shiny)
library(DT)
library(bslib)

# Load helper files - make sure these files actually exist and are correct
for(ui_helper in list.files(path = "ui_helpers", full.names = T, recursive = T)){
  source(ui_helper, local = TRUE)
}

# Ensure the environment variable is correctly set
database <- Sys.getenv("SDB_PATH")
if(database == "") {
  stop("SDB_PATH environment variable not set.")
}

#' Get the path to a markdown file
get_markdown_path <- function(filename, package_name = "sampleDB") {
  filepath <- system.file("app/www", filename, package = package_name)
  
  if (file.exists(filepath)) {
    return(filepath)
  } else {
    stop(paste("Markdown file", filename, "not found in package", package_name))
  }
}

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#007BFF",
  secondary = "#6C757D",
  success = "#17A2B8",
  info = "#28A745",
  warning = "#FFC107",
  danger = "#DC3545",
  light = "#F8F9FA",
  dark = "#212529",           # Darker header bar
  primary_light = "#BFD4F2",
  primary_dark = "#0056B3",
  body_color = "#333333",
  body_bg = "#FFFFFF",
  text_color = "#333333",
  link_color = "#007BFF",
  link_hover_color = "#0056B3",
  hr_border_color = "#E9ECEF",
  gray_100 = "#F8F9FA",
  gray_200 = "#E9ECEF",
  gray_300 = "#DEE2E6",
  gray_400 = "#CED4DA",
  gray_500 = "#ADB5BD",
  gray_600 = "#6C757D",
  gray_700 = "#495057",
  gray_800 = "#343A40",
  gray_900 = "#212529",
  input_bg = "#F0F3F5",      # Background color for inputs
  input_border = "#C3C7CA",  # Border color for inputs
  input_text = "#495057",    # Text color for inputs
  active_bg = "#FFA500",     # Orangish highlighting for active elements
  active_text = "#FFFFFF",   # Text color for active elements
  tabset_hover_bg = "#FFA500",   # Muted orange for tabset hovering
  tabset_hover_text = "#212529",  # Text color for tabset hovering
  base_font = c("'Fira Sans', sans-serif"),  
  heading_font = c("'Fira Sans', sans-serif") 
)

# Main Shiny App UI
ui <- page_navbar(
  title = tags$strong("SampleDB"),
  header = UICSS(),
  id = "navBar", # add an ID for better JS/CSS customization
  theme = my_theme,
  nav_panel(title = "Upload New Specimens", UIUploadSamples()),
  nav_panel(title = "Search, Delete & Archive", UISearchDelArchSamples()),
  nav_panel(title = "Move Specimens",  UIMoveSamples()),
  nav_panel(title = "Modify Containers", UIMoveContainerOfSamples()),
  nav_menu(title = "Update References",
             nav_panel(title = "Freezers", UIFreezerReference()),
             nav_panel(title = "Specimen Types", UISpecimenTypeReference()),
             nav_panel(title = "Studies", UIStudiesReference()),
             nav_panel(title = "Controls", UIControlsReference())
  ),
  nav_panel("Preferences", UIPreferences()),
  nav_spacer(),
  nav_item(tags$a("EPPIcenter", href = "https://eppicenter.ucsf.edu/"))
)

