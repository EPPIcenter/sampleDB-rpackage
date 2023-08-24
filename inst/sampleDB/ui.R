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

# Main Shiny App UI
ui <- navset_bar(
  title = "EPPIcenter SampleDB",
  header = UICSS(),
  id = "navBar", # add an ID for better JS/CSS customization
  tabPanel("Upload Samples", UIUploadSamples()),
  tabPanel("Search, Delete & Archive Samples", UISearchDelArchSamples()),
  tabPanel("Move Samples",  UIMoveSamples()),
  tabPanel("Move, Rename & Delete Containers", UIMoveContainerOfSamples()),
  navbarMenu("Update References",
             tabPanel("Freezers", UIFreezerReference()),
             tabPanel("Specimen Types", UISpecimenTypeReference()),
             tabPanel("Studies", UIStudiesReference()),
             tabPanel("Controls", UIControlsReference())
  ),
  tabPanel("Preferences", UIPreferences())
)

