library(dplyr)
library(sampleDB)
library(shinyFeedback)
library(shiny)
library(markdown)
library(DT)
library(shinyWidgets)
library(bslib)

#load helper files
for(ui_helper in list.files(path = "ui_helpers", full.names = T, recursive = T)){
  source(ui_helper, local = TRUE)
}

#SET PATH TO SQLITE DATABASE
database <- Sys.getenv("SDB_PATH")

navbarPage("EPPIcenter SampleDB",
           
  #css setup
  header = UICSS(),

  theme = bs_theme(version = 4, bootswatch = "flatly"),
  #upload 
  tabPanel("Upload Samples", UIUploadSamples()),
  #search
  tabPanel("Search Samples", UISearchSamples()),
  #move
  tabPanel("Move Samples",  UIMoveSamples()),
  #delarch
  tabPanel("Delete & Archive Samples", UIDelArchSamples()),
  #edit containers
  tabPanel("Move, Rename & Delete Containers", UIMoveContainerOfSamples()),
  #referrences
  navbarMenu("Update References",
            tabPanel("Freezers", UIFreezerReference()),
            tabPanel("Specimen Types", UISpecimenTypeReference()),
            tabPanel("Studies", UIStudiesReference())),
  #about
  tabPanel("Preferences", UIPreferences())
)
