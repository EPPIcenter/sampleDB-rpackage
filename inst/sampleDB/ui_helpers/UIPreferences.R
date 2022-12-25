UIPreferences <- function() {
  fluidPage(
    titlePanel("Preferences"),
    hr(),
    mainPanel(
      width = 4,
      textInput("PrefTraxerPositionOverride", label = NULL),
      br(),
      actionButton("PrefSaveButton", label = "Save Preferences", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      hr(),
      h5("Source code can be found", a("here", href="https://github.com/EPPIcenter/sampleDB-rpackage/")),
      h5("Please report issues", a("here", href="https://github.com/EPPIcenter/sampleDB-rpackage/issues/")),
      hr(),
      h5("Software Component Version Table"),
      br(),
      tableOutput("PrefVersionTable")
    )
  )
}