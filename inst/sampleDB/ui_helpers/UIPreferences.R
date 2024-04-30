UIPreferences <- function() {
  fluidPage(
    titlePanel("Preferences and Backup Service Control Panel"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        h4("Preferences"),
        textInput("PrefTraxerPositionOverride", label = "Traxer Position Override"),
        actionButton("PrefSaveButton", "Save Preferences"),
        
        h4("Backup Service Control"),
        hr(),
        p("The backup service will create a backup of the sampleDB database and transfer it to a remote SFTP server."),
        textInput("sftpHost", "Host"),
        textInput("sftpUser", "Username"),
        passwordInput("sftpPass", "Password"),
        textInput("sftpDir", "Destination Directory"),

        # Schedule Inputs
        h4("Backup Schedule"),
        radioButtons("scheduleType", "Schedule Type",
                     choices = c("Daily", "Weekly", "Monthly"),
                     inline = TRUE,
                     selected = "Weekly"),
        uiOutput("scheduleInputs"),
        actionButton("updateConfigBtn", "Update Configuration")      
      ),
      mainPanel(
        h5("Software Component Version Table"),
        tableOutput("PrefVersionTable"),
        hr(),        
        h5("Source code can be found", a("here", href="https://github.com/EPPIcenter/sampleDB-rpackage/")),
        h5("Please report issues", a("here", href="https://github.com/EPPIcenter/sampleDB-rpackage/issues/"))
      )
    )
  )
}
