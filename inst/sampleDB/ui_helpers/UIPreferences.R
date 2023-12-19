UIPreferences <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML("
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        .spinning-icon {
          animation: spin 2s linear infinite;
        }
      "))
    ),
    titlePanel("Preferences and Backup Service Control Panel"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        h4("Preferences"),
        textInput("PrefTraxerPositionOverride", label = "Traxer Position Override"),
        actionButton("PrefSaveButton", "Save Preferences"),
        hr(),
        h4("Backup Service Settings"),
        textInput("backupServiceUrlInput", "Backup Service URL", value = "https://localhost:8081"),
        actionButton("setBackupServiceUrlBtn", "Set Backup Service URL"),
        actionButton("healthBtn", "Health Check"),
        h4("Backup Service Control"),
        actionButton("startBtn", "Start Backup"),
        actionButton("stopBtn", "Stop Backup"),
        actionButton("restartBtn", "Restart Backup"),
        actionButton("statusBtn", "Get Status"),
        hr(),
        textInput("sftpHost", "SFTP Host"),
        textInput("sftpUser", "SFTP Username"),
        passwordInput("sftpPass", "SFTP Password"),
        textInput("sftpDir", "SFTP Directory"),

        # Schedule Inputs
        h4("Backup Schedule"),
        radioButtons("scheduleType", "Schedule Type",
                     choices = c("Daily", "Weekly", "Monthly"),
                     inline = TRUE),
        uiOutput("scheduleInputs"),
        actionButton("updateConfigBtn", "Update Configuration")
      ),
      mainPanel(
        h5("Software Component Version Table"),
        tableOutput("PrefVersionTable"),
        hr(),
        h5("Backup Service Status"),
        reactableOutput("backupServiceStatus"),
        hr(),

        # Backup Functionality Table with Connection Status Icon
        fluidRow(
          column(width = 12,
            tags$h3(
              "Backup Functionality Details", 
              uiOutput("connectionStatusIcon")
            )
          )
        ),
        reactableOutput("backupFunctionality"),
        h5("Source code can be found", a("here", href="https://github.com/EPPIcenter/sampleDB-rpackage/")),
        h5("Please report issues", a("here", href="https://github.com/EPPIcenter/sampleDB-rpackage/issues/"))
      )
    )
  )
}
