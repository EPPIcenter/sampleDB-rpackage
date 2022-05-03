UIMoveContainerOfSamples <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 3,
      #shouldnt need sample type but bc it is unknown at this point if container names are unique, sample type specifies the db table to use to find the container name
      radioButtons("MoveContainerSampleType","Move Container Sample Type", c("Micronix" = "micronix", "Cryovile" = "cryovile", "RDT" = "rdt", "Paper" = "paper"), inline = T),
      selectizeInput("MoveContainerName", label = "Container Name", choices = NULL),
      #should print to user the current location of the container
      selectInput("MoveContainerLocation", label = "Storage Location", choices = c("", sampleDB::CheckTable(database = database, "location")$location_name)),
      fluidRow(column(width = 1), column(width = 11, selectInput("MoveContainerLocationLevelI", label = HTML("<h5>Storage Location: Level I</h5>"), width = '100%', choices = NULL))),
      fluidRow(column(width = 1), column(width = 11, selectInput("MoveContainerLocationLevelII", label = HTML("<h5>Storage Location: Level II</h5>"), width = '100%', choices = NULL))),
      fluidRow(column(width = 12,
                      actionButton("MoveContainerAction", width = '49%', label = "Move Container", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
      br(),
      verbatimTextOutput("MoveContainerMessage"),
    ),
    mainPanel()
  )
}