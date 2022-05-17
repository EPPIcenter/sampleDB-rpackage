UIMoveContainerOfSamples <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 4,
      #shouldnt need sample type but bc it is unknown at this point if container names are unique, sample type specifies the db table to use to find the container name
      HTML("<h4><b>Edit Containers</b></h4>"),
      radioButtons("EditContainerSampleType","Sample Type", c("Micronix" = "micronix", "Cryovile" = "cryovile", "RDT" = "rdt", "Paper" = "paper"), inline = T),
      fluidRow(column(width = 6, HTML("<p><b>Plate Name</b></p>"), HTML("Human Readable Plate Name"), selectizeInput("EditContainerName", label = NULL, choices = NULL))),
      radioButtons("EditContainerType","Select Type of Container Edit", c("Move Container" = "MoveContainer", "Rename Container" = "RenameContainer", "Delete Container" = "DeleteContainer"), inline = T),
      hr(),
      conditionalPanel(condition = "input.EditContainerType == \"MoveContainer\"",
                       #should print to user the current location of the container
                       HTML("<p><b>Freezer Address</b></p>"),
                       HTML("<p>Freezer Name</p>"), selectInput("MoveContainerLocation", label = NULL, width = '47%', choices = c("", sampleDB::CheckTable(database = database, "location")$location_name) %>% sort()),
                       HTML("<p>Shelf Name</p>"), selectInput("MoveContainerLocationLevelI", label = NULL, width = '47%', choices = NULL),
                       HTML("<p>Basket Name</p>"), selectInput("MoveContainerLocationLevelII", label = NULL, width = '47%', choices = NULL),
                       fluidRow(column(width = 12,
                                       actionButton("MoveContainerAction", width = '47%', label = "Move Container", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
      ),
      conditionalPanel(condition = "input.EditContainerType == \"RenameContainer\"",
                       textInput("RenameContainerPlateName", width = '47%', label = "New Container Name"),
                       fluidRow(column(width = 12,
                                       actionButton("RenameContainerAction", width = '47%', label = "Rename Container", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
      conditionalPanel(condition = "input.EditContainerType == \"DeleteContainer\"",
                       fluidRow(column(width = 12,
                                       actionButton("DeleteContainerAction", width = '47%', label = "Delete Container", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
      br(),
      verbatimTextOutput("MoveContainerMessage"),
      verbatimTextOutput("RenameContainerMessage"),
      verbatimTextOutput("DeleteContainerMessage"),
    ),
    mainPanel()
  )
}
