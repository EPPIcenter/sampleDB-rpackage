UIDelEmptyContainerOfSamples <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 3,
      #shouldnt need sample type but bc it is unknown at this point if container names are unique, sample type specifies the db table to use to find the container name
      radioButtons("DeleteContainerSampleType","Delete Container Sample Type", c("Micronix" = "micronix", "Cryovial" = "cryovial", "RDT" = "rdt", "Paper" = "paper"), inline = T),
      selectInput("DeleteContainerName", label = "Container Name", choices = c("")),
      fluidRow(column(width = 12,
                      actionButton("DeleteContainerAction", width = '49%', label = "Delete Container", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
      span(verbatimTextOutput("DeleteEmptyContainerOfSamples"), style="font-size: 28px"),
    ),
    mainPanel())
}