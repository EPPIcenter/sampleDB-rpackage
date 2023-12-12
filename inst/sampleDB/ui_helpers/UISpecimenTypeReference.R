UISpecimenTypeReference <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 3,
      HTML("<h4><b>Add a Specimen Type</b></h4>"),
      textInput("AddSpecimenType", label = NULL, placeholder = "New Name"),
      actionButton("AddSpecimenTypeAction", label = "Add", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      textOutput("WaringAddSpecimenTypeUnique"),
      hr(),
      HTML("<h4><b>Rename a Specimen Type</b></h4>"),
      selectInput("RenameSpecimenType1", label = "Current Specimen Type Name", choices = NULL, selected = 1),
      textInput("RenameSpecimenType2", label = NULL, placeholder = "New Name"),
      actionButton("RenameSpecimenTypeAction", label = "Rename", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      textOutput("WarningChangeSpecimenTypeUnique"),
      HTML("<h4><b>Remove a Specimen Type</b></h4>"),
      selectInput("DeleteSpecimenType", label = NULL, choices = NULL, selected = 1),
      textOutput("WarningSpecimenTypeDeletion"),
      actionButton("DeleteSpecimenTypeAction", label = "Delete", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      HTML("<br></br>"),
      span(verbatimTextOutput("SpecimenReturnMessage"), style="font-size: 28px"),
    ),
    mainPanel(
      HTML("<h4><b>Specimen Types</b></h4>"),
      DT::dataTableOutput("TableSpecimenType"),
    ))
}