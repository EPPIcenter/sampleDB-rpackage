library(DBI)
library(reactable)
library(shinyjs)
library(bslib)

UIUploadSamples <- function() {
  
  ui <- layout_sidebar(
    sidebar = sidebar(
      title = "Add Specimens to SampleDB",
      width = validateCssUnit("25%"),
      shinyjs::useShinyjs(),
      radioButtons("UploadType", "Choose Your Upload Type", choices = global_upload_type_list),
      conditionalPanel(
        condition = "input.UploadType == 'samples'",
        h4("Upload Samples"),
        radioButtons("UploadSampleType","Choose a Sample Storage Type", choices = get_sample_types_by_action("upload"), selected = "micronix", inline = TRUE),
        conditionalPanel(
          condition = "input.UploadType == 'samples' && input.UploadSampleType == 'dbs_sample'",
          hr(),
          radioButtons("UploadDBSSampleManifest", "Select a DBS Container", inline = TRUE, choices = c("Box" = "box", "Bag" = "bag"), selected = "box")
        ),
        hr(),
        radioButtons("UploadFileType", "Choose a file type", choices = get_file_types_for_sample("micronix"), inline = TRUE, selected = "na"),
        hr(),
        fileInput("UploadSampleDataSet", "Upload Samples File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        #add freezer infor
        shinyjs::hidden(selectizeInput("UploadManifestName", label = "Plate Name", choices = c(),  options = list(create = TRUE))),
        shinyjs::hidden(selectInput("UploadLocationRoot", label = "Upload Location", choices = c())),        
        shinyjs::hidden(selectInput("UploadLocationLevelI", label = "Shelf Name", choices = c())),
        shinyjs::hidden(selectInput("UploadLocationLevelII", label = "Basket Name", choices = c()))
      ),
      conditionalPanel(
        condition = "input.UploadType == 'controls'",
        h4("Upload Controls"),
        radioButtons("UploadControlType","Control Storage Type", choices = get_all_control_types(), selected = "dbs_sheet", inline = TRUE),
        hr(),
        radioButtons("UploadControlAction", "Action", choices = get_control_action_types("dbs_sheet"), inline = TRUE),
        fileInput("InputUploadControls", label = "Upload DBS Control Sheet", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      hr(),
      #action buttons
      fluidRow(column(width = 6, actionButton("UploadAction", width = '100%', label = "Upload")),
               column(width = 6, actionButton("ClearUploadForm", width = '100%', label = "Clear Form"))),
      span(verbatimTextOutput("UploadOutputConsole"))
    ),
    card_body(
      conditionalPanel(
        condition = "input.UploadType == 'samples'",
        includeMarkdown(get_markdown_path("upload_samples_instructions.md"))
      ),
      conditionalPanel(
        condition = "input.UploadType == 'controls'",
        includeMarkdown(get_markdown_path("upload_controls_instructions.md"))
      ),
      tags$p("If you would like to download a template file, press the button below."),
      fluidRow(
        column(
          width = 6,
          downloadButton("UploadFileTemplate")
        )
      ),
      tags$h5("Required Fields"),
      tags$p("Below are", tags$strong("required"), "columns that", tags$strong("must"), "be included in your file."),
      reactableOutput("UploadFileExampleRequired"),
      tags$h5("Conditional Fields"),
      tags$p("Some fields", tags$strong("may"), "be required depending on the contents of your upload."),
      reactableOutput("UploadFileExampleConditional"),
      tags$h5("Optional Fields"),
      tags$p("The columns below do not need to be included in your upload."),
      reactableOutput("UploadFileExampleOptional")
    )
  )

  return(ui)
}