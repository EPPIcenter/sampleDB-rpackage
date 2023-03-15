UIDriedBloodSpotUpload <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 3,
      HTML("<h4><b>Upload Paper Samples Form</b></h4>"),
      hr(),
      fileInput("UploadPaperDataSet", "UploadPaperCSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      textOutput("WarningPaperUploadSampleID"),
      textOutput("WarningPaperUploadColnames"),
      textOutput("WarningUploadPaperSpecimenTypes"),
      textOutput("WarningPaperUploadDateFormat"),
      textOutput("WarningUploadPaperStudyShortCodes"),
      textOutput("WarningPaperSpecimenExists"),
      textInput("UploadPaperPlateID", label = "Bag Name"),
      textOutput("WarningPaperUploadContainer"),
      selectInput("UploadLocationPaperFreezerName", label = "Storage Location", choices = c("", CheckTable(database = database, "location")$name)),
      fluidRow(column(width = 1), column(width = 11, selectInput("UploadLocationPaperLevelI", label = HTML("<h5>Storage Location: Level I</h5>"), width = '100%', choices = NULL))),
      fluidRow(column(width = 1), column(width = 11, selectInput("UploadLocationPaperLevelII", label = HTML("<h5>Storage Location: Level II</h5>"), width = '100%', choices = NULL))),
      hr(),
      fluidRow(column(width = 12,
                      actionButton("UploadActionPaperSamples", width = '49%', label = "Upload Samples", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      actionButton("ClearUploadFormPaperSamples", width = '49%', label = "Clear Form"))),
      br(),
      span(verbatimTextOutput("UploadPaperReturnMessage1"), style="font-size: 28px"),
      span(verbatimTextOutput("UploadPaperReturnMessage2"), style="font-size: 28px")
    ),
    mainPanel(
      width = 9,
      HTML("<h4><center>This is an <b>Example SampleDB UploadPaperCSV</b>.</center></h4>"),
      fluidRow(
        column(width = 2),
        column(width = 8,
               span(verbatimTextOutput("ExampleUploadPaperCSVNoDate"), style ="max-width: 75px; text-align: center"),
        )),
      fluidPage(
        column(width = 1),
        column(width = 10,
               HTML("<h4><b>Creating an UploadMicronixCSV</b></h4>
                                   <h5>The combination of <code>study_subject</code>, <code>specimen_type</code> and <code>study_code</code> must be unique.
                               Consider adding a collection_date for the sample</h5>
                               <h4><b>Longitudinal Data</b></h4>
                               <h5>Simply append a column named <code>collection_date</code> to your CSV in order to add
                               longitudinal information to the samples. (Date format is YMD.)</h5>
                               <center><h4><b>Example SampleDB UploadPaperCSV</b> with <code>collection_date</code> Column</h4></center>"),
        ),
        column(width = 1)
      ),
      fluidRow(
        column(width = 2),
        column(width = 8,
               span(verbatimTextOutput("ExampleUploadPaperCSVDate"), style ="max-width: 75px; text-align: center"),
        )),
      # HTML("<h5>The combination of <code>study_subject</code>, <code>specimen_type</code>, <code>study_code</code> and <code>collection_date</code> must be unique</h5>")
    ))
}