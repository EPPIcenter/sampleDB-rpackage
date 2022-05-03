UICSS <- function(){
  tags$head(
    tags$style(HTML("
              .shiny-file-input-progress {
                  display: none
              }
              .progress-bar {
                  color: transparent!important
              }
              h5 {
                  line-height: 150%;
              }
              ::-webkit-input-placeholder {
                  font-style: italic;
              }
              :-moz-placeholder {
                  font-style: italic;
              }
              ::-moz-placeholder {
                  font-style: italic;
              }
              :-ms-input-placeholder {  
                font-style: italic; 
              }
              .shiny-output-error-validation {
                    color: #c4244c; font-weight: normal;
              }")))
}