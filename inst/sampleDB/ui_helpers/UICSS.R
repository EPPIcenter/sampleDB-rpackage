UICSS <- function(){
  tags$head(
    tags$style(
      HTML("
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
        }
        .no-select {
          -webkit-touch-callout: none; /* iOS Safari */
          -webkit-user-select: none;   /* Chrome/Safari/Opera */
          -khtml-user-select: none;    /* Konqueror */
          -moz-user-select: none;      /* Firefox */
          -ms-user-select: none;       /* Internet Explorer/Edge */
          user-select: none;
        }
        /* Your custom_css rules below */
        .custom-dropdown, .dropdown-menu .dropdown-item {
            width: 100%;
        }
        button, .btn {
            background-color: #FFF6D0;
            color: black;
        }
        button:hover, .btn:hover {
            background-color: #FFAC45;
            color: black;
        }
        .dropdown-menu {
            padding: 0;
        }
        .navbar {
            --bslib-navbar-light-bg: #212529 !important;
        }
        .navbar .nav-link,
        .navbar .nav-item > .nav-link,
        .navbar-nav > li > a {
            color: #fff !important; /* All tabs white by default */
        }

        .navbar .nav-link.active,
        .navbar .nav-item.active > .nav-link,
        .navbar-nav > .active > a,
        .navbar-nav > .active > a:focus,
        .navbar-nav > .active > a:hover {
            color: #FF4B4B !important; /* Active tab red */
            background-color: transparent !important;
        }
      ")
    )
  )
}