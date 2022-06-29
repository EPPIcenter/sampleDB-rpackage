#' Setup SampleDB Database
#' 
#' Save SampleDB's database file to a specified location
#' 
#' @examples
#' \dontrun{
#' library(sampleDB)
#' SampleDB_Setup()
#' }
#' 
#' @import dplyr
#' @import getPass
#' @export
#' 


SampleDB_Setup <- function(){

  password <- NULL
  if (rstudioapi::isAvailable()) {
    password <- rstudioapi::askForPassword("Please Enter Password")
  } else {
    password <- getPass::getPass("Please Enter Password: ")
  }

  # ROOT
  root_path <- ifelse(.is_windows(),
    file.path(Sys.getenv("programdata"), "sampleDB"),
    file.path("","var","lib","sampleDB"))

  message(paste("root directory:", root_path))

  # 1) Get Renviron.site file
  environ_path <-file.path(R.home(), "etc", "Renviron.site")

  # 2) Create the path to the database
  sdb_path <- file.path(root_path, "sampledb_database.sqlite")

  # 1) Script create / appends environ file
  # 2) Creates / grants access to files + folders
  # 3) Copy backup generator file

  setup_script <- system.file("extdata", ifelse(.is_windows(), "setup.bat", "setup.sh"), package = "sampleDB")
  sqlite_file <- system.file("extdata", "sampledb_database.sqlite", package = "sampleDB")

  # commands that need write access go in here

  if (!.is_windows()) {
    cmd <- paste("sudo -kS bash", setup_script, sdb_path, sqlite_file, environ_path)
  system(cmd, input = password, intern = T)

  # retrieve IP address
  ip_all <- system2("hostname", "-I", stdout = T)
  ip <- as.list(
    strsplit(ip_all, " ")[[1]]
    )[[1]]

  } else {
    cmd <- paste("cmd /c", setup_script, sdb_path, sqlite_file, environ_path)
    system(cmd)
  }

  url <- paste0("http://", ip, ":3838/sampleDB/")
  message(paste("Your SampleDB app is now available live at", url))

  #user message if this password does not have sudo privileges
  #check that this program is being installed on a linux (inc. ubuntu) machine
  #check that rstudio server is installed
  #check that shiny server is installed
  #check that "/etc/R/Renviron.site" exists
}

.is_windows <- function() (tolower(.Platform$OS.type) == "windows")

