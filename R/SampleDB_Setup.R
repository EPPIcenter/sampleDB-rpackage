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
#' @export
#'

SampleDB_Setup <- function(){
  password <- rstudioapi::askForPassword("Please Enter Password")

  # Create the path to the database
  sdb_path <- sampleDB:::.GetSampleDBPath()


  # 1) Script create / appends environ file
  # 2) Creates / grants access to files + folders
  # 3) Copy backup generator file

  setup_sh <- system.file("extdata", "setup.sh", package = "sampleDB")
  sdb_backup_gen <- system.file("extdata", "sampleDB_backup_generator.sh", package = "sampleDB")
  sqlite_file <- system.file("extdata", "sampledb_database.sqlite", package = "sampleDB")

  # commands that need write access go in here
  cmd <- paste("sudo -kS bash", setup_sh, sdb_path, sqlite_file, sdb_backup_gen)
  system(cmd, input = password, intern = T)

  # retrieve IP address
  ip_all <- system2("hostname", "-I", stdout = T)
  ip <- as.list(
    strsplit(ip_all, " ")[[1]]
    )[[1]]

  url <- paste0("http://", ip, ":3838/sampleDB/")
  message(paste("Your SampleDB app is now available live at", url))

  #user message if this password does not have sudo privileges
  #check that this program is being installed on a linux (inc. ubuntu) machine
  #check that rstudio server is installed
  #check that shiny server is installed
  #check that "/etc/R/Renviron.site" exists
  #a lot of things need to be 777'd (e.g. sampleDB:::.GetSampleDBPath())

  #https://stackoverflow.com/questions/1518729/change-sqlite-database-mode-to-read-writep

}
