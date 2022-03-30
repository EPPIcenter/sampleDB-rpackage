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
  
  ########
  password <- rstudioapi::askForPassword("Please Enter Password")
  
  #check that this program is being installed on a linux (inc. ubuntu) machine
  #check that rstudio server is installed
  #check that shiny server is installed
  
  #write sampledb file to `var/lib/sampleDB/`
  path <- "var/lib/sampleDB/"
  sqlite_file <- system.file("extdata", "sampledb_database.sqlite", package = "sampleDB")
  system(paste("sudo -kS cp", sqlite_file, path), input = password)
  message("Moved a SampleDB database template to \"/var/lib/sampleDB/sampledb_database.sqlite\"")
  # Sys.chmod(paste0(path, "/sampledb_database.sqlite"), mode = "0777") # may need to reformat cmd to include sudo
  
  #add variable to .Renviron-site
  #check that "/etc/R/Renviron.site" exists
  system(paste("sudo -kS bash -c \"echo SDB_PATH='\"'\"/databases/sampledb/v0.0.2/sampledb_database.sqlite\"'\"' >> /etc/R/Renviron.site\""), input = password)
  message("Added variable \"SDB_PATH\" to \"/etc/R/Renviron.site\"")
  
  # system("sudo -kS bash -c \"echo SDB_PATH='\"/databases/sampledb/v0.0.2/sampledb_database.sqlite\"' >> /etc/R/Renviron.site\"", input = "Gr33nhouse")
  
  ########
  # # ask user where to store database file
  # path <- readline("Please provide a readable and writable path to store the database: ")
  # 
  # stopifnot("Path does not exist" = dir.exists(path))
  # stopifnot("Cannot write to path" = file.access(path, mode = 2) == 0)
  #           
  # # move database to user specified path
  # sqlite_file <- system.file("extdata", "sampledb_database.sqlite", package = "sampleDB")
  # 
  # system(paste("cp", sqlite_file, path))
  # Sys.chmod(paste0(path, "/sampledb_database.sqlite"), mode = "0777")
  # 
  # # user message
  # message(paste0("SampleDB database file now exists at:\n", path, "/sampledb_database.sqlite"))
  # 
  # #add variable to .renvirons-site using linux
  # #if error is encountered counsel the user to either create .renvirons or make it readable or move it out of admin zone
}