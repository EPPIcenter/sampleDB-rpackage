#' Setup SampleDB Database
#' @import dplyr
#' @export
#' 


SampleDB_Setup <- function(){
  
  # ask user where to store database file
  path <- readline("Please provide a readable and writable path to store the database: ")
  
  # move database to user specified path
  sqlite_file <- system.file("extdata", "sampledb_database.sqlite", package = "sampleDB")
  system(paste("cp", sqlite_file, path))
  Sys.chmod(paste0(path, "/sampledb_database.sqlite"), mode = "0777")
  
  # user message
  message(paste0("SampleDB database file now exists at:\n", path, "/sampledb_database.sqlite"))
  
  #add variable to .renvirons-site using linux
  #if error is encountered counsel the user to either create .renvirons or make it readable or move it out of admin zone
  
  return()
}