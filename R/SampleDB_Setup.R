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
  
  #1. make directory "/var/lib/sampleDB/"
  system(paste("sudo -kS mkdir \"/var/lib/sampleDB\""), input = password)
  
  #2. write sampledb .sqlite file to "var/lib/sampleDB/"
  path <- "/var/lib/sampleDB/"
  sqlite_file <- system.file("extdata", "sampledb_database.sqlite", package = "sampleDB")
  system(paste("sudo -kS cp", sqlite_file, path), input = password)
  message("Moved a SampleDB SQLite template file to \"/var/lib/sampleDB/sampledb_database.sqlite\"")
  # Sys.chmod(paste0(path, "/sampledb_database.sqlite"), mode = "0777")
  
  #3. add variable to .Renviron-site
  system(paste("sudo -kS bash -c \"echo SDB_PATH='\"'\"/var/lib/sampleDB/sampledb_database.sqlite\"'\"' >> /etc/R/Renviron.site\""), input = password)
  message("Added variable \"SDB_PATH\" to \"/etc/R/Renviron.site\"")
  
  #4. set up rshiny server
  system("sudo -kS ln -s /usr/lib/R/site-library/sampleDB/sampleDB /srv/shiny-server", input = password)
  ip <- system("hostname -I | awk '{print $1}' 2>&1", intern = TRUE)
  url <- paste0("http://", ip, ":3838/sampleDB/")
  message(paste("Your SampleDB app is now available live at", url))
  
  #5. setup backup system -- can be done as a bash file
  system(paste("sudo -kS mkdir \"/var/lib/sampleDB/backups\""), input = password)
  system(paste("sudo -kS mkdir \"/var/lib/sampleDB/upload_files\""), input = password)
  system(paste("sudo -kS mkdir \"/var/lib/sampleDB/move_files\""), input = password)
  
  #copy backup generator back script to /bin
  sqlite_file <- system.file("extdata", "sampleDB_backup_generator.sh", package = "sampleDB")
  path <- "/bin/"
  system(paste("sudo -kS cp", sqlite_file, path), input = password)
  
  
  #user message if this password does not have sudo privileges
  #check that this program is being installed on a linux (inc. ubuntu) machine
  #check that rstudio server is installed
  #check that shiny server is installed
  #check that "/etc/R/Renviron.site" exists
  #a lot of things need to be 777'd (e.g. /var/lib/sampleDB/sampledb_database.sqlite)
  
  #https://stackoverflow.com/questions/1518729/change-sqlite-database-mode-to-read-write
}