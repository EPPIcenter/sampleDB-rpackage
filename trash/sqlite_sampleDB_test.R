
library(RSQLite)
library(tidyverse)
library(lubridate)


#################################
#functions

check_table <- function(table){
  conn <- dbConnect(RSQLite::SQLite(), "~/eppicenter/library/R/files/example_19-Oct-21.sample_db.sqlite")
  print(dbGetQuery(conn, paste("SELECT * FROM", table)) %>% tibble())
  dbDisconnect(conn)
}

#freezer table options
add_to_freezer_table <- function(date_created, date_modified, description){
  conn <- dbConnect(RSQLite::SQLite(), "files/example_19-Oct-21.sample_db.sqlite")
  dbSendQuery(conn, 
              'INSERT INTO location (created, last_updated, description) VALUES (?, ?, ?);', 
              list(date_created, date_modified, description))
  dbDisconnect(conn)
}

delete_from_freezer_table <- function(id){
  conn <- dbConnect(RSQLite::SQLite(), "files/example_19-Oct-21.sample_db.sqlite")
  dbSendQuery(conn, 
              paste0("DELETE FROM location WHERE id = ", id,";"))
  dbDisconnect(conn)
}

modify_freezer_table <- function(id, description){
  conn <- dbConnect(RSQLite::SQLite(), "~/eppicenter/library/R/files/example_19-Oct-21.sample_db.sqlite")
  dbSendQuery(conn,
              paste0("UPDATE location SET description = '", description,"' WHERE id = ", id, ";"))
  dbDisconnect(conn)
}

##################################
# List all the tables available in the database
conn <- dbConnect(RSQLite::SQLite(), "~/eppicenter/library/R/files/example_19-Oct-21.sample_db.sqlite")
tables.list <- list()
for (table in dbListTables(conn)){
  tables.list[[table]] <- dbGetQuery(conn, paste("SELECT * FROM", table)) %>% tibble()
}
dbDisconnect(conn)

#read in traxer and visionmate csvs
csv.traxer <- read_csv("~/eppicenter/library/R/files/traxer_data_example.csv")
csv.visionmate <- read_csv("~/eppicenter/library/R/files/visionmate_data_example.csv")


csv.traxer %>% 
  mutate(PlateRow = substring(Position, 1, 1),
         PlateCol = substring(Position, 2),
         Barcode = `Tube ID`) %>%
  select(-c(Position:Date))

csv.visionmate %>%
  rename(PlateRow = LocationRow,
         PlateCol = LocationColumn,
         Barcode = TubeCode)

# List all the tables available in the database
tables.list <- list()
for (table in dbListTables(conn)){
  tables.list[[table]] <- dbGetQuery(conn, paste("SELECT * FROM", table)) %>% tibble()
}

#############################

# dbGetQuery(conn, "SELECT * FROM storage_container") %>% tibble()
# dbGetQuery(conn, "SELECT * FROM matrix_plate") %>% tibble()
# 
# # Gather the first 10 rows in the cars_data table
# dbGetQuery(conn, "SELECT * FROM cars_data LIMIT 10")
# 
# # Get the car names and horsepower of the cars with 8 cylinders
# dbGetQuery(conn,"SELECT car_names, hp, cyl FROM cars_data WHERE cyl = 8")