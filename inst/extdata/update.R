library(dplyr)

pkgname <- 'sampleDB'
current_db_version <- "1.0.0"
next_db_version <- "1.4.0"
# start with a database backup to the default directory
# Backup_SampleDB()

current_database_schema <- suppressWarnings(
  normalizePath(
    paste0(
      file.path(
        "db",
        next_db_version,
        paste("sampledb_database", current_db_version, sep = "_")
      ),
      ".sql")
  )
)

system2("sqlite3", paste(next_database, "<", next_database_sql))

next_database_schema <- suppressWarnings(
  normalizePath(
    paste0(
      file.path(
        "db",
        next_db_version,
        paste("sampledb_database", next_db_version, sep = "_")
      ),
    ".sql")
  )
)

read.


system2("sqlite3", paste(next_database, "<", next_database_sql))



next_database_sql <- system.file("extdata",
                            next_database_schema, package = pkgname)


next_database <- tempfile()


system2("sqlite3", paste(next_database, "<", next_database_sql))

database <- Sys.getenv("SDB_PATH")
con <- DBI::dbConnect(RSQLite::SQLite(), database)

# todo: ensure that current version is filtered correctly
correct_version <- tbl(con, "version") %>%
  filter(name == current_db_version) %>%
  summarise(found = n()) %>%
  collect() %>%
  pull(found) == 1

if (!correct_version) {
  stop(paste("Current database should be", current_db_version))
}

tables <- DBI::dbListTables(con = con)
for (table in tables) {
  switch(table,
         "study" = .update_study(database, con))
}

DBI::dbDisconnect(con)


.update_study <- function(con) {

}
