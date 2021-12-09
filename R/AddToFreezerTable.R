#' @export

#freezer table options
add_to_freezer_table <- function(date_created, date_modified, description){
  conn <- dbConnect(RSQLite::SQLite(), "files/example_19-Oct-21.sample_db.sqlite")
  dbSendQuery(conn,
              'INSERT INTO location (created, last_updated, description) VALUES (?, ?, ?);',
              list(date_created, date_modified, description))
  dbDisconnect(conn)
}
