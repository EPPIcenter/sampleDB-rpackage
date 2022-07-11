#' @import dplyr
#' @export

CheckTableTx <- function(conn, table) {
  out_table <- RSQLite::dbGetQuery(conn, paste("SELECT * FROM", table)) %>% tibble()
  return(out_table)
}