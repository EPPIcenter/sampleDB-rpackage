#' @export

Backup <- function(database, backup_dest) {
	system2("sqlite3",
	        paste0(file.path(database),
	               " \".backup ", backup_dest, "\""))

	return(file.exists(backup_dest))
}


