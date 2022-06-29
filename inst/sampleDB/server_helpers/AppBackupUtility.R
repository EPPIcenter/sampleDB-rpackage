library(tools)
library(R.utils)

.backup <- function(source, destination) {
	system2("sqlite3",
	        paste0(file.path(source),
	               " \".backup ", destination, "\""))

	return(file.exists(destination))
}

.compress <- function(source, destination) {
	if(Sys.info()[['sysname']] == "Windows") {
		zip(source, destination)
	} else {
	  R.utils::gzip(filename = source, destname = destination, remove = TRUE)
	}

  return(file.exists(destination))
}

.uncompress <- function(source, destination) {
  if(Sys.info()[['sysname']] == "Windows") {
    unzip(source, destination)
  } else {
    R.utils::gunzip(filename = source, destname = destination, remove = FALSE)
  }

  return(file.exists(destination))
}

Backup_SampleDB <- function(database, backup_dir) {
	date <- gsub("[T:]", "_", lubridate::format_ISO8601(lubridate::now()))

	# backup the database
	temp_current_db <- tempfile()
	backup_dest <- file.path(backup_dir, paste0("sampledb_", date, ".backup.gz"))
	list.backups <- list.files(backup_dir)

	.backup(database, temp_current_db)

	if (0 == length(list.backups)) {
		message("Creating the first backup!")
		.compress(temp_current_db, backup_dest)
	} else {
		most_recent_backup <- file.path(backup_dir, tail(list.backups, 1))
		temp_eval_backup <- tempfile()
		if(!.uncompress(most_recent_backup, temp_eval_backup)) {
		  message('could not decompress')
		}

		list.files <- c(current = temp_current_db, latest = temp_eval_backup)
		df.checksums <- data.frame(checksum = md5sum(list.files))

		if (0 == anyDuplicated(df.checksums)) {
		  .compress(temp_eval_backup, backup_dest)
		} else {
			message(paste(most_recent_backup, "has not changed."))
		}
	}
}
