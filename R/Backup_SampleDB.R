#' @import tools
#' @export

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
		if(!.extract(most_recent_backup, temp_eval_backup)) {
		  message('could not decompress')
		}

		list.files <- c(current = temp_current_db, latest = temp_eval_backup)
		df.checksums <- data.frame(checksum = md5sum(list.files))

		if (0 == anyDuplicated(df.checksums)) {
		  .compress(temp_eval_backup, backup_dest)
		} else {
			message(paste(most_recent_backup, "has not changed."))
		}

		file.remove(temp_eval_backup)
	}
  file.remove(temp_current_db)
}

.backup <- function(database, backup_dest) {
	system2("sqlite3",
	        paste0(file.path(database),
	               " \".backup ", backup_dest, "\""))

	return(file.exists(backup_dest))
}

BFR.SIZE = BFR.SIZE=1e+07

.compress <- function(source, destination) {
	if(Sys.info()[['sysname']] == "Windows") {
		zip(source, destination)
	} else {

	  # From R.UTILS

		# Setup input and output connections
	  inn <- file(source, open="rb")
	  on.exit(if (!is.null(inn)) close(inn))

	  outComplete <- FALSE
	  out <- gzfile(destination, open="wb")
	  on.exit({
	    if (!is.null(out)) close(out)
	    # Remove incomplete file?
	    if (!outComplete) file.remove(destination)
	  }, add=TRUE)

	  # Process
	  nbytes <- 0
	  repeat {
	    bfr <- readBin(inn, what=raw(0L), size=1L, n=BFR.SIZE)
	    n <- length(bfr)
	    if (n == 0L) break
	    nbytes <- nbytes + n
	    writeBin(bfr, con=out, size=1L)
	    bfr <- NULL  # Not needed anymore
	  }
	  outComplete <- TRUE
	  close(out)
	  out <- NULL
	}

  return(file.exists(destination))
}

.extract <- function(source, destination) {
  if(Sys.info()[['sysname']] == "Windows") {
    unzip(source, destination)
  } else {

  	# From R.UTILS

    inn <- gzfile(source, open="rb")
	  on.exit(if (!is.null(inn)) close(inn))

	  outComplete <- FALSE
	  out <- file(destination, open="wb")
	  on.exit({
	    if (!is.null(out)) close(out)
	    # Remove incomplete file?
	    if (!outComplete) file.remove(destination)
	  }, add=TRUE)

	  # Process
	  nbytes <- 0
	  repeat {
	    bfr <- readBin(inn, what=raw(0L), size=1L, n=BFR.SIZE)
	    n <- length(bfr)
	    if (n == 0L) break
	    nbytes <- nbytes + n
	    writeBin(bfr, con=out, size=1L)
	    bfr <- NULL  # Not needed anymore
	  }
	  outComplete <- TRUE
	  close(out)
	  out <- NULL

  }

  return(file.exists(destination))
}


