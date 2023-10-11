#' @import lubridate
#' @import tools
#' @export

Backup_SampleDB <- function(database = Sys.getenv("SDB_PATH"),
                            backup_dest = NULL,
                            checksum = FALSE) {


  # Display current database size
  db_size <- file.info(database)$size
  message(paste("Current database size: ", format(db_size, big.mark = ",", scientific = FALSE), " bytes"))
  message(paste("Current database path: ", database))

  # Determine backup destination
  if (is.null(backup_dest)) {
    backup_dest <- suppressWarnings(
      normalizePath(
        file.path(
          dirname(database),
          "backups",
          paste0("sampledb_",
                 gsub("[T:]", "_", lubridate::format_ISO8601(lubridate::now())),
                 ".backup"))))
  }
  
  message(paste("Backup will be saved at: ", backup_dest))

  # Temporary file for initial backup
  temp_current_db <- tempfile()
  message("Creating initial backup...")
  if (.Backup(database, temp_current_db)) {
    init_backup_size <- file.info(temp_current_db)$size
    message(paste("Initial backup created. Size: ", format(init_backup_size, big.mark = ",", scientific = FALSE), " bytes"))
  } else {
    message("Failed to create initial backup.")
    return(FALSE)
  }

  # Check existing backups
  backups <- normalizePath(file.path(dirname(database), "backups"))
  list.backups <- list.files(backups)
  
  # Perform checksum if needed
  if (isFALSE(checksum) || 0 == length(list.backups)) {
    message("Compressing backup...")
    .Compress(temp_current_db, backup_dest)
  } else {
    message("Comparing with the most recent backup...")
    most_recent_backup <- file.path(backups, tail(list.backups, 1))
    temp_eval_backup <- tempfile()
    
    if(!.Uncompress(most_recent_backup, temp_eval_backup)) {
      message('Decompression failed. Proceeding without comparison.')
    }

    list.files <- c(current = temp_current_db, latest = temp_eval_backup)
    df.checksums <- data.frame(checksum = tools::md5sum(list.files))

    if (0 == anyDuplicated(df.checksums)) {
      message("Backup differs from the most recent one. Compressing...")
      .Compress(temp_current_db, backup_dest)
    } else {
      message(paste0("Backup is identical to the most recent one. Skipping compression."))
    }
    file.remove(temp_eval_backup)
  }
  invisible(file.remove(temp_current_db))
  message("Backup process completed.")
}


.Backup <- function(database, backup_dest) {

  args <- paste(paste0("\"", database, "\""), paste0("\".backup ", paste0("\'", backup_dest, "\'"), "\""))

  message(paste("Issuing system command: sqlite3", args))
  system2("sqlite3", args)

  return(file.exists(backup_dest))
}

.Uncompress <- function(source, destination, buffer_size = 1e+07) {
  compressed_file <- paste0(source, ".gz")

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
    bfr <- readBin(inn, what=raw(0L), size=1L, n=buffer_size)
    n <- length(bfr)
    if (n == 0L) break
    nbytes <- nbytes + n
    writeBin(bfr, con=out, size=1L)
    bfr <- NULL  # Not needed anymore
  }
  outComplete <- TRUE
  close(out)
  out <- NULL

  return(file.exists(destination))
}


.Compress <- function(source, destination, buffer_size = 1e+07) {
  is_windows <- (Sys.info()[['sysname']] == "Windows")
  compressed_file <- paste0(destination, ".gz")

  # From R.UTILS

  # Setup input and output connections

  inn <- file(source, open="rb")
  on.exit(if (!is.null(inn)) close(inn))

  outComplete <- FALSE
  out <- gzfile(compressed_file, open="wb")
  on.exit({
    if (!is.null(out)) close(out)
    # Remove incomplete file?
    if (!outComplete) {
      file.remove(compressed_file)
    }
  }, add=TRUE)

  # Process
  nbytes <- 0
  repeat {
    bfr <- readBin(inn, what=raw(0L), size=1L, n=buffer_size)
    n <- length(bfr)
    if (n == 0L) break
    nbytes <- nbytes + n
    writeBin(bfr, con=out, size=1L)
    bfr <- NULL  # Not needed anymore
  }
  outComplete <- TRUE
  close(out)
  out <- NULL

  return(file.exists(compressed_file))
}