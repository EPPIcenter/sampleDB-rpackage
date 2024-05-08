library(lubridate)
library(RSQLite)
library(tools)


#' Internal function to backup a SQLite database
#'
#' @param database Path to the SQLite database file.
#' @param backup_dest Destination path for the backup file.
#' @return Returns TRUE if the backup file exists, FALSE otherwise.
.Backup <- function(database, backup_dest) {

  args <- paste(paste0("\"", database, "\""), paste0("\".backup ", paste0("\'", backup_dest, "\'"), "\""))
  system2("sqlite3", args)

  return(file.exists(backup_dest))
}


#' Internal function to compress a file
#'
#' @param source_file The original file to be compressed.
#' @param destination_file The destination path for the compressed file.
#' @return The path to the compressed file.
.Compress <- function(source_file, destination_file) {
  conn <- gzfile(destination_file, "wb")
  on.exit(close(conn), add = TRUE)  # Ensure the connection is closed when the function exits

  writeBin(readBin(source_file, "raw", file.info(source_file)$size), conn)
  file.remove(source_file)  # Remove the original uncompressed file

  return(destination_file)
}


#' Backup Sample Database
#'
#' Backs up the sample database and optionally compares it to the latest backup to detect changes.
#' The function automatically compresses the backup file.
#'
#' @param database Path to the sample database, defaults to the "SDB_PATH" environment variable.
#' @param backup_dir Directory to store the backups; if NULL, it defaults to a subdirectory 'backups' in the database directory.
#' @param checksum Logical; if TRUE, checks if the new backup is different from the last one.
#' @export
Backup_SampleDB <- function(database = Sys.getenv("SDB_PATH"), backup_dir = NULL, checksum = TRUE) {

  if (is.null(backup_dir)) {
    backup_dir <- file.path(dirname(database), "backups")
  }

  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }

  backup_filename <- paste0("sampledb_", gsub("[T:]", "_", format_ISO8601(now())), ".backup")
  backup_dest <- file.path(backup_dir, backup_filename)

  temp_current_db <- tempfile(fileext = ".backup")
  .Backup(database, temp_current_db)

  # Checksum comparison
  if (checksum) {
    backups <- list.files(backup_dir, pattern = "\\.gz$", full.names = TRUE)
    if (length(backups) > 0) {
      latest_backup <- backups[length(backups)]
      temp_latest_backup <- tempfile(fileext = ".backup")
      .Uncompress(latest_backup, temp_latest_backup)

      if (any(duplicated(tools::md5sum(c(temp_current_db, temp_latest_backup))))) {
        message("No changes since last backup. Backup not saved.")
        file.remove(temp_current_db)
        return(invisible(NULL))
      }
      file.remove(temp_latest_backup)
    }
  }

  backup_dest <- paste0(backup_dest, ".gz")
  compressed_file_path <- .Compress(temp_current_db, backup_dest)
  message(paste("Backup completed:", compressed_file_path))

  return(compressed_file_path)
}


#' Internal function to uncompress a file
#'
#' @param source_file Compressed file path.
#' @param destination_file Destination path for the uncompressed file.
#' @return Returns TRUE if the destination file exists after decompression.
.Uncompress <- function(source_file, destination_file) {
  conn_read <- gzfile(source_file, "rb")
  on.exit(close(conn_read), add = TRUE)

  conn_write <- file(destination_file, "wb")
  on.exit(close(conn_write), add = TRUE)

  writeBin(readBin(conn_read, "raw", file.info(source_file)$size), conn_write)
  return(file.exists(destination_file))
}


