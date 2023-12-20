library(lubridate)
library(RSQLite)
library(tools)

.Backup <- function(database, backup_dest) {

  args <- paste(paste0("\"", database, "\""), paste0("\".backup ", paste0("\'", backup_dest, "\'"), "\""))
  system2("sqlite3", args)

  return(file.exists(backup_dest))
}

.Compress <- function(source_file, destination_file) {
  gzfile(destination_file, "wb") %>% {
    writeBin(readBin(source_file, "raw", file.info(source_file)$size), .)
    close(.)
  }
  file.remove(source_file)  # Remove the original uncompressed file

  return(destination_file)
}

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

.Uncompress <- function(source_file, destination_file) {
  gzfile(source_file, "rb") %>% {
    writeBin(readBin(., "raw", file.info(source_file)$size), file(destination_file, "wb"))
    close(.)
  }
  return(file.exists(destination_file))
}

