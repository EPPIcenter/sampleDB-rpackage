#' @import lubridate
#' @import tools
#' @export

Backup_SampleDB <- function(
  database = Sys.getenv("SDB_PATH"),
  backup_dest = NULL,
  checksum = FALSE)
{
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

  temp_current_db <- tempfile()
  backups <- normalizePath(file.path(dirname(database), "backups"))
  list.backups <- list.files(backups)
  sampleDB:::.Backup(database, temp_current_db)

  if (isFALSE(checksum) || 0 == length(list.backups)) {
    sampleDB:::.Compress(temp_current_db, backup_dest)
  } else {
    most_recent_backup <- file.path(backups, tail(list.backups, 1))
    temp_eval_backup <- tempfile()
    if(!sampleDB:::.Uncompress(most_recent_backup, temp_eval_backup)) {
      message('could not decompress')
    }

    list.files <- c(current = temp_current_db, latest = temp_eval_backup)
    df.checksums <- data.frame(checksum = tools::md5sum(list.files))

    if (0 == anyDuplicated(df.checksums)) {
      sampleDB:::.Compress(temp_current_db, backup_dest)
    } else {
      message(paste0("No changes since last backup [",
        most_recent_backup, "]"))
    }
    file.remove(temp_eval_backup)
  }
  invisible(file.remove(temp_current_db))
}