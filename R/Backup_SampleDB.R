#' @import lubridate
#' @export

Backup_SampleDB <- function(
  database = .sampleDB$database,
  backup_dest = file.path(.sampleDB$backups, paste0("sampledb_", gsub("[T:]", "_", lubridate::format_ISO8601(lubridate::now())), ".backup.gz")),
  force = FALSE)
{
  temp_current_db <- tempfile()
  list.backups <- list.files(.sampleDB$backups)
  sampleDB:::.Backup(database, temp_current_db)

  if (isTRUE(force) || 0 == length(list.backups)) {
    sampleDB:::.Gzip(temp_current_db, backup_dest)
  } else {
    most_recent_backup <- file.path(.sampleDB$backups, tail(list.backups, 1))
    temp_eval_backup <- tempfile()
    if(!sampleDB:::.Gunzip(most_recent_backup, temp_eval_backup)) {
      message('could not decompress')
    }

    list.files <- c(current = temp_current_db, latest = temp_eval_backup)
    df.checksums <- data.frame(checksum = md5sum(list.files))

    if (0 == anyDuplicated(df.checksums)) {
      sampleDB:::.Gzip(temp_eval_backup, backup_dest)
    } else {
      message(paste(most_recent_backup, "has not changed."))
    }

    file.remove(temp_eval_backup)
  }
  invisible(file.remove(temp_current_db))
}