library(tools)

message('Loading global environment...')

# Global Variables
Global <- list(
  DefaultStateSearchTerm = "Active",
  DefaultStatusSearchTerm = "In Use"
)

database <- sampleDB:::.GetSampleDBPath()

# Session independent declarations go here
  observe({
    # Re-execute this reactive expression every 24 hrs
    invalidateLater(1000 * 60 * 60 * 24, NULL)
    date <- gsub("[T:]", "_", lubridate::format_ISO8601(lubridate::now()))

    # backup the database
    temp_current_db <- tempfile()
    backup_dir <- file.path(dirname(database), 'backups')
    backup_dest <- file.path(backup_dir, paste0("sampledb_", date, ".backup.gz"))
    list.backups <- list.files(backup_dir)
    Backup(database, temp_current_db)

    if (0 == length(list.backups)) {
      message("Creating the first backup!")
      Gzip(temp_current_db, backup_dest)
    } else {
      most_recent_backup <- file.path(backup_dir, tail(list.backups, 1))
      temp_eval_backup <- tempfile()
      if(!Gunzip(most_recent_backup, temp_eval_backup)) {
        message('could not decompress')
      }

      list.files <- c(current = temp_current_db, latest = temp_eval_backup)
      df.checksums <- data.frame(checksum = md5sum(list.files))

      if (0 == anyDuplicated(df.checksums)) {
        Gzip(temp_eval_backup, backup_dest)
      } else {
        message(paste(most_recent_backup, "has not changed."))
      }

      file.remove(temp_eval_backup)
    }
    file.remove(temp_current_db)

    # only keep the last 10 backups
    list.backups <- list.files(backup_dir)
    if (length(list.backups) > 10) { # could symbolize 
      # could stat the file but this should suffice
      file.remove(file.path(backup_dir, head(list.backups, 1))) 
    }
  })

dbUpdateEvent <- reactivePoll(1000 * 10, NULL,
    function() file.mtime(sampleDB:::.GetSampleDBPath()),
    function() {
      list.data <- list(
          plate_name = sampleDB::CheckTable("matrix_plate")$plate_name,
          box_name = sampleDB::CheckTable("box")$box_name,
          rdt_bag_name = sampleDB::CheckTable("bag")$bag_name,
          paper_bag_name = sampleDB::CheckTable("bag")$bag_name,
          study = sampleDB::CheckTable("study")$short_code,
          specimen_type = sampleDB::CheckTable("specimen_type")$label,
          location = sampleDB::CheckTable("location")$location_name
        )

      return(list.data)
    }
  )




