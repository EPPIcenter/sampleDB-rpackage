
message('Loading global environment...')

# Session independent declarations go here
Backup_SampleDB <- function() {
    observe({
      # Re-execute this reactive expression every 24 hrs
      invalidateLater(1000 * 60 * 60 * 24, session = NULL)
      backup_sh <- file.path("/bin", "sampleDB_backup_generator.sh")
      if (file.exists(backup_sh)) {
        system2("bash", backup_sh)
      } else {
        message("ERROR: Backup script is missing. Run SampleDB_Setup() to re-install script.")
      }
    })
}

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

# Call on application launch without being
# tied to a session
Backup_SampleDB()


