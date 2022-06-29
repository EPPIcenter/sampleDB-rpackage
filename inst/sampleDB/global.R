
message('Loading global environment...')

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

Backup_SampleDB()
