
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
    message("firing!")
    invalidateLater(1000 * 60 * 60 * 24, NULL)
    Backup_SampleDB(database, file.path(dirname(database), 'backups'))
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




