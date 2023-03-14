library(yaml)

message('Loading global environment...')

# Global Variables
Global <- list(
  DefaultStateSearchTerm = "Active",
  DefaultStatusSearchTerm = "In Use"
)

database <- Sys.getenv("SDB_PATH")

# Session independent declarations go here

dbUpdateEvent <- reactivePoll(1000 * 5, NULL,
    function() file.mtime(Sys.getenv("SDB_PATH")),
    function() {
      list.data <- list(
          micronix_plate_name = sampleDB::CheckTable("micronix_plate")$name,
          cryovial_box_name = sampleDB::CheckTable("cryovial_box")$name,
          study = sampleDB::CheckTable("study") %>%
            pull(var = id, name = short_code),
          specimen_type = sampleDB::CheckTable("specimen_type")$name,
          location = sampleDB::CheckTable("location")$name,
          subject = sampleDB::CheckTable(database = database, "study_subject") %>%
            pull(var = study_id, name = name),
          status = sampleDB::CheckTable(database = database, "status")$name,
          state = sampleDB::CheckTable(database = database, "state")$name
        )

      return(list.data)
    }
  )




