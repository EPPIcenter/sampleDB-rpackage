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
          micronix_plate_name = CheckTable("micronix_plate")$name,
          cryovial_box_name = CheckTable("cryovial_box")$name,
          study = CheckTable("study") %>%
            pull(var = id, name = short_code),
          specimen_type = CheckTable("specimen_type")$name,
          location = CheckTable("location")$name,
          subject = CheckTable(database = database, "study_subject") %>%
            pull(var = study_id, name = name),
          status = CheckTable(database = database, "status")$name,
          state = CheckTable(database = database, "state")$name
        )

      return(list.data)
    }
  )




