library(yaml)
library(RSQLite)

message('Loading global environment...')

# Global Variables
Global <- list(
  DefaultStateSearchTerm = "Active",
  DefaultStatusSearchTerm = "In Use"
)

database <- Sys.getenv("SDB_PATH")

con=dbConnect(RSQLite::SQLite(), database)
storage.type.id.map = tbl(con, "sample_type") %>%
  pull(name, name = id)

storage.type.id.map = tolower(storage.type.id.map)

# print(storage.type.id.map)

dbDisconnect(con)
