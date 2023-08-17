library(yaml)

message('Loading global environment...')

# Global Variables
Global <- list(
  DefaultStateSearchTerm = "Active",
  DefaultStatusSearchTerm = "In Use"
)

database <- Sys.getenv("SDB_PATH")

# Extract all unique sample types and user actions

# This should be read in?
global_upload_type_list <- c("Samples" = "samples", "Controls" = "controls")