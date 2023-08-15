library(yaml)

message('Loading global environment...')

# Global Variables
Global <- list(
  DefaultStateSearchTerm = "Active",
  DefaultStatusSearchTerm = "In Use"
)

database <- Sys.getenv("SDB_PATH")

samples_json <- load_parse_json("samples.json")
global_sample_names_ids_list <- setNames(lapply(samples_json$samples, function(sample) sample$id),
                             sapply(samples_json$samples, function(sample) sample$name))


print(global_sample_names_ids_list)

# Extract all unique sample types and user actions

# This should be read in?
global_upload_type_list <- c("Samples" = "samples", "Controls" = "controls")
global_sample_actions_list <- c("Upload" = "upload", "Move" = "move")
global_sample_file_types <- get_sample_file_types()
