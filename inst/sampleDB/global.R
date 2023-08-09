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

# Extract all unique sample types and user actions

# This should be read in?
global_sample_actions_list <- c("Upload" = "upload", "Move" = "move")

# Initialize a list to hold the results
example_data <- list()

# Loop over all sample types and user actions
for (sample_type in unname(global_sample_names_ids_list)) {
  for (user_action in unname(global_sample_actions_list)) {
    for (file_type in c("na", "visionmate", "traxcer")) {  # Assuming these are the file types you want to loop over
      # Fetch the fields for each combination of sample_type, user_action, and file_type
      fields <- get_sample_file_columns(sample_type, user_action, file_type)
      # Store the results in the sample_data list
      example_data[[paste(sample_type, user_action, file_type, sep = "_")]] <- fields
    }
  }
}

