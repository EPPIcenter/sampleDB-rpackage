#' Helper function to read JSON files
#'
#' Reads the JSON files provided by the filename.
#' 
#' @param filename The name of the JSON file to be read.
#' 
#' @return A list structure containing the contents of the JSON file.
#' @importFrom jsonlite fromJSON
#' @keywords internal
#' @noRd
read_json_file <- function(filename) {
  file_path <- get_data_file_path(filename)
  fromJSON(file_path, flatten = TRUE)
}

#' Get the path to a data file in the inst/app/www directory
#'
#' This function takes a filename and returns the path to the file in the inst/app/www directory of the package.
#'
#' @param file_name The name of the file.
#' @return The path to the file.
#' @examples
#' \dontrun{
#' file_path <- get_data_file_path("mydata.json")
#' }
#' @keywords internal
#' @export
#' @noRd
get_data_file_path <- function(file_name) {
  system.file(file.path("app", "www"), file_name, package = "sampleDB")
}

#' Get sample types from a JSON file
#'
#' Retrieves a named list for sample types with "name" as the names in the list and "type" as the values.
#' 
#' @param samples_file Name of the samples JSON file. Default is "samples.json".
#' 
#' @return A named list of sample types.
#' @export
get_sample_types <- function(samples_file = "samples.json") {
  data <- read_json_file(samples_file)
  types <- setNames(names(data$samples), sapply(data$samples, function(x) x$name))
  return(types)
}

#' Get sample types from a JSON file by action
#'
#' Retrieves a named list for sample types with "name" as the names in the list and "type" as the values, filtered by action.
#' 
#' @param samples_file Name of the samples JSON file. Default is "samples.json".
#' 
#' @return A named list of sample types.
#' @export
get_sample_types_by_action <- function(action, samples_file = "samples.json") {

  stopifnot("Action must be 'move' or 'upload'" = action %in% c("move", "upload"))

  data <- read_json_file(samples_file)
  action_match <- sapply(data$samples, function(x) if (action %in% x$actions) x$name)
  null_removed <- Filter(Negate(is.null), action_match)
  setNames(names(null_removed), sapply(null_removed, function(x) x))
}

#' Get control types from a JSON file
#'
#' Retrieves a named list for control types with "name" as the names in the list and "type" as the values.
#' 
#' @param controls_file Name of the controls JSON file. Default is "controls.json".
#' 
#' @return A named list of control types.
#' @export
get_control_types <- function(controls_file = "controls.json") {
  data <- read_json_file(controls_file)
  types <- setNames(names(data$controls), sapply(data$controls, function(x) x$name))
  return(types)
}

#' Helper function to read the app configuration file
#'
#' Reads the app.json file which provides application specific configuration.
#'
#' @param app_file (default: "app.json") Path to the JSON file containing application configurations.
#' 
#' @return A list structure containing the contents of the app configuration file.
#' 
#' @keywords internal
#' @noRd
read_app_file <- function(app_file = "app.json") {
  read_json_file(app_file)
}

#' Retrieve file types from the app configuration file
#'
#' Reads the app.json file and extracts available file type identifiers.
#'
#' @param app_file (default: "app.json") Path to the JSON file containing application configurations.
#' 
#' @return A named list of file type identifiers where keys from the JSON are values and the values are the names.
#' 
#' @keywords internal
#' @noRd
get_file_types <- function(app_file = "app.json") {
  app_data <- read_app_file(app_file)
  return(setNames(names(app_data$file_type_identifiers), app_data$file_type_identifiers))
}

#' Retrieve action types from the app configuration file
#'
#' Reads the app.json file and extracts available action identifiers.
#'
#' @param app_file (default: "app.json") Path to the JSON file containing application configurations.
#' 
#' @return A named list of action identifiers where keys from the JSON are values and the values are the names.
#' 
#' @keywords internal
#' @noRd
get_action_types <- function(app_file = "app.json") {
  app_data <- read_app_file(app_file)
  return(setNames(names(app_data$action_identifiers), app_data$action_identifiers))
}

#' Get action types for a specific sample
#'
#' @param sample_type A character string representing the type of the sample.
#' @param sample_file A character string indicating the path to the samples.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of action types or NULL if no actions are defined.
#' @examples 
#' get_sample_action_types("micronix")
#' @export
get_sample_action_types <- function(sample_type, sample_file = "samples.json", app_file = "app.json") {
  sample_data <- read_json_file(sample_file)
  app_data <- read_json_file(app_file)
  
  # Check if the sample_type exists in the samples data
  if (sample_type %in% names(sample_data$samples)) {
    action_ids <- sample_data$samples[[sample_type]]$actions
    # Map the action identifiers to their respective names using the app.json
    return(setNames(action_ids, app_data$action_identifiers[action_ids]))
  }
  
  return(NULL)
}

#' Get file types for a specific sample
#'
#' @param sample_type A character string representing the type of the sample.
#' @param sample_file A character string indicating the path to the samples.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of file types or a list with "NA" if not specified.
#' @examples 
#' get_file_types_for_sample("micronix")
#' @export
get_file_types_for_sample <- function(sample_type, sample_file = "samples.json", app_file = "app.json") {
  sample_data <- read_json_file(sample_file)
  app_data <- read_json_file(app_file)
  
  # Check if the sample_type exists in the samples data
  if (sample_type %in% names(sample_data$samples)) {
    file_type_ids <- sample_data$samples[[sample_type]]$file_types
    # Map the file type identifiers to their respective names using the app.json
    return(setNames(file_type_ids, app_data$file_type_identifiers[file_type_ids]))
  }
  
  return(list("NA" = "na"))
}

#' Get action types for a specific control
#'
#' @param control_type A character string representing the type of the control.
#' @param control_file A character string indicating the path to the controls.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of action types or NULL if no actions are defined.
#' @examples 
#' get_control_action_types("dbs_sheet")
#' @export
get_control_action_types <- function(control_type, control_file = "controls.json", app_file = "app.json") {
  control_data <- read_json_file(control_file)
  app_data <- read_json_file(app_file)
  
  # Check if the control_type exists in the controls data
  if (control_type %in% names(control_data$controls)) {
    action_ids <- control_data$controls[[control_type]]$actions
    # Map the action identifiers to their respective names using the app.json
    return(setNames(action_ids, app_data$action_identifiers[action_ids]))
  }
  
  return(NULL)
}


#' Get file types for a specific control
#'
#' @param control_type A character string representing the type of the control.
#' @param control_file A character string indicating the path to the controls.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of file types or a list with "NA" if not specified.
#' @examples 
#' get_file_types_for_control("dbs_sheet")
#' @export
get_file_types_for_control <- function(control_type, control_file = "controls.json", app_file = "app.json") {
  control_data <- read_json_file(control_file)
  app_data <- read_json_file(app_file)
  
  # Since controls.json doesn't explicitly have a file_types field for each control, we're assuming if there's no field, we should return the ID as the value.
  # Check if the control_type exists in the controls data
  if (control_type %in% names(control_data$controls)) {
    return(list(control_type = app_data$file_type_identifiers[control_type]))
  }
  
  return(list("NA" = "na"))
}

#' Get all sample types
#'
#' @param sample_file A character string indicating the path to the samples.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of sample types.
#' @examples 
#' get_all_sample_types()
#' @export
get_all_sample_types <- function(sample_file = "samples.json", app_file = "app.json") {
  sample_data <- read_json_file(sample_file)
  app_data <- read_json_file(app_file)
  
  sample_type_names <- sapply(sample_data$samples, function(x) x$name)
  # Using the names from samples.json as the list names and the types (ids) as the list values
  sample_type_ids <- names(sample_data$samples)
  
  return(setNames(sample_type_ids, sample_type_names))
}


#' Get all control types
#'
#' @param control_file A character string indicating the path to the controls.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of control types.
#' @examples 
#' get_all_control_types()
#' @export
get_all_control_types <- function(control_file = "controls.json", app_file = "app.json") {
  control_data <- read_json_file(control_file)
  app_data <- read_json_file(app_file)
  
  control_type_names <- sapply(control_data$controls, function(x) x$name)
  # Using the names from controls.json as the list names and the types (ids) as the list values
  control_type_ids <- names(control_data$controls)
  
  return(setNames(control_type_ids, control_type_names))
}



#' Create a ColumnData S3 object
#' 
#' This function serves as a constructor for the ColumnData S3 class.
#' It is used to represent column information in a structured manner.
#'
#' @param required A vector or NULL. Represents required columns.
#' @param conditional A vector or NULL. Represents conditional columns.
#' @param optional A vector or NULL. Represents optional columns.
#'
#' @return An S3 object of class 'ColumnData'.
ColumnData <- function(required = NULL, conditional = NULL, optional = NULL) {
  out <- list(
    required = required,
    conditional = conditional,
    optional = optional
  )
  class(out) <- "ColumnData"
  return(out)
}

#' Print method for ColumnData objects
#' 
#' This method provides a custom print representation for objects of class 'ColumnData'.
#' It displays the required, conditional, and optional columns separately.
#'
#' @param x An object of class 'ColumnData'.
#' @param ... Further arguments passed to or from other methods.
print.ColumnData <- function(x, ...) {
  cat("Required Columns:\n")
  print(x$required)
  cat("\nConditional Columns:\n")
  print(x$conditional)
  cat("\nOptional Columns:\n")
  print(x$optional)
}

#' Dereference Location and Container Keys
#' 
#' Helper function that dereferences 'destination_location' and 'destination_container' keys
#' in the provided keys list, using the 'app_data' structure.
#' 
#' @param keys A list containing the keys to be dereferenced.
#' @param app_data A list containing application data. This should have 'locations' and 'containers' lists within it.
#'
#' @return A vector containing dereferenced keys.
#' 
#' @noRd
dereference_location_container <- function(keys, app_data) {
  dereferenced <- c()

  if ("location_key" %in% names(keys) && !is.null(keys$location_key)) {
    location_keys <- app_data$locations[[keys$location]]
    dereferenced <- c(dereferenced, location_keys)
  }

  if ("container_key" %in% names(keys) && !is.null(keys$container_key)) {
    location_keys <- app_data$containers[[keys$container_key]]
    dereferenced <- c(dereferenced, location_keys)
  }
  
  if ("container_barcode_key" %in% names(keys) && !is.null(keys$container_barcode_key)) {
    location_keys <- app_data$container_barcodes[[keys$container_barcode_key]]
    dereferenced <- c(dereferenced, location_keys)
  }

  return(dereferenced)
}

#' Dereference Control Location and Container Keys
#' 
#' Helper function that dereferences 'extraction_storage_location' and 'storage_container' keys
#' in the provided control keys list, using the 'app_data' structure.
#' 
#' @param header_keys A list containing the control keys to be dereferenced.
#' @param app_data A list containing application data. This should have 'locations' and 'containers' lists within it.
#' @param action_keys A list containing the keys that are used in this action for this control.
#'
#' @return A vector containing dereferenced control keys.
#' 
#' @noRd
dereference_control_location_container <- function(header_keys, app_data, action_keys) {

  dereferenced <- c()
  keys <- header_keys[names(header_keys) %in% unlist(action_keys)]

  if ("storage_location" %in% names(keys) && !is.null(keys$storage_location)) {
    location_keys <- app_data$locations[[keys$storage_location]]
    dereferenced <- c(dereferenced, location_keys)
  }

  if ("extraction_target_location" %in% names(keys) && !is.null(keys$extraction_target_location)) {
    location_keys <- app_data$locations[[keys$extraction_target_location]]
    dereferenced <- c(dereferenced, location_keys)
  }
  
  if ("container_key" %in% names(keys) && !is.null(keys$container_key)) {
    container_keys <- app_data$containers[[keys$container_key]]
    dereferenced <- c(dereferenced, container_keys)
  }

  if ("extraction_target_container" %in% names(keys) && !is.null(keys$extraction_target_container)) {
    container_keys <- app_data$containers[[keys$extraction_target_container]]
    dereferenced <- c(dereferenced, container_keys)
  }
  
  return(dereferenced)
}


#' Get values with priority to sample_values over shared_values.
#'
#' This function prioritizes and fetches values from either sample_values or shared_values.
#'
#' @param keys List of keys for which values are needed.
#' @param sample_values List of sample values.
#' @param shared_values List of shared values.
#'
#' @return A vector of values prioritized from sample_values, if available.
get_values <- function(keys, sample_values, shared_values) {
  
  # Initialize an empty list to hold the final values
  final_values <- vector("character", length = 0)
  
  # Loop through each key
  for (key in keys) {
    
    # Prioritize sample_value if it exists, otherwise use shared_value
    if (key %in% names(sample_values) && all(sample_values[[key]] != "")) {
      final_values <- c(final_values, sample_values[[key]])
    } else if (key %in% names(shared_values) && all(shared_values[[key]] != "")) {
      final_values <- c(final_values, shared_values[[key]])
    }
  }
  
  # If final_values is empty, return NULL
  if (length(final_values) == 0) {
    return(NULL)
  }
  
  # Otherwise, return as a character vector (removes names)
  return(unname(final_values))
}

#' Get dereferenced values using sample_values and shared_values
#'
#' This function retrieves the values based on the keys provided.
#'
#' @param dereferenced_keys List of keys to look up.
#' @param sample_values List of primary values.
#' @param shared_values List of secondary values.
#'
#' @return A flattened list of values.
get_dereferenced_values <- function(dereferenced_keys, sample_values, shared_values) {

  # Flatten the sample_values for easier look-up
  flatten_list <- function(x) {
    if (!is.list(x)) return(list(x))
    else return(do.call(c, lapply(x, flatten_list)))
  }
  flat_sample_values <- flatten_list(sample_values)

  # Lookup function
  lookup_value <- function(key) {
    if (key %in% names(flat_sample_values)) {
      return(flat_sample_values[[key]])
    } else if (key %in% names(shared_values)) {
      return(shared_values[[key]])
    } else {
      return(key)  # Return the original key if not found in sample_values or shared_values
    }
  }

  # Return the looked up values for the dereferenced_keys
  return(unname(sapply(unlist(dereferenced_keys), lookup_value)))
}


#' Retrieve ColumnData for a given sample type and action.
#'
#' This function provides the ColumnData S3 object for a specific sample type and action.
#'
#' @param sample_type Type of the sample for which the columns are needed.
#' @param action Desired action.
#' @param file_type The file type being used. Default is 'na'.
#' @param config_yml (default: Sys.getenv("SDB_PATH")) Path to the application config file
#' @param samples_file (default: "samples.json") Path to the JSON file containing samples data.
#'
#' @return A ColumnData S3 object.
#' @export
get_sample_file_columns <- function(sample_type, action, file_type = "na", config_yml = Sys.getenv("SDB_CONFIG"), samples_file = "samples.json") {

  if (sample_type == "dbs_sample" && action == "move") {
    return(
      ColumnData(
        required = c("Label", "OldContainer", "OldContainerType", "NewContainer", "NewContainerType"),
        conditional = NULL,
        optional = NULL
      )
    )
  }

  sample_data <- read_json_file(samples_file)
  app_data <- read_app_file()
  app_config <- yaml::read_yaml(config_yml)
  
  # Ensure that sample type and action exists
  if (!sample_type %in% names(sample_data$samples)) {
    stop("Invalid sample type provided.")
  }
  if (!action %in% names(sample_data$action_requirements)) {
    stop("Invalid action provided.")
  }

  header_key <- sample_data$samples[[sample_type]]
  shared_action_keys <- sample_data$action_requirements[[action]]$shared
  action_keys <- sample_data$action_requirements[[action]][[sample_type]]
  shared_values <- sample_data$sample_key_associations$shared
  sample_values <- sample_data$sample_key_associations[[sample_type]]

  if (!is.null(file_type) && file_type != 'na') {
    sample_values <- sample_data$sample_key_associations[[sample_type]][[file_type]]
  }

  # Get the union of keys between both shared and sample specific keys
  required_keys_combined <- unique(c(action_keys$required_keys, shared_action_keys$required_keys))

  # Get locations and containers
  dereferenced_values <- dereference_location_container(header_key, app_data)

  # If there are key subsets, then only keep those otherwise keep everything
  common_keys <- intersect(names(dereferenced_values), required_keys_combined)
  dereferenced_values <- if (!purrr::is_empty(common_keys)) dereferenced_values[common_keys] else dereferenced_values

  # Overwrite the values in dereferenced_values with the ones from sample_values
  common_keys <- intersect(names(dereferenced_values), names(sample_values))
  dereferenced_values[common_keys] <- sample_values[common_keys]

  # NOTE: quick fix as this is always the case 
  required_dereferenced_values <- dereferenced_values
  required_dereferenced_values[["container_barcode_key"]] <- NULL
  
  # Include both dereferenced_values and sample_values in the required values
  required_vals <- unique(c( get_values(required_keys_combined, sample_values, shared_values),
                      unlist(unname(required_dereferenced_values)))
  )

  # Check for traxcer position override if file_type is 'traxcer'
  if (file_type == 'traxcer' && !is.null(app_config$traxcer_position$override) && !is.na(app_config$traxcer_position$override)) {
    required_vals[required_vals == "Position"] <- app_config$traxcer_position$override
  }


  ### Conditional values

  conditional_keys_combined <- c(action_keys$conditional_keys, shared_action_keys$conditional_keys)
  conditional_vals <- get_values(conditional_keys_combined, sample_values, shared_values)

  ### Optional values

  optional_keys_combined <- c(action_keys$optional_keys, shared_action_keys$optional_keys)
  dereferenced_values <- dereference_location_container(header_key, app_data)

  common_keys <- intersect(names(dereferenced_values), optional_keys_combined)
  dereferenced_values <- if (!purrr::is_empty(common_keys)) dereferenced_values[common_keys] else dereferenced_values

  # If there are key subsets, then only keep those otherwise keep everything
  optional_vals <- get_values(optional_keys_combined, sample_values, shared_values)

  # NOTE: quick fix
  optional_vals <- c(optional_vals, unlist(unname(dereferenced_values[names(dereferenced_values) %in% optional_keys_combined])))

  # Cleaning up null values
  required_vals <- required_vals[!is.null(required_vals)]
  conditional_vals <- conditional_vals[!is.null(conditional_vals)]
  optional_vals <- optional_vals[!is.null(optional_vals)]

  if (sample_type == "dbs_sample") {
    required_vals <- c(required_vals, "Label", "ContainerName", "ContainerType")
  }

  return(ColumnData(
    required = required_vals,
    conditional = conditional_vals,
    optional = optional_vals
  ))
}

#' Retrieve ColumnData for a given control type and action
#'
#' This function provides the ColumnData S3 object for a specific control type and action.
#'
#' @param control_type Type of the control for which the columns are needed.
#' @param action Desired action.
#' @param file_type The file type being used. Default is 'na'.
#' @param config_yml (default: Sys.getenv("SDB_CONFIG")) Path to the application config file.
#' @param controls_file (default: "controls.json") Path to the JSON file containing controls data.
#'
#' @return A ColumnData S3 object.
#' @export
get_control_file_columns <- function(control_type, action, file_type = "na", config_yml = Sys.getenv("SDB_CONFIG"), controls_file = "controls.json") {

  if (control_type == "dbs_sheet" && action == "move") {
    return(
      ColumnData(
        required = c("Batch", "ControlUID", "SheetName", "SourceBagName", "DestBagName"), # Require the ControlUID as well because these sheets have the same names.
        conditional = NULL,
        optional = NULL
      )
    )
  } else if (control_type == "whole_blood" && action == "move") {
    return(
      ColumnData(
        required = c("Batch", "ControlUID", "BoxRow", "BoxColumn", "SourceBox"),
        conditional = NULL,
        optional = NULL
      )
    )
  }

  ## NOTE: update the below to follow the pattern above.

  control_data <- read_json_file(controls_file)
  app_data <- read_app_file()
  app_config <- yaml::read_yaml(config_yml)
  
  # Ensure that control type and action exists
  if (!control_type %in% names(control_data$controls)) {
    stop("Invalid control type provided.")
  }
  if (!action %in% names(control_data$action_requirements)) {
    stop("Invalid action provided.")
  }

  header_key <- control_data$controls[[control_type]]
  shared_action_keys <- control_data$action_requirements[[action]]$shared
  action_keys <- control_data$action_requirements[[action]][[control_type]]
  shared_values <- control_data$control_key_associations$shared
  control_values <- control_data$control_key_associations[[control_type]]

  if (!is.null(file_type) && file_type != 'na') {
    control_values <- control_data$control_key_associations[[control_type]][[file_type]]
  }

  # Dereference logic
  required_keys_combined <- unique(c(action_keys$required_keys, shared_action_keys$required_keys))
  dereferenced_values <- dereference_control_location_container(header_key, app_data, required_keys_combined)

  # Keep only the common keys
  common_keys <- intersect(names(dereferenced_values), required_keys_combined)
  dereferenced_values <- if (!purrr::is_empty(common_keys)) dereferenced_values[common_keys] else dereferenced_values
  
  # Overwrite logic
  common_keys <- intersect(names(dereferenced_values), names(control_values))
  dereferenced_values[common_keys] <- control_values[common_keys]

  # NOTE: quick fix as this is always the case 
  required_dereferenced_values <- dereferenced_values
  required_dereferenced_values[["container_barcode_key"]] <- NULL

  required_vals <- unique(c( get_values(required_keys_combined, control_values, shared_values),
                      unlist(unname(required_dereferenced_values)))
  )
  
  conditional_keys_combined <- c(action_keys$conditional_keys, shared_action_keys$conditional_keys)
  conditional_vals <- get_values(conditional_keys_combined, control_values, shared_values)

  ### Optional values

  optional_keys_combined <- c(action_keys$optional_keys, shared_action_keys$optional_keys)
  dereferenced_values <- dereference_location_container(header_key, app_data)

  common_keys <- intersect(names(dereferenced_values), optional_keys_combined)
  dereferenced_values <- if (!purrr::is_empty(common_keys)) dereferenced_values[common_keys] else dereferenced_values

  # If there are key subsets, then only keep those otherwise keep everything
  optional_vals <- get_values(optional_keys_combined, control_values, shared_values)

  # NOTE: quick fix
  optional_vals <- c(optional_vals, unlist(unname(dereferenced_values[names(dereferenced_values) %in% optional_keys_combined])))
  if (action=="extraction") {
    optional_vals <- c(optional_vals, "PlateBarcode")
  }

  if (control_type == "dbs_sheet") {
    required_vals <- c(required_vals, "BagName") # Just hardcode this for now
  }

  # Cleaning up null values
  required_vals <- required_vals[!is.null(required_vals)]
  conditional_vals <- conditional_vals[!is.null(conditional_vals)]
  optional_vals <- optional_vals[!is.null(optional_vals)]


  return(ColumnData(
    required = required_vals,
    conditional = conditional_vals,
    optional = optional_vals
  ))
}



#' Retrieve ColumnData for a given reference type.
#'
#' This function provides the ColumnData S3 object for a specific reference type.
#'
#' @param reference_type Type of the reference for which the columns are needed.
#' @param config_yml (default: Sys.getenv("SDB_CONFIG")) Path to the application config file
#' @param references_file (default: "references.json") Path to the JSON file containing references data.
#'
#' @return A ColumnData S3 object.
#' @export
get_reference_file_columns <- function(reference_type, config_yml = Sys.getenv("SDB_CONFIG"), references_file = "references.json") {
  
  reference_data <- read_json_file(references_file)
  app_config <- yaml::read_yaml(config_yml)
  
  # Ensure that reference type exists
  if (!reference_type %in% names(reference_data$references)) {
    stop("Invalid reference type provided.")
  }

  header_key <- reference_data$references[[reference_type]]
  reference_keys <- header_key$required_keys
  shared_values <- reference_data$reference_key_associations$shared
  reference_values <- reference_data$reference_key_associations[[reference_type]]
  
  required_vals <- get_values(reference_keys, reference_values, shared_values)
  
  optional_vals <- NULL
  if ("optional_keys" %in% names(header_key)) {
    optional_vals <- get_values(header_key$optional_keys, reference_values, shared_values)
  }
  
  # Cleaning up null values
  required_vals <- required_vals[!is.null(required_vals)]
  if (!is.null(optional_vals)) {
    optional_vals <- optional_vals[!is.null(optional_vals)]
  }

  return(ColumnData(
    required = required_vals,
    optional = optional_vals
  ))
}



#' Get locations by specified sample type
#'
#' @param sample_type A character string specifying the type of sample.
#' @param sample_file A character string indicating the path to the samples.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of locations for the specified sample type.
#' @examples 
#' get_location_by_sample(sample_type = "micronix")
#' @export
get_location_by_sample <- function(sample_type, sample_file = "samples.json", app_file = "app.json") {
  sample_data <- read_json_file(sample_file)
  app_data <- read_json_file(app_file)
  
  # Check if sample type exists
  if(!sample_type %in% names(sample_data$samples)) {
    stop("Specified sample type not found.")
  }
  
  location_keys <- sample_data$samples[[sample_type]]$location_key
  # If there's only one location, make it a list for consistency
  if (!is.list(location_keys)) {
    location_keys <- list(location_keys)
  }
  
  # Fetch the complete location data from app_data using the keys
  location_data <- lapply(location_keys, function(loc) {
    data <- app_data$locations[[loc]]
    list(location_root = data$location_root, level_i = data$level_i, level_ii = data$level_ii)
  })
  
  return(location_data[[1]])
}

#' Get source location by specified control type
#'
#' @param control_type A character string specifying the type of control.
#' @param control_file A character string indicating the path to the controls.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of extraction locations for the specified control type.
#' @examples 
#' get_extraction_location_by_control(control_type = "DBS Sheet")
#' @export
get_storage_location_by_control <- function(control_type, control_file = "controls.json", app_file = "app.json") {
  
  # Read the data
  control_data <- read_json_file(control_file)
  app_data <- read_json_file(app_file)
  
  # Check if control type exists
  if(!control_type %in% names(control_data$controls)) {
    stop("Specified control type not found.")
  }
  
  # Extract the source location key
  location_source_key <- control_data$controls[[control_type]]$extraction_storage_location
  
  # Fetch the complete location data from app_data using the source key
  location_source_data <- lapply(location_source_key, function(loc) {
    data <- app_data$locations[[loc]]
    list(location_root = data$location_root, level_i = data$level_i, level_ii = data$level_ii)
  })

  return(location_source_data[[1]])
}

#' Get destination location by specified control type
#'
#' @param control_type A character string specifying the type of control.
#' @param control_file A character string indicating the path to the controls.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of destination locations for the specified control type.
#' @examples 
#' get_destination_location_by_control(control_type = "DBS Sheet")
#' @export
get_destination_location_by_control <- function(control_type, control_file = "controls.json", app_file = "app.json") {
  
  # Read the data
  control_data <- read_json_file(control_file)
  app_data <- read_json_file(app_file)
  
  # Check if control type exists
  if(!control_type %in% names(control_data$controls)) {
    stop("Specified control type not found.")
  }
  
  # Extract the destination location key
  location_destination_key <- control_data$controls[[control_type]]$extraction_target_location
  
  # Fetch the complete location data from app_data using the destination key
  location_destination_data <- lapply(location_destination_key, function(loc) {
    data <- app_data$locations[[loc]]
    list(location_root = data$location_root, level_i = data$level_i, level_ii = data$level_ii)
  })

  return(location_destination_data[[1]])
}

#' Get containers by specified sample type
#'
#' @param sample_type A character string specifying the type of sample.
#' @param sample_file A character string indicating the path to the samples.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of containers for the specified sample type.
#' @examples 
#' get_container_by_sample(sample_type = "micronix")
#' @export
get_container_by_sample <- function(sample_type, sample_file = "samples.json", app_file = "app.json") {
  sample_data <- read_json_file(sample_file)
  app_data <- read_json_file(app_file)
  
  # Check if sample type exists
  if(!sample_type %in% names(sample_data$samples)) {
    stop("Specified sample type not found.")
  }
  
  container_keys <- sample_data$samples[[sample_type]]$container_key
  # If there's only one container, make it a list for consistency
  if (!is.list(container_keys)) {
    container_keys <- list(container_keys)
  }

  if (is.null(container_keys[[1]])) {
    errmsg <- sprintf("Container keys are not defined for %s", sample_type)
    stop(errmsg)
  }
  
  # Fetch the complete container data from app_data using the keys
  container_data <- lapply(container_keys, function(cont) {
    data <- app_data$containers[[cont]]
    list(position_keys = list(data$position_keys), container_name_key = data$container_name_key, container_barcode_key = data$container_barcode_key)
  })
  
  return(container_data[[1]])
}

#' Get containers by specified sample type
#'
#' @param control_type A character string specifying the type of sample.
#' @return A named list of containers for the specified sample type.
#' @examples 
#' get_container_by_sample(sample_type = "micronix")
#' @export
get_container_by_control <- function(control_type) {
  if (control_type == "dbs_sheet") {
    return (list(container_name_key = "BagName"))
  } else if (control_type == "whole_blood") {
    return (list(container_name_key = "BoxName"))
  } else {
    stop("Unrecognized control type")
  }
}


#' Get source container by specified control type
#'
#' @param control_type A character string specifying the type of control.
#' @param control_file A character string indicating the path to the controls.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of extraction containers for the specified control type.
#' @examples 
#' get_extraction_container_by_control(control_type = "DBS Sheet")
#' @export
get_storage_container_by_control <- function(control_type, control_file = "controls.json", app_file = "app.json") {

  # Read the data
  control_data <- read_json_file(control_file)
  app_data <- read_json_file(app_file)
  
  # Check if control type exists
  if(!control_type %in% names(control_data$controls)) {
    stop("Specified control type not found.")
  }
  
  # Extract the source container key
  container_key <- control_data$controls[[control_type]]$container_key
  
  # Fetch the complete container data from app_data using the source key
  container_source_data <- lapply(container_key, function(cont) {
    data <- app_data$containers[[cont]]
    list(position_keys = list(data$position_keys), container_name_key = data$container_name_key, container_barcode_key = data$container_barcode_key)
  })

  return(container_source_data[[1]])
}


#' Get destination container by specified control type
#'
#' @param control_type A character string specifying the type of control.
#' @param control_file A character string indicating the path to the controls.json file.
#' @param app_file A character string indicating the path to the app.json file.
#' @return A named list of destination containers for the specified control type.
#' @examples 
#' get_destination_container_by_control(control_type = "DBS Sheet")
#' @export
get_destination_container_by_control <- function(control_type, control_file = "controls.json", app_file = "app.json") {
  # Read the data

  control_data <- read_json_file(control_file)
  app_data <- read_json_file(app_file)
  
  # Check if control type exists
  if(!control_type %in% names(control_data$controls)) {
    stop("Specified control type not found.")
  }
  
  # Extract the destination container key
  container_destination_key <- control_data$controls[[control_type]]$extraction_target_container
  
  # Fetch the complete container data from app_data using the destination key
  container_destination_data <- lapply(container_destination_key, function(cont) {
    data <- app_data$containers[[cont]]
    list(position_keys = list(data$position_keys), container_name_key = data$container_name_key, container_barcode_key = data$container_barcode_key)
  })

  return(container_destination_data[[1]])
}


#' Get the expected position column for a sample type
#' 
#' Micronix have different file formats which report sample positioning in different ways. This
#' function makes it easy to look up that information. 
#' 
#' @param sample_type A character string specifying the type of sample.
#' @param file_type The file type (default: "na")
#' @param sample_file A character string indicating the path to the samples.json file. (default: "samples.json")
#' @param control_file A character string specifying the path to the controls.json file. (default: "app.json")
#' @return The position column for that sample type.
#' @export
get_position_column_by_sample <- function(sample_type, file_type = "na", sample_file = "samples.json", app_file = "app.json") {
  
  sample_data <- read_json_file(sample_file)
  app_data <- read_json_file(app_file)

   # Check if control type exists
  if(!sample_type %in% names(sample_data$samples)) {
    stop("Specified sample type not found.")
  }

  # Get the sample type header with all application data keys for lookup
  header_key <- sample_data$samples[[sample_type]]

  # Get the container key
  container_key <- header_key[["container_key"]]

  # If there are file specific overrides, find them here
  overrides <- sample_data[["sample_key_associations"]][[sample_type]][[file_type]]

  override_position <- NULL
  if (!is.null(overrides)) {
    override_position <- overrides[["position_keys"]]
  }

  expected_columns <- if(!is.null(override_position)) override_position else app_data[["containers"]][[container_key]][['position_keys']]

  return(expected_columns)
}