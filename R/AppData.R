# R/AppData.R

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
get_data_file_path <- function(file_name) {
  system.file("app/www", file_name, package = "sampleDB")
}

#' Merge Default Values with Exceptions
#'
#' This internal function is used to combine a default set of key-value pairs with
#' an exceptions set, where the exceptions will override the defaults.
#'
#' @param defaults A named list of default values.
#' @param exceptions A named list of exceptions that will override the defaults.
#'
#' @return A merged list with exceptions overriding the defaults.
#'
#' @examples
#' defaults <- list(a = 1, b = 2, c = 3)
#' exceptions <- list(b = 20, d = 4)
#' merge_defaults_with_exceptions(defaults, exceptions)
#'
#' @keywords internal
merge_defaults_with_exceptions <- function(defaults, exceptions) {
  if (is.null(exceptions)) {
    return(defaults)
  }
  
  for (key in names(exceptions)) {
    if (is.list(exceptions[[key]]) && !is.null(defaults[[key]]) && is.list(defaults[[key]])) {
      defaults[[key]] <- merge_defaults_with_exceptions(defaults[[key]], exceptions[[key]])
    } else {
      defaults[[key]] <- exceptions[[key]]
    }
  }
  return(defaults)
}



#' Get fields for specific action, sample type, and file type
#'
#' Extracts the necessary fields for a given action, sample type, and file type from parsed JSON data.
#' The function considers default configurations and also allows for specific exceptions
#' based on sample and file types. It has built-in checks to ensure valid input values
#' for `sample_type`, `action`, and `file_type`.
#'
#' @param json_data A list. The parsed JSON data containing sample details and defaults.
#' @param references_json_data A list. Parsed references JSON data with details about locations, containers, etc.
#' @param sample_type A character string specifying the type of sample (e.g., "micronix").
#' @param action A character string indicating the desired action (e.g., "upload").
#' @param file_type A character string defining the type of the file (e.g., "visionmate"). The default is "na" (application standard).
#' @return A list containing fields for the specified sample type, action, and file type.
#'         This includes required, conditional, and optional fields, alongside container and location information.
#' @examples
#' \dontrun{
#' json_data <- load_parse_json("path/to/your/samples.json")
#' references_data <- load_parse_json("path/to/your/references.json")
#' fields <- get_fields_for_action_sample(json_data, references_data, "Micronix", "upload", "visionmate")
#' }
#' @seealso \link[jsonlite]{fromJSON} for parsing JSON in R.
#' @keywords internal
#' @export
get_fields_for_action_sample <- function(json_data, references_json_data, sample_type, action, file_type = "na") {

  # Ensure non-null parameters
  if (is.null(sample_type)) {
    stop("Sample type cannot be NULL")
  }

  if (is.null(action)) {
    stop("Action cannot be NULL")
  }

  if (is.null(file_type)) {
    stop("File type cannot be NULL")
  }

  # Get the sample type data
  sample_type_data <- json_data$samples

  # Check if the provided sample_type is valid
  valid_sample_types <- sapply(sample_type_data, function(x) x$id)

  if (!(sample_type %in% valid_sample_types)) {
    stop(paste("Invalid sample type:", sample_type))
  }

  # Check if the provided action is valid
  valid_actions <- names(json_data$defaults)
  if (!(action %in% valid_actions)) {
    stop(paste("Invalid action:", action))
  }

  # Find the matching sample type
  for (sample in sample_type_data) {
    if (sample$id == sample_type) {

      # Check if the provided file_type is valid for the current sample_type
      if (file_type != "na" && !is.null(sample$exceptions) && !(file_type %in% names(sample$exceptions))) {
        stop(paste("Invalid file type:", file_type, "for sample type:", sample_type))
      }

      # Get the fields for the specific action and file type
      if (file_type == "na") {
        # Use defaults for 'na' file type
        fields <- json_data$defaults[[action]]
      } else {
        fields <- merge_defaults_with_exceptions(json_data$defaults[[action]], sample$exceptions[[file_type]])
      }

      # Append container and location details from the root and sample level
      fields$container <- sample$container %||% json_data$defaults$container
      fields$location <- sample$location %||% json_data$defaults$location

      # If a specific container is mentioned in exceptions, it takes precedence over defaults
      if (!is.null(sample$exceptions[[file_type]]$container)) {
        fields$container <- sample$exceptions[[file_type]]$container
      } else {
        fields$container <- references_json_data$containers[[fields$container]]
      }

      # Fetch location details from references
      fields$location <- references_json_data$locations[[fields$location]]

      return(fields)
    }
  }

  # If the sample type is not found, return NULL
  return(NULL)
}


#' Get fields for specific action, control type, and file type
#'
#' This function extracts the required, conditional, and optional fields for a specific action,
#' control type from the parsed JSON data. It also takes into account exceptions for the given control type.
#'
#' @param json_data A list containing the parsed JSON data.
#' @param references_json_data A list containing the parsed references JSON data.
#' @param control_type The type of the control.
#' @param action The type of the action.
#' @return A list containing the required, conditional, and optional fields.
#' @examples
#' \dontrun{
#' json_data <- load_parse_json("path/to/your/file.json")
#' fields <- get_fields_for_action_control(json_data, "Micronix", "create")
#' }
#' @keywords internal
#' @export
get_fields_for_action_control <- function(json_data, references_json_data, control_type, action) {

  # Ensure non-null parameters
  if (is.null(control_type)) {
    stop("Control type cannot be NULL")
  }

  if (is.null(action)) {
    stop("Action cannot be NULL")
  }

  # Get the control type data
  control_type_data <- json_data$controls

  # Check if the provided control_type is valid
  valid_control_types <- sapply(control_type_data, function(x) x$id)
  if (!(control_type %in% valid_control_types)) {
    stop(paste("Invalid control type:", control_type))
  }

  # Check if the provided action is valid
  valid_actions <- names(json_data$defaults)
  if (!(action %in% valid_actions)) {
    stop(paste("Invalid action:", action))
  }

  # Find the matching control type
  for (control in control_type_data) {
    if (control$id == control_type) {

      # Get the fields for the specific action
      fields <- merge_defaults_with_exceptions(json_data$defaults[[action]], control$exceptions[[action]])

      # Set NULL values to NA for consistency
      fields <- lapply(fields, function(x) if(is.null(x)) NA else x)

      # Extract location and container details
      if (!is.null(fields$loc_src) && !is.na(fields$loc_src)) {
        fields$loc_src <- references_json_data$locations[[fields$loc_src]]
      }

      if (!is.null(fields$loc_dest) && !is.na(fields$loc_dest)) {
        fields$loc_dest <- references_json_data$locations[[fields$loc_dest]]
      }

      if (!is.null(fields$container_src) && !is.na(fields$container_src)) {
        fields$container_src <- references_json_data$containers[[fields$container_src]]
      }

      if (!is.null(fields$container_dest) && !is.na(fields$container_dest)) {
        fields$container_dest <- references_json_data$containers[[fields$container_dest]]
      }

      return(fields)
    }
  }

  # If the control type is not found, return NULL
  return(NULL)
}

#' Get fields for specific action, sample type, and file type
#'
#' This function extracts the required,
#' conditional, and optional fields for a specific action,
#' sample type, and file type from the parsed JSON data.
#'
#' @param json_data A list containing the parsed JSON data.
#' @param sample_type The type of the sample.
#' @param action The type of the action.
#' @param file_type The type of the file.
#' @return A list containing the required, conditional, and optional fields.
#' @examples
#' \dontrun{
#' json_data <- load_parse_json("path/to/your/file.json")
#' fields <- get_fields_for_action(json_data, "Micronix", "upload", "na")
#' }
#' @keywords internal
#' @export
get_fields_for_action <- function(json_data, sample_type, action, file_type) {
  # Get the sample type data
  sample_type_data <- json_data$samples

  # Find the matching sample type
  for (sample in sample_type_data) {
    if (sample$name == sample_type) {
      # Get the fields for the specific action and file type
      fields <- sample$actions[[action]]$file_types[[file_type]]
      return(fields)
    }
  }

  # If the sample type is not found, return NULL
  return(NULL)
}

#' Load and parse JSON file
#'
#' This function reads a JSON file and converts it into a list in R.
#' The list can then be manipulated using standard R operations.
#'
#' @param file_path The path to the JSON file.
#' @return A list containing the parsed JSON data.
#' @examples
#' \dontrun{
#' json_data <- load_parse_json("path/to/your/file.json")
#' }
#' @keywords internal
#' @export
load_parse_json <- function(file_path) {
  # Use jsonlite::fromJSON() to parse the JSON file
  json_data <- jsonlite::fromJSON(get_data_file_path(file_path), simplifyVector = FALSE)

  return(json_data)
}

#' Load and parse samples JSON
#'
#' This function loads and parses the samples.json file,
#' extracting the fields for a specific sample type, action,
#' and file type. It also takes into account exceptions for the given sample and file types.
#'
#' @param file_path The path to the JSON file.
#' @param references_json_path The path to the references JSON file.
#' @param sample_type The type of sample.
#' @param action The action to perform on the sample.
#' @param file_type The type of file.
#' @return A list with the required, conditional, and optional fields.
#' @examples
#' \dontrun{
#' fields <- get_fields_samples_json("Micronix", "upload", "na")
#' }
#' @keywords internal
#' @export
get_fields_samples_json <- function(file_path, references_json_path, sample_type, action, file_type) {
  json_data <- load_parse_json(file_path)
  references_json_data <- load_parse_json(references_json_path)
  fields <- get_fields_for_action_sample(json_data, references_json_data, sample_type, action, file_type)
  return(fields)
}

#' Load and parse controls JSON
#'
#' This function loads and parses the controls.json file.
#' The list can then be manipulated using standard R operations.
#'
#' @param file_path The path to the JSON file.
#' @param references_json_path The path to the references JSON file.
#' @param control_type The type of control.
#' @param action The action to perform on the control.
#' @return A list with the required and optional fields.
#' @examples
#' \dontrun{
#' fields <- get_fields_controls_json("dbs_sheet", "create")
#' }
#' @keywords intenal
#' @export
get_fields_controls_json <- function(file_path, references_json_path, control_type, action) {
  json_data <- load_parse_json(file_path)
  references_json_data <- load_parse_json(references_json_path)
  fields <- get_fields_for_action_control(json_data, references_json_data, control_type, action)
  return(fields)
}

#' Load and parse references JSON
#'
#' This function loads and parses the references.json file.
#' The list can then be manipulated using standard R operations.
#'
#' @param file_path Path to the references.json file.
#' @param category The category of references.
#' @param type The specific type within the category (optional).
#' @return The fields for the given category and type.
#' @examples
#' \dontrun{
#' fields <- get_fields_references_json("references.json", "locations", "minus20")
#' }
#' @keywords internal
#' @export
get_fields_references_json <- function(file_path, category, type = NULL) {

  # Load and parse the JSON data
  json_data <- load_parse_json(file_path)

  # Retrieve the fields for the given category
  fields <- json_data[[category]]
  if (!is.null(type)) {
    fields <- fields[[type]]
  }

  return(fields)
}



#' Get fields from JSON file
#'
#' This function fetches the fields from a given JSON file based on the file name and additional parameters
#'
#' @param appdata The name of JSON file. Must be one of "samples", "controls", or "references"
#' @param ... Additional parameters required by the specific JSON parsing functions
#' @return A list with the required, conditional, and optional fields (for "samples" and "controls") or the fields for the given category (for "references")
#' @keywords internal
#' @export
get_fields_from_json <- function(appdata, ...) {
  if (appdata == "samples") {
    return(do.call(get_fields_samples_json, list("samples.json", "references.json", ...)))
  } else if (appdata == "controls") {
    return(do.call(get_fields_controls_json, list("controls.json", "references.json", ...)))
  } else if (appdata == "references") {
    return(do.call(get_fields_references_json, list("references.json", ...)))
  } else {
    stop(sprintf("No appdata found for %s.", appdata))
  }
}

#' This is an R6 Class for the FileColumnAttributes object
#'
#' @noRd
FileColumnAttributes <- R6::R6Class(
  "FileColumnAttributes",
  public = list(
    required = NULL,
    conditional = NULL,
    optional = NULL,
    location = NULL,
    container = NULL,
    initialize = function(required, conditional, optional, location, container) {
      self$required <- required
      self$conditional <- conditional
      self$optional <- optional
      self$location <- location
      self$container <- container
    },
    all_fields = function() {
      return (c(self$required, self$conditional, self$optional, self$location, self$container))
    },
    get_required_colnames = function() {
      return(unname(unlist(self$required)))
    },
    get_conditional_colnames = function() {
      return(unname(unlist(self$conditional)))
    },
    get_optional_colnames = function() {
      return(unname(unlist(self$optional)))
    },
    get_location_colnames = function() {
      return(unname(unlist(self$location)))
    },
    get_container_colnames = function() {
      return(unname(unlist(self$container)))
    }
  )
)

#' Get user column attributes for samples
#'
#' This function gets all the required, conditional, and optional fields
#' for a given user action. It also takes into account exceptions for the given sample and file types.
#'
#' @param sample_type The type of sample
#' @param user_action The user action to get fields for
#' @param file_type The type of file
#' @param config_yml The path to the config.yml file
#' @return A FileColumnAttributes object with required, conditional, and optional fields for the user action
#' @keywords appdata
#' @export
get_sample_file_columns <- function(sample_type, user_action, file_type, config_yml = Sys.getenv("SDB_CONFIG")) {
  ## Get fields for the action - order matters here
  fields <- get_fields_from_json("samples", sample_type, user_action, file_type)

  ## Initialize the output fields
  container_fields <- fields[['container']]

  ## If the file type is traxcer, replace with the custom config value
  if (file_type == "traxcer") {
    ## Read Configuration File and replace with user override from user preferences
    config <- yaml::read_yaml(config_yml)
    traxcer_position <- ifelse(
      !is.na(config$traxcer_position$override),
      config$traxcer_position$override,
      config$traxcer_position$default
    )

    if (!is.na(config$traxcer_position$override)) {
      replaced_values <- sapply(container_fields, function(value) {
        stringr::str_replace(value, config$traxcer_position$default, config$traxcer_position$override)
      })
      container_fields <- as.list(replaced_values)
    }
  }

  ## Create an R6 object of the FileColumnAttributes class
  file_column_attr <- FileColumnAttributes$new(
    required = fields[['required']],
    conditional = fields[['conditional']],
    optional = fields[['optional']],
    location = fields[['location']],
    container = container_fields
  )

  return(file_column_attr)
}

#' Get user action fields for controls
#'
#' This function gets all the fields for a given user action related to controls.
#'
#' @param control_type The type of control
#' @param user_action The user action to get fields for
#' @return A FileColumnAttributes object with fields for the user action for controls
#' @keywords appdata
#' @export
get_control_file_columns <- function(control_type, user_action) {
  ## Get fields for the action
  fields <- get_fields_from_json("controls", control_type, user_action)

  ## Initialize the output fields
  location <- c(fields["loc_src"], fields["loc_dest"])
  container <- c(fields["container_src"], fields["container_dest"])

  ## Create an R6 object of the FileColumnAttributes class
  file_column_attr <- FileColumnAttributes$new(
    required = fields[["required"]],
    conditional = fields[["conditional"]],
    optional = fields[["optional"]],
    location = location[!is.na(names(location))],
    container = container[!is.na(names(container))]
  )

  return(file_column_attr)
}

#' Retrieve File Column Attributes for a Given Category and Reference Type
#'
#' This function gets all the fields for a given user action related to references 
#' and returns a `FileColumnAttributes` object populated with those fields. 
#' It supports different reference categories in the JSON, such as locations, strains, compositions, etc. 
#' Depending on the category and the reference type (if provided), 
#' the function will fetch fields categorized as `required`, `optional`, or directly if no sub-categories are present.
#'
#' @param category A string specifying the category of reference, e.g., "locations", "strains".
#' @param reference_type An optional string specifying the specific type within the category, e.g., "minus20". Default is NULL.
#' 
#' @return A `FileColumnAttributes` object populated with the fields related to the given category and reference type.
#' 
#' @examples
#' \dontrun{
#' # For categories with a specific type
#' attributes <- get_reference_file_columns("locations", "minus20")
#'
#' # For categories without a specific type
#' attributes <- get_reference_file_columns("strains")
#' }
#' 
#' @export
get_reference_file_columns <- function(category, reference_type = NULL) {
  # Get fields for the action
  fields <- get_fields_from_json("references", category, reference_type)

  # Initialize the output fields
  required_column_names <- NULL
  conditional_column_names <- NULL
  optional_column_names <- NULL

  # If fields for the type are sub-categorized as 'required', 'optional', etc.
  if ('required' %in% names(fields)) {
    required_column_names <- fields[['required']]
  }
  
  if ('conditional' %in% names(fields)) {
    conditional_column_names <- fields[['conditional']]
  }
  
  if ('optional' %in% names(fields)) {
    optional_column_names <- fields[['optional']]
  }

  # If no sub-categories, then assign fields directly
  if (is.null(required_column_names) && is.null(conditional_column_names) && is.null(optional_column_names)) {
    required_column_names <- fields
  }

  # Create an R6 object of the FileColumnAttributes class
  file_column_attr <- FileColumnAttributes$new(
    required = required_column_names,
    conditional = conditional_column_names,
    optional = optional_column_names,
    location = NULL,   
    container = NULL   
  )

  return(file_column_attr)
}


#' Extract Actions, File Types, and Sample Types from JSON Content
#'
#' This function parses a given JSON structure to extract the available actions,
#' file types, and sample types.
#'
#' @param json_content A list containing the parsed JSON data.
#' @return A list containing three vectors: actions, file_types, and sample_types.
#' @examples
#' \dontrun{
#'   json_path <- "path_to_your_json_file.json"
#'   json_content <- fromJSON(json_path)
#'   info <- extract_info_from_json(json_content)
#'   print(info)
#' }
#' @keywords appdata
#' @export
extract_sample_from_json <- function(json_content) {

  # Extract actions from the 'defaults' section
  actions <- names(json_content$defaults)

  # Extract file types and sample types from the 'samples' section
  file_types <- unique(unlist(lapply(json_content$samples, function(sample) {
    return(names(sample$exceptions))
  })))

  sample_types <- unlist(lapply(json_content$samples, function(sample) {
    return(sample$id)
  }))

  # Return as a list
  return(list(actions = actions, file_types = file_types, sample_types = sample_types))
}

#' Extract sample IDs from JSON
#'
#' This function loads the provided JSON and returns the sample IDs from the "samples" section.
#'
#' @param file_path The path to the JSON file.
#' @return A named list where each item's name is a capitalized version of the sample ID, 
#'   and the item's value is the original sample ID.
#'
#' @examples
#' \dontrun{
#' sample_ids <- get_sample_ids_from_json("your_json_file_name.json")
#' print(sample_ids)
#' }
#' @importFrom jsonlite fromJSON
#' @seealso \code{\link{load_parse_json}}
#' @keywords internal
#' @export
get_sample_ids_from_json <- function(file_path="samples.json") {
  # Load the JSON data
  json_data <- load_parse_json(file_path)
  
  # Extract sample IDs from the "samples" section
  sample_ids <- sapply(json_data$samples, function(sample) sample$id)
  
  # Explicit naming
  explicit_names <- c("Micronix" = "micronix", "Cryovial" = "cryovial")
  names(sample_ids) <- c("Micronix", "Cryovial")
  
  return(sample_ids)
}


#' Retrieve File Types for Each Sample from JSON
#'
#' This function extracts the available file types for each sample defined in the provided JSON file.
#' "NA" will always be returned as one of the file types for each sample.
#'
#' @param json_path A character string representing the path to the JSON file.
#'
#' @return A named list where each item's name is a sample ID, and the item's value is a named vector 
#'   of file types available for that sample, with the name being the capitalized version and the value
#'   being the original file type. Each vector always includes "NA" as one of the options.
#'
#' @examples
#' \dontrun{
#' file_types <- get_sample_file_types("samples.json")
#' print(file_types)
#' }
#'
#' @importFrom jsonlite fromJSON
#' @seealso \code{\link{load_parse_json}}
#' @export
get_sample_file_types <- function(json_path="samples.json") {
  # Load the JSON data
  json_data <- load_parse_json(json_path)
  
  # Extract sample data
  sample_data <- json_data$samples
  
  # For each sample, extract its file types
  file_types_list <- lapply(sample_data, function(sample) {
    # Get the names of exceptions as they represent file types
    exception_types <- names(sample$exceptions)
    
    # Always include 'na' as an option
    file_types <- c("na", exception_types)
    
    # Create named version
    names(file_types) <- sapply(file_types, function(ft) if(ft == "na") "NA" else tools::toTitleCase(ft))
    
    return(file_types)
  })
  
  # Convert list to named vector
  names_vec <- sapply(sample_data, function(x) x$id)
  names(file_types_list) <- names_vec
  
  return(file_types_list)
}
