#' Setup a directory if it does not exist.
#'
#' @param directory The path to the directory to be created.
#'
#' @return Returns TRUE if the directory is created, FALSE otherwise.
#' @export
#'
#' @examples
#' setup_directory("new_directory_path")
#'
setup_directory <- function(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory)
    Sys.chmod(directory, mode = "0777", use_umask = FALSE)
    return(TRUE)
  }
  return(FALSE)
}

#' Copy a file to a destination if it does not exist there.
#'
#' @param src The source file path.
#' @param dest The destination file path.
#'
#' @return Returns TRUE if the file is copied, FALSE otherwise.
#' @export
#'
#' @examples
#' copy_if_not_exists("source.txt", "destination.txt")
#'
copy_if_not_exists <- function(src, dest) {
  if (!file.exists(dest)) {
    file.copy(src, dest)
    return(TRUE)
  }
  return(FALSE)
}




#' Update environment variable
#'
#' @param name Character string of the environment variable name.
#' @param value Character string of the environment variable value.
#' @param environ_file_path Character string of the path to the environment file.
#'
#' @return None.
update_env_variable <- function(name, value, environ_file_path) {
  write.table(
    x = matrix(data = c(name, paste0("\"", value, "\"")), nrow = 1, ncol = 2),
    file = environ_file_path,
    append = TRUE,
    col.names = FALSE,
    sep = "=",
    quote = FALSE,
    row.names = FALSE
  )

  Sys.setenv(name = value)
  message(paste(crayon::green(cli::symbol$tick),
                paste0(name, " location set [", value, "]")))
}

#' Generate Path to Database Upgrade Script
#'
#' This function generates the file path to the SQL upgrade script based on the current version index, 
#' available database versions, and package name.
#'
#' @param current_version_idx Index of the current database version.
#' @param db_versions A vector of available database versions.
#' @param pkgname Name of the package where the SQL scripts are located.
#'
#' @return A string representing the file path to the SQL upgrade script.
#' @export
generate_upgrade_script_path <- function(current_version_idx, db_versions, pkgname) {
  system.file("extdata",
             paste0(file.path("db", 
                              db_versions[current_version_idx + 1], 
                              paste(c("sampledb", "database", db_versions[current_version_idx], db_versions[current_version_idx + 1]), 
                                    collapse = "_")), ".sql"),
             package = pkgname)
}

#' Upgrade Database
#'
#' @param database Path to database toupgrade
#' @param current_version Current version of the database.
#' @param expected_version Expected version of the database.
#' @param db_versions Vector of available database versions.
#' @param pkgname Name of package
upgrade_database <- function(database, current_version, expected_version, db_versions, pkgname) {

  con <- init_db_conn(database)
  on.exit(DBI::dbDisconnect(con), add = TRUE)  # Ensure connection is closed when function exits or if an error occurs

  # Validate inputs
  stopifnot(!is.null(con))
  stopifnot(is(current_version, "character"), length(current_version) == 1)
  stopifnot(is(expected_version, "character"), length(expected_version) == 1)
  stopifnot(is(db_versions, "character"))

  # Start the transaction
  DBI::dbBegin(con)
  
  successful <- FALSE  # To track if the upgrade was successful
  tryCatch({

    current_version_idx <- which(current_version == db_versions)

    while (current_version != expected_version) {
      upgrade_script <- generate_upgrade_script_path(current_version_idx, db_versions, pkgname)
      if (!file.exists(upgrade_script)) {
        stop(paste("Upgrade script not found:", upgrade_script))
      }

      sql <- readr::read_lines(upgrade_script) %>%
        glue::glue_collapse(sep = "\n") %>%
        glue::glue_sql(.con = con) %>%
        strsplit(., ';')
      
      execute_sql(con, sql[[1]])

      current_version_idx <- current_version_idx + 1
      current_version <- db_versions[current_version_idx]
    }

    successful <- TRUE

  }, error = function(e) {
    message("An error occurred during the database upgrade: ", e$message)
  })
  
  # End the transaction
  if (successful) {
    DBI::dbCommit(con)
    DBI::dbExecute(con, "VACUUM")
  } else {
    DBI::dbRollback(con)
  }
}


#' Initialize Database with Base Version
#'
#' This function initializes the SQLite database using the base version.
#'
#' @param new_database Path to the new temporary SQLite database.
#' @param pkgname Name of the package containing the database upgrade scripts.
#' @param db_versions Vector of available database versions.
#' 
#' @return None (primarily called for side effects).
#' @examples
#' \dontrun{
#'   initialize_database_with_base_version(tempfile(), "my_package", c("1.0.0", "2.0.0"))
#' }
initialize_database_with_base_version <- function(new_database, pkgname, base_version = "1.0.0") {
  base_db_sql <- system.file("extdata",
                             paste0(file.path("db", base_version, paste(c("sampledb", "database", base_version), collapse = "_")), ".sql"),
                             package = pkgname)

  system2("sqlite3", paste(new_database, "<", base_db_sql))
  Sys.chmod(new_database, mode = "0777", use_umask = FALSE)
}


#' Finalize the database upgrade
#'
#' This function finalizes the database upgrade by:
#' 1. Removing unused pages.
#' 2. Closing the database connection.
#' 3. Copying the new database to the original path.
#' 4. Changing file permissions.
#'
#' @param database Path to the original database.
#' @param new_database Path to the upgraded database.
#' @param con Connection to new database
#' @return None.
#' @keywords internal setup
# Modified finalize_upgrade to accept a connection object and handle it.
finalize_upgrade <- function(database, new_database, con) {

  # close lock on the old database
  if (!is.null(con)) {
    DBI::dbDisconnect(con)
    file.remove(database)
  }
  file.copy(new_database, Sys.getenv("SDB_PATH"))
  Sys.chmod(Sys.getenv("SDB_PATH"), mode = "0777", use_umask = FALSE)
}

#' Handle any errors during the database upgrade
#'
#' This function provides a way to handle errors that might arise during the database upgrade.
#' It will clean up any temporary files and disconnect from the database.
#'
#' @param new_database Path to the temporary new database.
#' @param e Error message.
#' @return Stops with an error message.
handle_upgrade_error <- function(new_database, e) {
  # Remove the new temporary database if it exists
  if (file.exists(new_database)) {
    file.remove(new_database)
  }

  # Stop and display the error message
  stop(e$message)
}

#' Main environment setup function
#'
#' Setups the environment based on the provided settings. 
#' This includes setting up the configuration, database location and subdirectories.
#'
#' @param site_install Logical indicating whether installation is site-wide.
#' @param pkgname Character string of the package name.
#' @param expected_versions Named list of expected versions for components.
#' @param database The path to the database.
#'
#' @return None.
setup_environment <- function(site_install, pkgname, expected_versions, database) {

  # Setup Config
  config <- Sys.getenv("SDB_CONFIG")
  environ_file_path <- get_environ_file_path(site_install)

  if (0 == nchar(config)) {
    config <- get_normalized_path(site_install, pkgname, "config", "config.yml")
    update_env_variable("SDB_CONFIG", config, environ_file_path)
  } else {
    message(paste(crayon::white(cli::symbol$info), paste0("Config location already set [", config, "]")))
  }

  configdir <- dirname(config)
  if (!dir.exists(configdir)) dir.create(configdir)

  # Update or install configuration
  update_configuration_file(pkgname, expected_versions$config)


  # Setup Database
  if (0 == nchar(database)) {
    database <- get_normalized_path(site_install, pkgname, "data", "sampledb_database.sqlite")
    update_env_variable("SDB_PATH", database, environ_file_path)
  } else {
    message(paste(crayon::white(cli::symbol$info), paste0("Database location already set [", database, "]")))
  }

  datadir <- dirname(database)
  if (!dir.exists(datadir)) dir.create(datadir)

  Sys.chmod(datadir, mode = "0777", use_umask = FALSE)

  subdirs <- suppressWarnings(normalizePath(file.path(datadir, c("backups", "upload_files", "move_files"))))
  for (subdir in subdirs) {
    if (!dir.exists(subdir)) {
      dir.create(subdir)
      Sys.chmod(subdir, mode = "0777", use_umask = FALSE)
      message(paste(crayon::green(cli::symbol$tick), paste0("Subdirectory installed [", subdir, "]")))
    } else {
      message(paste(crayon::white(cli::symbol$info), paste0("Subdirectory exists [", subdir, "]")))
    }
  }
}

#' Setup or upgrade the database
#'
#' This function sets up a new SQLite database or upgrades it if it already exists. 
#' The function will:
#' 1. Check the current version of the database.
#' 2. Create a new one if none exists or upgrades an existing database to the target version.
#' 3. Handle errors during the setup or upgrade process and revert any changes.
#'
#' @param expected_database_version A string that contains the expected database version for the application release.
#' @param pkgname Name of the R package.
#' @param database Path to the SQLite database file. 
#'
#' @return NULL (The function is primarily called for its side effects).
#' @export
#' @examples
#' \dontrun{
#'   setup_database(list(database = "2.0.0"))
#' }
setup_database <- function(expected_database_version, pkgname, database) {
  db_directory <- system.file("extdata", "db", package = pkgname)
  db_versions <- basename(list.dirs(db_directory, recursive = FALSE))
  
  if (length(db_versions) < 1) {
    stop("The upgrade file directory structure is incomplete.")
  }
  
  stopifnot("no upgrade could be found for the version specified" = expected_database_version %in% db_versions)

  # If database doesn't exist, then the current version is NA
  current_db_version <- ifelse(file.exists(database), get_db_version(database), NA)

  # If database exists and matches the expected version, no action is needed
  if (!is.na(current_db_version) && current_db_version == expected_database_version) {
    message(paste(crayon::white(cli::symbol$info), paste0("Database exists [version=", current_db_version, "]")))
    return()
  }

  message(paste(crayon::white(cli::symbol$info), paste0("Installing database [version=", expected_database_version, "]")))
  new_database <- tempfile()

  con <- NULL
  tryCatch({
    if (is.na(current_db_version) || !file.exists(database)) {
      # If no database exists, initialize with base version
      initialize_database_with_base_version(database, pkgname, db_versions[1])

      # Check version of newly created database
      current_db_version <- get_db_version(database)

      # If version of the new database isn't 1.0.0, upgrade to expected version
      if (expected_database_version != current_db_version) {
        # Open a connection for the upgrade
        con <- init_db_conn(database)
        on.exit(DBI::dbDisconnect(con), add = TRUE)  # Ensure connection is closed when function exits or if an error occurs

        upgrade_database(database, current_db_version, expected_database_version, db_versions, pkgname)
      }

    } else {

      # Proceed with the upgrade logic
      con <- init_db_conn(database)
      on.exit(DBI::dbDisconnect(con), add = TRUE)  # Ensure connection is closed when function exits or if an error occurs

      Backup_SampleDB()

      success <- copy_database(database, new_database)
      if (!success) {
        handle_upgrade_error(new_database, simpleError("Failed to copy the database."))
        return()
      }

      current_db_version <- get_db_version(con)

      if (!is.na(current_db_version) && current_db_version != expected_database_version) {
        upgrade_database(new_database, current_db_version, expected_database_version, db_versions, pkgname)
      }

      finalize_upgrade(database, new_database, con)
      message(paste(crayon::green(cli::symbol$tick), paste0("Database upgraded to ", expected_database_version, " [", database, "]")))

    }
    
  }, error = function(e) {
    handle_upgrade_error(new_database, e)
  })
}



#' Deploy Shiny Application
#'
#' This function checks if the environment meets the requirements to deploy
#' a Shiny application and attempts to deploy it if all conditions are met.
#'
#' @param pkgname The name of the package containing the Shiny application to be deployed.
#' @param site_install Logical indicating if this is a site installation.
#'
#' @examples
#' \dontrun{
#' deploy_shiny_app("my_package", TRUE)
#' }
#'
#' @keywords internal
deploy_shiny_app <- function(pkgname, site_install) {
    message(cli::rule(left = crayon::bold("Deploying", pkgname, "Shiny Application")))

    if (!is_linux_platform()) {
        message_not_supported_platform()
        return()
    }

    if (!site_install) {
        message_not_site_install()
        return()
    }

    shiny_server_path <- file.path("", "srv", "shiny-server")

    if (!is_shiny_server_installed(shiny_server_path)) {
        message_shiny_not_installed()
        return()
    }

    pkg_path <- get_package_path(pkgname)
    if (is.null(pkg_path)) {
        message("The package path could not be determined.")
        return()
    }

    deploy_or_notify_existing_app(shiny_server_path, pkgname, pkg_path)
    display_app_url()
}


#' Check if the system platform is Linux.
#'
#' @return Logical value indicating if the platform is Linux.
is_linux_platform <- function() {
    Sys.info()[["sysname"]] %in% c("Linux")
}

#' Display a message indicating Shiny server isn't supported.
#'
#' @return NULL
message_not_supported_platform <- function() {
    message(paste(crayon::red(cli::symbol$cross), "Shiny server is not supported on this platform."))
}

#' Display a message indicating the need for a site install.
#'
#' @return NULL
message_not_site_install <- function() {
    message(paste(crayon::red(cli::symbol$cross), "Must be a site install in order to link the application to shiny server."))
}

#' Check if Shiny server is installed.
#'
#' @param shiny_server_path Path to the Shiny server.
#' @return Logical value indicating if Shiny server is installed.
is_shiny_server_installed <- function(shiny_server_path) {
    dir.exists(shiny_server_path)
}

#' Display a message indicating Shiny server isn't installed.
#'
#' @return NULL
message_shiny_not_installed <- function() {
    message(paste(crayon::red(cli::symbol$cross), "Shiny server is not installed."))
}

#' Deploy the Shiny app or notify if already deployed.
#'
#' @param shiny_server_path Path to the Shiny server.
#' @param pkgname Name of the package.
#' @param pkg_path Path of the package installation.
#' @return NULL
#'
#' @keywords internal
deploy_or_notify_existing_app <- function(shiny_server_path, pkgname, pkg_path) {
    if (!file.exists(file.path(shiny_server_path, pkgname))) {
        file.symlink(pkg_path, file.path(shiny_server_path, pkgname))
        message(paste(crayon::green(cli::symbol$tick), "Shiny application deployed."))
    } else {
        message(paste(crayon::white(cli::symbol$info), "Shiny application already deployed."))
    }
}


#' Display the URL of the deployed Shiny application.
#'
#' @return NULL
display_app_url <- function() {
    ip_all <- system2("hostname", "-I", stdout = TRUE)
    ip <- strsplit(ip_all, " ")[[1]][1]
    # TODO: pull in port from config
    url <- paste0("http://", ip, ":3838/sampleDB/")
    message(paste(crayon::bold("Application is available here:"), url))
}

#' Get Installation Path of a Package
#'
#' Retrieves the installation path of the given package.
#'
#' @param pkgname Character string specifying the name of the package.
#'
#' @return Character string indicating the installation path of the package,
#' or `NULL` if the package is not found.
#'
#' @examples
#' get_package_path("sampleDB")
#'
#' @keywords internal
get_package_path <- function(pkgname) {
  if (pkgname %in% installed.packages()[, "Package"]) {
    return(find.package(pkgname))
  } else {
    return(NULL)
  }
}

#' Check If Package Is Installed System-wide
#'
#' Determines if the given package is installed in a system-wide library location.
#' This is assessed based on whether the package's installation path matches any
#' of the common system library paths (retrieved via `.libPaths()`).
#'
#' @param pkgname Character string specifying the name of the package to check.
#' 
#' @return Logical value indicating if the package is installed in a system-wide
#' location (`TRUE`) or not (`FALSE`).
#' 
#' @examples
#' is_system_installed("sampleDB")
#' is_system_installed("nonexistentPackage")
#' 
#' @keywords internal
is_system_installed <- function(pkgname) {
  path <- get_package_path(pkgname)
  if (is.null(path)) {
    return(FALSE)
  }
  
  # Common system library paths - this may vary depending on the system
  sys_lib_paths <- .libPaths()
  
  return(path %in% sys_lib_paths)
}


#' Retrieve Expected Versions from JSON File
#'
#' This function gets the expected versions of the specified package from
#' an external 'versions.json' file within the package's 'extdata' directory.
#'
#' @param pkgname The name of the package for which the expected versions
#'   should be retrieved.
#' 
#' @return A list containing the expected versions as described in the
#'   'versions.json' file.
#' 
#' @examples
#' \dontrun{
#' get_expected_versions("sampleDB")
#' }
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
get_expected_versions <- function(pkgname) {
  jsonlite::fromJSON(txt = system.file("extdata", "versions.json", package = pkgname))
}

#' Set up and deploy the SampleDB environment, database, and Shiny server.
#'
#' This function orchestrates the deployment of the SampleDB environment,
#' database, and Shiny server based on user preferences. The function calls
#' appropriate helper functions to achieve each of these steps.
#'
#' @param env Logical flag indicating whether to setup the environment.
#'        Defaults to TRUE.
#' @param db Logical flag indicating whether to setup the database.
#'        Defaults to TRUE.
#' @param server Logical flag indicating whether to deploy the Shiny server.
#'        Defaults to TRUE.
#'
#' @return NULL (The function is primarily called for its side effects).
#' @examples
#' \dontrun{
#'   SampleDB_Setup(env = TRUE, db = TRUE, server = TRUE)
#' }
#' @export
#' @import jsonlite
#' @keywords setup
SampleDB_Setup <- function(env=TRUE, db=TRUE, server=TRUE) {

  pkgname <- "sampleDB"
  site_install <- is_system_installed(pkgname)

  database <- Sys.getenv("SDB_PATH")

  message(paste(cli::rule(left = crayon::bold(paste("Deploying", pkgname, "Environment")))))

  expected_versions <- jsonlite::fromJSON(txt = system.file("extdata",
                                        "versions.json", package = pkgname))

  tryCatch({
    if (env) setup_environment(site_install, pkgname, expected_versions, database)
    if (db) setup_database(expected_versions$database, pkgname, database)
    if (server) deploy_shiny_app(pkgname, site_install)
  },
  warning = function(w) {
    message(w)
  },
  error = function(e) {
    message(e)
  })

  # Silence the NULL that is returned
  return(invisible())
}

#' Merge Configuration Lists
#'
#' This function merges two configuration lists: a current configuration and a new configuration.
#' It will return a merged configuration, preferring values from the current configuration when they exist and are not NA.
#'
#' @param current_config A list representing the current configuration.
#' @param new_config A list representing the new configuration.
#' 
#' @return A merged list containing elements from both configurations.
#' @examples
#' \dontrun{
#' current <- list(version = "1.0.0", key = "old_value")
#' new <- list(version = "1.1.0", key = NA, new_key = "new_value")
#' merged <- merge_configs(current, new)
#' print(merged)
#' }
merge_configs <- function(current_config, new_config) {
  
  # If either is NULL or not a list, return the appropriate value
  if (is.null(current_config)) return(new_config)
  if (is.null(new_config)) return(current_config)
  if (!is.list(current_config) || !is.list(new_config)) return(current_config)

  # Use modifyList to combine the lists recursively
  combined_config <- modifyList(new_config, current_config)

  # Handle non-list items (override new config if the current config is not NA)
  lapply(names(combined_config), function(name) {
    if (!is.list(combined_config[[name]]) && is.na(new_config[[name]]) && !is.na(current_config[[name]])) {
      combined_config[[name]] <- current_config[[name]]
    }
  })
  
  return(combined_config)
}

#' Update Configuration File
#'
#' This function updates the configuration file based on a package's new configuration.
#' If the current configuration is missing or outdated, it will be merged with or replaced by the new configuration.
#'
#' @param pkgname The name of the package containing the new configuration.
#' @param expected_config_version A string with the expected user configuration version.
#'
#' @return NULL (The function is primarily called for its side effects).
#' @examples
#' \dontrun{
#' update_configuration_file("my_package_name", "1.1.0")
#' }
update_configuration_file <- function(pkgname, expected_config_version) {
  config <- Sys.getenv("SDB_CONFIG")

  # Install new config if doesn't exist
  if (!file.exists(config)) {
    file.copy(system.file("conf", "config.yml", package = pkgname), config)
    message(paste(crayon::green(cli::symbol$tick), paste0("Configuration file installed [", config, "]")))
    return()
  } 

  # Read both configs
  new_config <- yaml::read_yaml(system.file("conf", "config.yml", package = pkgname))
  current_config <- yaml::read_yaml(config)

  # Update and write back if needed
  if (is.null(current_config$version) || current_config$version < expected_config_version) {
    updated_config <- merge_configs(current_config, new_config)
    yaml::write_yaml(updated_config, config)
    message(paste(crayon::green(cli::symbol$tick), paste0("Configuration file updated to version ", updated_config$version, " [", config, "]")))
  } else {
    message(paste(crayon::white(cli::symbol$info), paste0("Configuration file exists [", config, "]")))
  }
}
