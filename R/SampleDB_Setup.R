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

  if (name == "SDB_CONFIG") {
    Sys.setenv("SDB_CONFIG" = value)
  }
  if (name == "SDB_PATH") {
    Sys.setenv("SDB_PATH" = value)
  }

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
  message("INFO: upgrade_database() called")
  
  message("INFO: Initializing database connection")
  con <- init_db_conn(database)
  message(paste("INFO: Connection object:", str(con)))

  on.exit(DBI::dbDisconnect(con), add = TRUE)

  message("INFO: Validating inputs")
  stopifnot(!is.null(con))
  stopifnot(is(current_version, "character"), length(current_version) == 1)
  stopifnot(is(expected_version, "character"), length(expected_version) == 1)
  stopifnot(is(db_versions, "character"))

  message("INFO: Starting the transaction")
  DBI::dbBegin(con)

  successful <- FALSE

  tryCatch({
    message("INFO: Inside tryCatch block")
    
    current_version_idx <- which(current_version == db_versions)
    message(paste("INFO: Current version index:", current_version_idx))

    while (current_version != expected_version) {
      message("INFO: Entering upgrade loop")
      
      upgrade_script_path <- generate_upgrade_script_path(current_version_idx, db_versions, pkgname)
      message(paste("INFO: Upgrade script path:", upgrade_script_path))
      
      if (!file.exists(upgrade_script_path)) {
        message(paste("WARN: Upgrade script not found:", upgrade_script_path))
        stop(paste("Upgrade script not found:", upgrade_script_path))
      }

      # Read the entire upgrade script
      upgrade_script <- readr::read_lines(upgrade_script_path, skip_empty_rows = FALSE)
      
      # Split the script into commands based on empty lines
      commands <- split(upgrade_script, cumsum(upgrade_script == ""))
      
      # Execute each command block
      for (cmd in commands) {
        sql_command <- paste(cmd, collapse = "\n")
        if (nchar(sql_command) > 0) { # Ensure the command is not just an empty line
          message(paste("INFO: SQL commands to be executed:\n", sql_command))
          execute_sql(con, sql_command)
        }
      }
      
      current_version_idx <- current_version_idx + 1
      current_version <- db_versions[current_version_idx]
      message(paste("INFO: Upgraded to version:", current_version))
    }

    message("INFO: Upgrade successful")
    successful <- TRUE

  }, error = function(e) {
    message(paste("WARN: An error occurred during the database upgrade:", e$message))
  })

  message("INFO: Ending the transaction")
  if (successful) {
    DBI::dbCommit(con)
    DBI::dbExecute(con, "VACUUM")
    message("INFO: Transaction committed and database vacuumed")
  } else {
    DBI::dbRollback(con)
    message("WARN: Transaction rolled back due to unsuccessful upgrade")
  }
  
  message("INFO: upgrade_database() completed")
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

setup_environment <- function(site_install, pkgname, expected_versions, database) {

  # Initialize the log file
  log_file <- "setup_environment.log"

  # Log: Function called
  cat("[INFO] setup_environment() called\n", file=log_file, append=TRUE)
  
  # Setup Config
  config <- Sys.getenv("SDB_CONFIG")
  environ_file_path <- get_environ_file_path(site_install)

  # Log: Initial variables
  cat(sprintf("[INFO] Initial Variables - site_install: %s, pkgname: %s, config: %s, database: %s\n",
              site_install, pkgname, config, database), file=log_file, append=TRUE)

  if (0 == nchar(config)) {
    config <- get_normalized_path(site_install, pkgname, "config", "config.yml")
    update_env_variable("SDB_CONFIG", config, environ_file_path)
    # Log: Config updated
    cat(sprintf("[INFO] Config updated to: %s\n", config), file=log_file, append=TRUE)
  } else {
    message(paste(crayon::white(cli::symbol$info), paste0("Config location already set [", config, "]")))
    # Log: Config already set
    cat(sprintf("[INFO] Config location already set to: %s\n", config), file=log_file, append=TRUE)
  }

  configdir <- dirname(config)
  if (!dir.exists(configdir)) dir.create(configdir, recursive = TRUE)
  
  # Update or install configuration
  update_configuration_file(pkgname, expected_versions$config)

  # Log: Database setup
  cat("[INFO] Setting up database\n", file=log_file, append=TRUE)

  if (0 == nchar(database)) {
    database <- get_normalized_path(site_install, pkgname, "data", "sampledb_database.sqlite")
    update_env_variable("SDB_PATH", database, environ_file_path)
    # Log: Database path updated
    cat(sprintf("[INFO] Database path updated to: %s\n", database), file=log_file, append=TRUE)
  } else {
    message(paste(crayon::white(cli::symbol$info), paste0("Database location already set [", database, "]")))
    # Log: Database path already set
    cat(sprintf("[INFO] Database location already set to: %s\n", database), file=log_file, append=TRUE)
  }

  datadir <- dirname(database)
  if (!dir.exists(datadir)) dir.create(datadir, recursive = TRUE)
  
  Sys.chmod(datadir, mode = "0777", use_umask = FALSE)

  subdirs <- suppressWarnings(normalizePath(file.path(datadir, c("backups", "upload_files", "move_files"))))
  for (subdir in subdirs) {
    if (!dir.exists(subdir)) {
      dir.create(subdir)
      Sys.chmod(subdir, mode = "0777", use_umask = FALSE)
      message(paste(crayon::green(cli::symbol$tick), paste0("Subdirectory installed [", subdir, "]")))
      # Log: Subdirectory created
      cat(sprintf("[INFO] Subdirectory created: %s\n", subdir), file=log_file, append=TRUE)
    } else {
      message(paste(crayon::white(cli::symbol$info), paste0("Subdirectory exists [", subdir, "]")))
      # Log: Subdirectory exists
      cat(sprintf("[INFO] Subdirectory exists: %s\n", subdir), file=log_file, append=TRUE)
    }
  }

  # Log: Function completed
  cat("[INFO] setup_environment() completed\n", file=log_file, append=TRUE)
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
  message("INFO: Entering setup_database function")

  message("INFO: Locating database directory")
  db_directory <- system.file("extdata", "db", package = pkgname)
  message(paste("INFO: Database directory is:", db_directory))

  message("INFO: Listing database versions")
  db_versions <- basename(list.dirs(db_directory, recursive = FALSE))
  message(paste("INFO: Available database versions:", paste(db_versions, collapse = ", ")))

  if (length(db_versions) < 1) {
    message("ERROR: The upgrade file directory structure is incomplete.")
    stop("The upgrade file directory structure is incomplete.")
  }

  message(paste("INFO: Checking if expected_database_version is in db_versions:", expected_database_version %in% db_versions))
  stopifnot("no upgrade could be found for the version specified" = expected_database_version %in% db_versions)

  message("INFO: Checking if database file exists")
  current_db_version <- ifelse(file.exists(database), get_db_version(database), NA)
  message(paste("INFO: Current database version is:", current_db_version))

  if (!is.na(current_db_version) && current_db_version == expected_database_version) {
    message(paste("INFO: Database already exists with expected version:", current_db_version))
    return()
  }

  message(paste("INFO: Installing database with version:", expected_database_version))
  new_database <- tempfile()
  message(paste("INFO: Temporary database file created at:", new_database))

  con <- NULL
  tryCatch({
    message("INFO: Entering tryCatch block")

    if (is.na(current_db_version) || !file.exists(database)) {
      message("INFO: No existing database. Initializing...")

      initialize_database_with_base_version(database, pkgname, db_versions[1])

      current_db_version <- get_db_version(database)
      message(paste("INFO: New database version is:", current_db_version))

      if (expected_database_version != current_db_version) {
        message("INFO: Upgrading database to expected version")

        con <- init_db_conn(database)
        on.exit(DBI::dbDisconnect(con), add = TRUE)

        upgrade_database(database, current_db_version, expected_database_version, db_versions, pkgname)
      }

    } else {
      message("INFO: Existing database found. Preparing to upgrade...")

      con <- init_db_conn(database)
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      # message("INFO: Backing up database...")      
      # Backup_SampleDB()

      message(paste0("INFO: Copying database to ", new_database))
      success <- copy_database(database, new_database)
      message(paste("INFO: Database copy was successful:", success))

      if (!success) {
        message("ERROR: Failed to copy the database.")
        handle_upgrade_error(new_database, simpleError("Failed to copy the database."))
        return()
      }

      current_db_version <- get_db_version(con)
      message(paste("INFO: Current version of new database copy:", current_db_version))

      if (!is.na(current_db_version) && current_db_version != expected_database_version) {
        message("INFO: Upgrading new database copy to expected version")
        
        upgrade_database(new_database, current_db_version, expected_database_version, db_versions, pkgname)
      }

      finalize_upgrade(database, new_database, con)
      message(paste("INFO: Database successfully upgraded to:", expected_database_version))

    }

  }, error = function(e) {
    message(paste("ERROR: An error occurred:", e$message))
    handle_upgrade_error(new_database, e)
  })

  message("INFO: Exiting setup_database function")
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
#' @importFrom jsonlite fromJSON
#' @keywords setup
SampleDB_Setup <- function(env=TRUE, db=TRUE, server=TRUE, reset_env_variables=FALSE) {

  log_file <- file("SampleDB_Setup.log", open="wt")  # Open the log file

  # Log the initial setup
  writeLines("[INFO] SampleDB_Setup() called", log_file)

  # Log the function parameters
  writeLines(paste("[INFO] Initial Parameters - env:", env, "db:", db, "server:", server, "reset_env_variables:", reset_env_variables), log_file)

  if (reset_env_variables) {
    # Log the environment variables before unsetting
    writeLines(paste("[INFO] Before unset - SDB_CONFIG:", Sys.getenv("SDB_CONFIG"), "SDB_PATH:", Sys.getenv("SDB_PATH")), log_file)

    Sys.unsetenv("SDB_CONFIG")
    Sys.unsetenv("SDB_PATH")

    # Log that the environment variables have been unset
    writeLines("[INFO] Environment variables SDB_CONFIG and SDB_PATH unset", log_file)

    # Log the environment variables after unsetting to confirm
    writeLines(paste("[INFO] After unset - SDB_CONFIG:", Sys.getenv("SDB_CONFIG"), "SDB_PATH:", Sys.getenv("SDB_PATH")), log_file)
  }

  pkgname <- "sampleDB"
  site_install <- is_system_installed(pkgname)

  writeLines(paste("[INFO] pkgname set to:", pkgname, "site_install set to:", site_install), log_file)

  message(paste(cli::rule(left = crayon::bold(paste("Deploying", pkgname, "Environment")))))

  expected_versions <- jsonlite::fromJSON(txt = system.file("extdata",
                                        "versions.json", package = pkgname))

  writeLines("[INFO] Expected versions loaded", log_file)

  tryCatch({
    if (env) {
      writeLines("[INFO] Running setup_environment()", log_file)
      setup_environment(site_install, pkgname, expected_versions, Sys.getenv("SDB_PATH"))
    }
    if (db) {
      writeLines("[INFO] Running setup_database()", log_file)
      setup_database(expected_versions$database, pkgname, Sys.getenv("SDB_PATH"))
    }
    if (server) {
      writeLines("[INFO] Running deploy_shiny_app()", log_file)
      deploy_shiny_app(pkgname, site_install)
    }
  },
  warning = function(w) {
    message(w)
    writeLines(paste("[WARN]", w), log_file)
  },
  error = function(e) {
    message(e)
    writeLines(paste("[ERROR]", e), log_file)
  })

  writeLines("[INFO] SampleDB_Setup() completed", log_file)

  close(log_file)  # Close the log file

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

  # Assign the new version
  combined_config$version <- new_config$version

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
# Your log file
update_configuration_file <- function(pkgname, expected_config_version) {

  log_file <- "setup_environment.log"

  # Log: Function called
  cat("[INFO] update_configuration_file() called\n", file=log_file, append=TRUE)

  config <- Sys.getenv("SDB_CONFIG")

  # Log: Initial variables
  cat(sprintf("[INFO] Initial Variables - pkgname: %s, expected_config_version: %s, config: %s\n",
              pkgname, expected_config_version, config), file=log_file, append=TRUE)

  # Install new config if doesn't exist
  if (!file.exists(config)) {
    file.copy(system.file("conf", "config.yml", package = pkgname), config)
    message(paste(crayon::green(cli::symbol$tick), paste0("Configuration file installed [", config, "]")))
    # Log: New config installed
    cat(sprintf("[INFO] New configuration file installed at: %s\n", config), file=log_file, append=TRUE)
    return()
  }

  # Read both configs
  new_config <- yaml::read_yaml(system.file("conf", "config.yml", package = pkgname))
  current_config <- yaml::read_yaml(config)

  # Log: Reading configurations
  cat("[INFO] Reading new and current configurations\n", file=log_file, append=TRUE)

  # Update and write back if needed
  if (is.null(current_config$version) || current_config$version < expected_config_version) {
    updated_config <- merge_configs(current_config, new_config)
    yaml::write_yaml(updated_config, config)
    message(paste(crayon::green(cli::symbol$tick), paste0("Configuration file updated to version ", updated_config$version, " [", config, "]")))
    # Log: Configuration updated
    cat(sprintf("[INFO] Configuration file updated to version: %s\n", updated_config$version), file=log_file, append=TRUE)
  } else {
    message(paste(crayon::white(cli::symbol$info), paste0("Configuration file exists [", config, "]")))
    # Log: Configuration already exists
    cat(sprintf("[INFO] Configuration file already exists at: %s\n", config), file=log_file, append=TRUE)
  }

  # Log: Function completed
  cat("[INFO] update_configuration_file() completed\n", file=log_file, append=TRUE)
}
