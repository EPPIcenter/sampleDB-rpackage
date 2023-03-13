# Setup SampleDB Database
#'
#' Save SampleDB's database file to a specified location
#'
#' @examples
#' \dontrun{
#' library(sampleDB)
#' SampleDB_Setup()
#' }
#'
#' @import dplyr
#' @import rappdirs
#' @import yaml
#' @import glue
#' @import rjson
#' @export
#'

SampleDB_Setup <- function() {
  libname <- .sampleDB$libname
  pkgname <- .sampleDB$pkgname
  site_install <- .sampleDB$site_install
  message(paste(cli::rule(left = crayon::bold(paste("Deploying", pkgname, "Environment")))))

  tryCatch(
    expr = {

      expected_versions <- rjson::fromJSON(file = system.file("extdata",
                                              "versions.json", package = pkgname))

      # Config Setup

      config <- Sys.getenv("SDB_CONFIG")
      if (0 == nchar(config)) {
        config <- suppressWarnings(
          normalizePath(
            file.path(
              ifelse(site_install,
                rappdirs::site_config_dir(),
                rappdirs::user_config_dir()
              ),
              pkgname,
              "config.yml"
            )
          )
        )

        environ_file_path <- suppressWarnings(
          normalizePath(
              ifelse(site_install,
                file.path(Sys.getenv("R_HOME"), "etc", "Renviron.site"),
                file.path(Sys.getenv("HOME"), ".Renviron")
              )))

        write.table(
          x = matrix(data = c("SDB_CONFIG", paste0("\"", config, "\"")),
                              nrow = 1, ncol = 2),
          file = environ_file_path,
          append = TRUE,
          col.names = FALSE,
          sep = "=",
          quote = FALSE,
          row.names = FALSE)

        Sys.setenv("SDB_CONFIG" = config)
        message(paste(crayon::green(cli::symbol$tick),
          paste0("Config location set [", config, "]")))

      } else {
        message(paste(crayon::white(cli::symbol$info),
          paste0("Config location already set [", config, "]")))
      }

      configdir <- dirname(config)
      if (!dir.exists(configdir))
        dir.create(configdir)

      if (!file.exists(config)) {
          file.copy(system.file("conf",
              "config.yml", package = pkgname), config)
          message(paste(crayon::green(cli::symbol$tick), paste0("Configuration file installed [", config, "]")))
      } else {
          new_config <- yaml::read_yaml(system.file("conf",
                          "config.yml", package = pkgname))

          current_config <- yaml::read_yaml(Sys.getenv("SDB_CONFIG"))

          if (is.null(current_config$version) || current_config$version < expected_versions$config) {
            current_config <- .recurse_update_config(current_config, new_config)
            yaml::write_yaml(current_config, Sys.getenv("SDB_CONFIG"))
            message(paste(crayon::green(cli::symbol$tick), paste0("Configuration file updated to version ", current_config$version, " [", config, "]")))
          } else {
            message(paste(crayon::white(cli::symbol$info), paste0("Configuration file exists [", config, "]")))
          }
      }

      # Database Setup

      database <- Sys.getenv("SDB_PATH")

      if(0 == nchar(database)) {

        environ_file_path <- suppressWarnings(
          normalizePath(
              ifelse(site_install,
                file.path(Sys.getenv("R_HOME"), "etc", "Renviron.site"),
                file.path(Sys.getenv("HOME"), ".Renviron")
              )))

        if (!file.exists(environ_file_path))
            file.create(environ_file_path)

        database <- suppressWarnings(
          normalizePath(
            file.path(
              ifelse(site_install,
                rappdirs::site_data_dir(),
                rappdirs::user_data_dir()
              ),
              pkgname,
              "sampledb_database.sqlite"
              )
            )
          )


        write.table(
          x = matrix(data = c("SDB_PATH", paste0("\"", database, "\"")),
                              nrow = 1, ncol = 2),
          file = environ_file_path,
          append = TRUE,
          col.names = FALSE,
          sep = "=",
          quote = FALSE,
          row.names = FALSE
        )

        Sys.setenv("SDB_PATH" = database)
        message(paste(crayon::green(cli::symbol$tick),
          paste0("Database location set [", database, "]")))

      } else {
        message(paste(crayon::white(cli::symbol$info),
          paste0("Database location already set [", database, "]")))
      }

      datadir <- dirname(database)
      if (!dir.exists(datadir))
        dir.create(datadir)

      db_directory <- system.file("extdata", "db", package = pkgname)
      db_versions <- list.dirs(db_directory)
      if (length(db_versions) < 2) {
        stop("The upgrade file directory structure is incomplete.")
      }

      upgrade_scripts <- db_versions[2:length(db_versions)] # remove parent directory in list

      db_versions <- basename(upgrade_scripts)

      stopifnot("no upgrade could be found for the version specified" = expected_versions$database %in% db_versions)

      current_version_idx <- NULL
      current_db_version <- NA

      # get the current database version
      if (file.exists(database)) {
        con <- DBI::dbConnect(SQLite(), database)
        current_db_version <- DBI::dbGetQuery(con, "SELECT name FROM version ORDER BY name DESC LIMIT 1") %>% pull(name)
        DBI::dbDisconnect(con)
      }

      # if the databases match, don't do anything
      if (!is.na(current_db_version) && current_db_version == expected_versions$database) {
        message(paste(crayon::white(cli::symbol$info), paste0("Database exists [version=", current_db_version, "]")))
      } else {
        message(paste(crayon::white(cli::symbol$info), paste0("Installing database [version=", expected_versions$database, "]")))

        new_database <- tempfile()

        tryCatch({

          old_db_con <- NULL
          # if the database does not exist, create base db in temporary location, otherwise copy the old database to the temp location
          if (!file.exists(database)) {
            base_db_sql <- system.file("extdata",
                                        paste0(file.path("db", db_versions[1], paste(c("sampledb", "database", db_versions[1]), collapse = "_")), ".sql"),
                                        package = pkgname)

            system2("sqlite3", paste(new_database, "<", base_db_sql))
            Sys.chmod(new_database, mode = "0777", use_umask = FALSE)
          } else {
            
            ## backup the database 
            Backup_SampleDB()

            old_db_con <- DBI::dbConnect(SQLite(), database)

            lapply(c("PRAGMA locking_mode = EXCLUSIVE;", "BEGIN EXCLUSIVE;"), function(s) { DBI::dbExecute(old_db_con, s) })

            if (file.exists(database)) {
              file.copy(database, new_database)
            }
          }

          con <- DBI::dbConnect(SQLite(), new_database)

          lapply(c("PRAGMA locking_mode = EXCLUSIVE;", "BEGIN EXCLUSIVE;"), function(s) { DBI::dbExecute(con, s) })

          current_db_version <- DBI::dbGetQuery(con, "SELECT name FROM version ORDER BY name DESC LIMIT 1") %>% pull(name)
          current_version_idx <- which(current_db_version == db_versions) 

          while (current_db_version != expected_versions$database) {

            upgrade_script <- system.file("extdata",
              paste0(file.path("db", db_versions[current_version_idx + 1], paste(c("sampledb", "database", db_versions[current_version_idx], db_versions[current_version_idx + 1]), collapse = "_")), ".sql"),
              package = pkgname)

            sql <- readr::read_lines(upgrade_script) %>%
              glue::glue_collapse(sep = "\n") %>%
              glue::glue_sql(.con = con) %>%
              strsplit(., ';')

            lapply(sql[[1]], function(s) { DBI::dbExecute(con, s) })

            current_version_idx <- current_version_idx + 1
            current_db_version <- db_versions[current_version_idx]
          }

          # commit 
          DBI::dbCommit(con)

          # remove unused pages and close connection to the new database
          DBI::dbExecute(con, "VACUUM")
          DBI::dbDisconnect(con)

          # close lock on the old database
          if (!is.null(old_db_con)) {
            DBI::dbDisconnect(old_db_con)
            file.remove(database)
          }
          file.copy(new_database, Sys.getenv("SDB_PATH"))
          message(paste(crayon::green(cli::symbol$tick), paste0("Database upgraded to ", expected_versions$database, " [", database, "]")))
          Sys.chmod(Sys.getenv("SDB_PATH"), mode = "0777", use_umask = FALSE)
        },
        error = function(e) {
          if (file.exists(new_database))
            file.remove(new_database)

          DBI::dbDisconnect(con)
          stop(e$message)
        })
      }

      # create subdirectories

      subdirs <- suppressWarnings(
        normalizePath(
            file.path(
              datadir, c(
                "backups",
                "upload_files",
                "move_files"
              )
            )
          )
        )


      # subdirs <- file.path(datadir, c("backups", "upload_files", "move_files"))
      for (subdir in subdirs) {
          if (!dir.exists(subdir)) {
              dir.create(subdir)
              Sys.chmod(subdir, mode = "0777", use_umask = FALSE)
              message(paste(crayon::green(cli::symbol$tick), paste0("Subdirectory installed [", subdir, "]")))
          } else {
              message(paste(crayon::white(cli::symbol$info), paste0("Subdirectory exists [", subdir, "]")))
          }
      }

      Sys.chmod(datadir, mode = "0777", use_umask = FALSE)

      # shiny application deployment
      message(paste(cli::rule(left = crayon::bold("Deploying", pkgname, "Shiny Application"))))

      if (!Sys.info()[["sysname"]] %in% c("Linux")) {
        message(paste(
          crayon::red(cli::symbol$cross),
            "Shiny server is not supported on this platform."
        ))
      } else if (isFALSE(site_install)) {
        message(paste(
          crayon::red(cli::symbol$cross),
            "Must be a site install in order to link the application to shiny server."
        ))
      } else {
        shiny_server <- file.path("", "srv", "shiny-server")
        if (!dir.exists(shiny_server)) {
          stop(paste(
            crayon::red(cli::symbol$cross), "Shiny server is not installed."
          ))
        } else {
          message(paste(
            crayon::green(cli::symbol$tick), "Shiny server is installed."
          ))
          if (!file.exists(file.path(shiny_server, pkgname))) {
            file.symlink(file.path(.sampleDB$libname, pkgname),
              file.path(shiny_server, pkgname))
            message(paste(
              crayon::green(cli::symbol$tick), "Shiny application deployed."
            ))
          } else {
            message(paste(
              crayon::white(cli::symbol$info), "Shiny application already deployed."
            ))
          }
          # retrieve IP address
          ip_all <- system2("hostname", "-I", stdout = TRUE)
          ip <- as.list(
            strsplit(ip_all, " ")[[1]]
            )[[1]]

          # TODO: pull in port from config
          url <- paste0("http://", ip, ":3838/sampleDB/")
          message(paste(
            crayon::bold("Application is available here:", url)))
        }
      }
    },
    warning = function(w) {
      message(w)
    },
    error = function(e) {
      message(e)
    }
  )
}


.recurse_update_config <- function(current_config, new_config) {

  for (name in names(new_config)) {
    new_config[[name]] <- .recurse_update_config(current_config[[name]], new_config[[name]])

    if (is.list(current_config[[name]])) {
      next
    }

    if (is.null(current_config[[name]]) || is.null(new_config[[name]])) {
      next
    }

    if (is.na(current_config[[name]]) & is.na(new_config[[name]])) {
      next
    }

    if (is.na(new_config[[name]]) & !is.na(current_config[[name]])) {
      new_config[[name]] <- current_config[[name]]
      next
    }

    if (new_config[[name]] != current_config[[name]]) {
      new_config[[name]] <- current_config[[name]]
    }
  }

  return(new_config)
}
