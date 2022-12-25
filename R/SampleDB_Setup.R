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
#' @export
#'

SampleDB_Setup <- function() {
  libname <- .sampleDB$libname
  pkgname <- .sampleDB$pkgname
  site_install <- .sampleDB$site_install
  message(paste(cli::rule(left = crayon::bold(paste("Deploying", pkgname, "Environment")))))

  tryCatch(
    expr = {

      expected_versions <- yaml::read_yaml(system.file("extdata",
                                              "versions.yml", package = pkgname))

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

      # install database file
      if (!file.exists(database)) {
          database_sql <- system.file("extdata",
                                      paste0(file.path("db", expected_versions$database, paste(c("sampledb", "database", expected_versions$database), collapse = "_")), ".sql"),
                                      package = pkgname)

          system2("sqlite3", paste(database, "<", database_sql))
          Sys.chmod(database, mode = "0777", use_umask = FALSE)
          message(paste(crayon::green(cli::symbol$tick), paste0("Database version ", expected_versions$database, " installed [", database, "]")))
      }

      # upgrade to the newest database iteratively
      else {
        upgrade_directory <- system.file("extdata", "db", package = pkgname)
        db_versions <- list.dirs(upgrade_directory)
        if (length(db_versions) < 2) {
          stop("The upgrade file directory structure is incomplete.")
        }

        upgrade_scripts <- db_versions[2:length(db_versions)] # remove parent directory in list
        db_versions <- basename(upgrade_scripts)

        # get the current database version
        con <- DBI::dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
        DBI::dbConnect(con)

        lastest_db_version <- DBI::dbGetQuery(con, "SELECT name FROM version ORDER BY name DESC LIMIT 1")

        DBI::dbDisconnect(con)

        current_version_idx = which(lastest_db_version$name == db_versions)

        # if the databases match, don't do anything
        if (db_versions[current_version_idx] == expected_versions$database) {
          message(paste(crayon::white(cli::symbol$info), paste0("Database exists [", database, "]")))
        }

        # iterate from the current version to the target version iteratively
        else {

          # note: backs up to SDB_PATH backup folder
          message("Backing up current database.")
          Backup_SampleDB()

          while (db_versions[current_version_idx] != expected_versions$database) {
            stopifnot("no upgrade could be found for the version specified" = current_version_idx <= length(db_versions))
            upgrade_script <- system.file("extdata",
                                          paste0(file.path("db", db_versions[current_version_idx + 1], paste(c("sampledb", "database", db_versions[current_version_idx], db_versions[current_version_idx + 1]), collapse = "_")), ".R"),
                                          package = pkgname)

            source(upgrade_script)
            current_version_idx <- current_version_idx + 1
          }

          message(paste(crayon::green(cli::symbol$tick), paste0("Database upgraded to ", expected_versions$database, " [", database, "]")))
        }
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

    # if (is.null(current_config[[name]]) && !is.null(new_config[[name]])) {
    #   new_config[[name]] <- current_config[[name]]
    #   next
    # }

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
