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
#' @export
#'

SampleDB_Setup <- function() {
  libname <- .sampleDB$libname
  pkgname <- .sampleDB$pkgname
  site_install <- .sampleDB$site_install
  message(paste(cli::rule(left = crayon::bold(paste("Deploying", pkgname, "Environment")))))

  tryCatch(
    expr = {

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
          message(paste(crayon::green(cli::symbol$tick), paste0("Config file installed [", config, "]")))
      } else {
          message(paste(crayon::white(cli::symbol$info), paste0("Configuration file exists [", config, "]")))
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
          row.names = FALSE)

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
                        "sampledb_database.sql", package = pkgname)
          system2("sqlite3", paste(database, "<", database_sql))
          Sys.chmod(database, mode = "0777", use_umask = FALSE)
          message(paste(crayon::green(cli::symbol$tick), paste0("Database installed [", database, "]")))
      } else {
          message(paste(crayon::white(cli::symbol$info), paste0("Database exists [", database, "]")))
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
