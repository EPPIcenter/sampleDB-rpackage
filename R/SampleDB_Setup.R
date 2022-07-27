#' Setup SampleDB Database
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
#' @export
#'

SampleDB_Setup <- function() {
  shiny_server <- "/srv/shiny-server"
  datadir <- .sampleDB$datadir
  pkgname <- .sampleDB$pkgname
  libname <- .sampleDB$libpath
  b_site_install <- .sampleDB$site_install

  message(paste(cli::rule(left = crayon::bold(paste("Deploying", pkgname, "Environment")))))

  # create data directory
  if (!dir.exists(datadir)) {
      dir.create(datadir)
      message(paste(
        crayon::green(cli::symbol$tick), paste0("Data Directory installed [", .sampleDB$datadir, "]")
      ))
  } else {
    message(paste(crayon::white(cli::symbol$info), paste0("Data Directory exists [", .sampleDB$datadir, "]")))
  }

  # create subdirectories
  for (subdir in list(.sampleDB$backups, .sampleDB$upload_files, .sampleDB$move_files)) {
      if (!dir.exists(subdir)) {
          dir.create(subdir)
          message(paste(crayon::green(cli::symbol$tick), paste0("Subdirectory installed [", subdir, "]")))
      } else {
          message(paste(crayon::white(cli::symbol$info), paste0("Subdirectory exists [", subdir, "]")))
      }
  }

  # install database file
  db_file <- file.path(datadir, "sampledb_database.sqlite")
  if (!file.exists(db_file)) {
      file.copy(system.file("extdata",
          "sampledb_database.sqlite", package = pkgname), db_file)
      message(paste(crayon::green(cli::symbol$tick), paste0("Database installed [", db_file, "]")))
  } else {
      message(paste(crayon::white(cli::symbol$info), paste0("Database exists [", db_file, "]")))
  }

  # shiny application deployment
  message(paste(cli::rule(left = crayon::bold("Deploying Shiny Application"))))

  if (isFALSE(b_site_install)) {
    stop(paste(crayon::red(cli::symbol$cross),
      "Shiny application available for site installation only"))
  }
  if (Sys.info()[["sysname"]] %in% c("Linux")) {
    if (!dir.exists(shiny_server)) {
      stop(paste(
        crayon::red(cli::symbol$cross), "Shiny server is not installed."
      ))
    } else {
      message(paste(
        crayon::green(cli::symbol$tick), "Shiny server is installed."
      ))
      if (!file.exists(file.path(shiny_server, pkgname))) {
        file.symlink(file.path(shiny_server, pkgname),
          file.path(libname, pkgname))
        message(paste(
          crayon::green(cli::symbol$tick), "Shiny application deployed."
        ))
      } else {
        message(paste(
          crayon::green(cli::symbol$tick), "Shiny application already deployed."
        ))
      }
      # retrieve IP address
      ip_all <- system2("hostname", "-I", stdout = TRUE)
      ip <- as.list(
        strsplit(ip_all, " ")[[1]]
        )[[1]]

      # TODO: pull in port from config
      url <- paste0("http://", ip, ":3838/sampleDB/")
      message(paste("Application is available here:", url))
    }
  } else {
    message(paste(
      crayon::red(cli::symbol$cross),
        "Shiny server is not supported on this platform."
    ))
  }
}
