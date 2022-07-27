library(rappdirs)

.onLoad <- function(libname, pkgname) {

    b_site_install <- libname %in%
        normalizePath(c(.Library.site, .Library), winslash = "/")

    # install data files
    datadir <- normalizePath(
        path.expand(
            file.path(
                ifelse(b_site_install,
                rappdirs::site_data_dir(),
                rappdirs::user_data_dir()),
            pkgname))
    )

    args <- list(datadir)
    pathname <- paste0("SDB_PATH", ifelse(b_site_install, "_SITE", "_LOCAL"))
    if (nchar(Sys.getenv(pathname)) == 0) {
        names(args) <- pathname
        do.call("Sys.setenv", args)
    }
}

.onAttach <- function(libname, pkgname) {

    packageStartupMessage(paste(
        cli::rule(left = crayon::bold(paste("Loading", pkgname)))))

    b_site_install <- libname %in%
        normalizePath(c(.Library.site, .Library), winslash = "/")

    datadir <- Sys.getenv(paste0(
        "SDB_PATH", ifelse(b_site_install, "_SITE", "_LOCAL")))

    packageStartupMessage(
        ifelse(b_site_install,
        paste0("Loading site library [", libname, "]."),
        paste0("Loading local library [", libname, "].")))

    # setup environment
    packageStartupMessage("Setting up environment...")
    assign(paste0(".", pkgname), data.frame(
        pkgname = pkgname,
        libname = libname,
        site_install = b_site_install,
        datadir = datadir,
        database = file.path(datadir, "sampledb_database.sqlite"),
        backups = file.path(datadir, "backups"),
        upload_files = file.path(datadir, "upload_files"),
        move_files = file.path(datadir, "move_files")
    ), as.environment(paste0("package:", pkgname)))

    # could print out environment here...

    packageStartupMessage("Setup complete.")
}
