.onAttach <- function(libname, pkgname) {
    # for shiny install
    assign(paste0(".", pkgname), list(
        libname = suppressWarnings(
          normalizePath(
            file.path(libname))),
        pkgname = pkgname,
        site_install = libname %in% gsub(
          "/$", "", normalizePath(
            c(.Library.site, .Library), winslash = "/")),
        datadir = dirname(Sys.getenv("SDB_PATH")) # only used as a debugging helper
    ), as.environment(paste0("package:", pkgname)))
}
