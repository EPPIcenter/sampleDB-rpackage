library(rappdirs)

.onAttach <- function(libname, pkgname) {
    # for shiny install
    assign(paste0(".", pkgname), list(
        libname = suppressWarnings(
          normalizePath(
            file.path(libname))),
        pkgname = pkgname,
        site_install = libname %in% c(.Library, .Library.site) 
    ), as.environment(paste0("package:", pkgname)))
}
