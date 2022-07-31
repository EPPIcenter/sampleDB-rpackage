library(rappdirs)

.onAttach <- function(libname, pkgname) {
    # for shiny install
    assign(paste0(".", pkgname), list(
        libname = suppressWarnings(
          normalizePath(
            file.path(libname))),
        pkgname = pkgname
    ), as.environment(paste0("package:", pkgname)))
}
