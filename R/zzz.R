library(rappdirs)

.onAttach <- function(libname, pkgname) {
    # for shiny install
    assign(paste0(".", pkgname), list(
        libname = suppressWarnings(
          normalizePath(
            file.path(libname))),
        pkgname = pkgname,
        site_install = libname %in% gsub(
          "/$", "", normalizePath(
            c(.Library.site, .Library), winslash = "/"))
    ), as.environment(paste0("package:", pkgname)))
}
