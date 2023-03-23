library(yaml)
# Utility Functions that are shared between Pkg Funs and Shiny App Funs
# Use ::: to access, these functions are not package exports

.Backup <- function(database, backup_dest) {

  args <- paste(paste0("\"", database, "\""), paste0("\".backup ", paste0("\'", backup_dest, "\'"), "\""))
  system2("sqlite3", args)

  return(file.exists(backup_dest))
}

.Uncompress <- function(source, destination, buffer_size = 1e+07) {
  compressed_file <- paste0(source, ".gz")

  # From R.UTILS

  inn <- gzfile(source, open="rb")
  on.exit(if (!is.null(inn)) close(inn))

  outComplete <- FALSE
  out <- file(destination, open="wb")
  on.exit({
    if (!is.null(out)) close(out)
    # Remove incomplete file?
    if (!outComplete) file.remove(destination)
  }, add=TRUE)

  # Process
  nbytes <- 0
  repeat {
    bfr <- readBin(inn, what=raw(0L), size=1L, n=buffer_size)
    n <- length(bfr)
    if (n == 0L) break
    nbytes <- nbytes + n
    writeBin(bfr, con=out, size=1L)
    bfr <- NULL  # Not needed anymore
  }
  outComplete <- TRUE
  close(out)
  out <- NULL

  return(file.exists(destination))
}


.Compress <- function(source, destination, buffer_size = 1e+07) {
  is_windows <- (Sys.info()[['sysname']] == "Windows")
  compressed_file <- paste0(destination, ".gz")

  # From R.UTILS

  # Setup input and output connections

  inn <- file(source, open="rb")
  on.exit(if (!is.null(inn)) close(inn))

  outComplete <- FALSE
  out <- gzfile(compressed_file, open="wb")
  on.exit({
    if (!is.null(out)) close(out)
    # Remove incomplete file?
    if (!outComplete) {
      file.remove(compressed_file)
    }
  }, add=TRUE)

  # Process
  nbytes <- 0
  repeat {
    bfr <- readBin(inn, what=raw(0L), size=1L, n=buffer_size)
    n <- length(bfr)
    if (n == 0L) break
    nbytes <- nbytes + n
    writeBin(bfr, con=out, size=1L)
    bfr <- NULL  # Not needed anymore
  }
  outComplete <- TRUE
  close(out)
  out <- NULL

  return(file.exists(compressed_file))
}
