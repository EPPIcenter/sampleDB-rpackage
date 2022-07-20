#' @export

Gzip <- function(source, destination, buffer_size = 1e+07) {
	if(Sys.info()[['sysname']] == "Windows") {
		zip(source, destination)
	} else {

		# From R.UTILS

		# Setup input and output connections

		inn <- file(source, open="rb")
		on.exit(if (!is.null(inn)) close(inn))

		outComplete <- FALSE
		out <- gzfile(destination, open="wb")
		on.exit({
		if (!is.null(out)) close(out)
		# Remove incomplete file?
		if (!outComplete) { 
			message("REMOVING")
			file.remove(destination)
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
		}

  return(file.exists(destination))
}