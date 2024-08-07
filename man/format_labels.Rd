% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UploadControls.R
\name{format_labels}
\alias{format_labels}
\title{Format labels for compositions in a dplyr pipeline}
\usage{
format_labels(
  data,
  legacy_col = "legacy",
  label_col = "label",
  index_col = "index"
)
}
\arguments{
\item{data}{A tibble containing the compositions to be labeled.
The dataframe should contain the columns \code{legacy}, \code{label}, and \code{index}.}

\item{legacy_col}{The name of the column that contains the legacy flag. Default is "legacy".}

\item{label_col}{The name of the column that contains the labels. Default is "label".}

\item{index_col}{The name of the column that contains the index. Default is "index".}
}
\value{
A tibble containing the formatted labels.
}
\description{
This function formats the labels for the given compositions based on the legacy status and index.
The function is designed to work within a dplyr pipeline.
}
\examples{
df <- tibble(legacy = c(1, 0), label = c("S1", "S2"), index = c(1, 2))
df \%>\% format_labels()
}
