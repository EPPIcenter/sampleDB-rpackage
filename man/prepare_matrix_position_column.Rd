% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProcessCSV.R
\name{prepare_matrix_position_column}
\alias{prepare_matrix_position_column}
\title{Modify the user column for validation purposes}
\usage{
prepare_matrix_position_column(
  user_data,
  dimensions,
  expected_position_column,
  position_col
)
}
\arguments{
\item{user_data}{The users uploaded and formatted data.}

\item{dimensions}{Dimensions of the matrix to check against.}

\item{expected_position_column}{Expected position column names(s)}

\item{position_col}{The name of the concatenated position to be used with validation}
}
\value{
The users updated data frame.
}
\description{
Modify the user column for validation purposes
}
\keyword{internal}
\keyword{validation}
