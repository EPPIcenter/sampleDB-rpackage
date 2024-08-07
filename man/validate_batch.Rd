% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValidateReferences.R
\name{validate_batch}
\alias{validate_batch}
\title{Validate Batch Data}
\usage{
validate_batch(user_data, action, database)
}
\arguments{
\item{user_data}{A data frame containing control data to validate.}

\item{action}{The action being performed, either "create" or "extraction".}

\item{database}{The database connection or specification to use for validation.}
}
\value{
A list containing validation errors, if any.
}
\description{
This function conducts a series of validation checks on Strains data.
The function will return any errors encountered during validation.
}
\keyword{strain}
\keyword{validation,}
