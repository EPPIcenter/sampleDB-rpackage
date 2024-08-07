% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Database.R
\name{get_strains}
\alias{get_strains}
\title{Get all strains from the database}
\usage{
get_strains(con)
}
\arguments{
\item{con}{A database connection}
}
\value{
A character vector of all strains in the database

A character vector of all strains in the database
}
\description{
Get all strains from the database
}
\examples{
con <- dbConnect(RSQLite::SQLite(), ":memory:")
get_strains(con)
dbDisconnect(con)
}
\keyword{database}
\keyword{utility}
