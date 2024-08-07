% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampleDB_Setup.R
\name{finalize_upgrade}
\alias{finalize_upgrade}
\title{Finalize the database upgrade}
\usage{
finalize_upgrade(database, new_database, con)
}
\arguments{
\item{database}{Path to the original database.}

\item{new_database}{Path to the upgraded database.}

\item{con}{Connection to new database}
}
\value{
None.
}
\description{
This function finalizes the database upgrade by:
\enumerate{
\item Removing unused pages.
\item Closing the database connection.
\item Copying the new database to the original path.
\item Changing file permissions.
}
}
\keyword{internal}
\keyword{setup}
