% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utils.R
\name{validate_specimen_type_db}
\alias{validate_specimen_type_db}
\title{Validate specimen type references (Database Version)}
\usage{
validate_specimen_type_db(con, table_name, row_number_col, specimen_type_col)
}
\arguments{
\item{con}{A database connection object.}

\item{table_name}{The name of the formatted CSV table in the database.}

\item{row_number_col}{The name of the row number column.}

\item{specimen_type_col}{The name of the specimen type column.}
}
\value{
ErrorData object indicating any specimen types not found.
}
\description{
This function checks if the specimen type reference provided in the dataset exists in the database.
Note: This function assumes that the data is already present in the database.
}
\keyword{validation}
