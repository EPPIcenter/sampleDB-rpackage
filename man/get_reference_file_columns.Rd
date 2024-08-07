% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AppData.R
\name{get_reference_file_columns}
\alias{get_reference_file_columns}
\title{Retrieve ColumnData for a given reference type.}
\usage{
get_reference_file_columns(
  reference_type,
  config_yml = Sys.getenv("SDB_CONFIG"),
  references_file = "references.json"
)
}
\arguments{
\item{reference_type}{Type of the reference for which the columns are needed.}

\item{config_yml}{(default: Sys.getenv("SDB_CONFIG")) Path to the application config file}

\item{references_file}{(default: "references.json") Path to the JSON file containing references data.}
}
\value{
A ColumnData S3 object.
}
\description{
This function provides the ColumnData S3 object for a specific reference type.
}
