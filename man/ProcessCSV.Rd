% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProcessCSV.R
\name{ProcessCSV}
\alias{ProcessCSV}
\title{Checks the format of a csv file and validates the data given sample type and file type parameters}
\usage{
ProcessCSV(
  user_csv,
  user_action,
  sample_storage_type,
  search_type = NULL,
  container_name = NULL,
  freezer_address = NULL,
  file_type = "na",
  validate = TRUE,
  database = Sys.getenv("SDB_PATH"),
  config_yml = Sys.getenv("SDB_CONFIG")
)
}
\arguments{
\item{user_csv}{The path to the user file on disk}

\item{user_action}{The action that will be taken. This can be 'upload', 'move' or 'search'.}

\item{sample_storage_type}{The type of storage the samples are in. This can be '1', '2' or '3', which identify 'Micronix', 'Cryovial' or 'DBS', respectively.}

\item{container_name}{Optional parameter to specify the container the samples are being added to. This is not required if the container name is specifed in your file.}

\item{freezer_address}{The location of the container as a named list \code{list(name=NULL, level_I=NULL, level_II=NULL)}. This is not required in the location is specified in file.}

\item{file_type}{The file type. This usually will be 'na'. The default value is 'na'.}

\item{validate}{Whether to validate the data. Setting to \code{FALSE} will skip the validation step and will only check the file format. Default is \code{TRUE}.}

\item{database}{Path to the sampleDB database. Default is \code{Sys.getenv("SDB_PATH")}.}

\item{config_yml}{Path to the user configuration file. Default is \code{Sys.getenv("SDB_CONFIG")}.}
}
\description{
Process your CSV to check for appropriate columns and data validation.
}
\details{
\code{ProcessCSV()} creates an intermediate dataframe from a user provided csv that can then be passed to UploadSamples or MoveSamples:
}
\examples{
\dontrun{
 # Format a sample datasheet with micronix samples using the 'na' micronix format that will be added to sampleDB. Add the container name and container location as parameters.
 formatted_csv <- ProcessCSV(
   user_csv = "/path/to/sample_data.csv",
   user_action = "upload",
   file_type = "na",
   sample_storage_type = "micronix",
   container_name="LN2_XXXX",
   freezer_address=list(name="FreezerA", level_I="Shelf1", level_II="BasketC")
  )
}
}
