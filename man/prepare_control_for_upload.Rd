% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UploadControls.R
\name{prepare_control_for_upload}
\alias{prepare_control_for_upload}
\title{Prepare Control for Upload}
\usage{
prepare_control_for_upload(user_data, now)
}
\arguments{
\item{user_data}{A dataframe with user data containing 'Density' and 'CompositionID' columns.}

\item{now}{A timestamp indicating the current time.}
}
\value{
A dataframe with additional columns 'Control', 'Created', and 'LastUpdated'.
}
\description{
This function prepares a user_data dataframe for upload by adding a 'Control'
column (generated from Density and CompositionID values), and by adding 'Created'
and 'LastUpdated' columns with the current timestamp.
}
\examples{
test_data <- data.frame(Density = c(100, 1000, 1500),
                        CompositionID = c("S1_1", "S3_2", "S2_2"))
prepare_control_for_upload(test_data, Sys.time())

}
