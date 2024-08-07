% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValidateControls.R
\name{validate_extraction_counts_with_totals}
\alias{validate_extraction_counts_with_totals}
\title{Validate Extraction Counts Against Blood Spot Collection Totals}
\usage{
validate_extraction_counts_with_totals(
  con,
  user_data,
  row_number_col,
  control_uid_col,
  batch_col,
  sheet_name_col,
  sheet_barcode_col
)
}
\arguments{
\item{con}{A database connection object.}

\item{user_data}{A dataframe containing the new extractions to be validated.
It must contain columns that can be used to derive blood_spot_collection_id indirectly.}

\item{row_number_col}{The name of the column in \code{user_data} that provides a unique row identifier.}

\item{control_uid_col}{The name of the column in \code{user_data} that corresponds to the control UID.}

\item{batch_col}{The name of the column in \code{user_data} related to the batch information.}

\item{sheet_name_col}{The name of the column in \code{user_data} for the sheet name.}

\item{sheet_label_col}{The name of the column in \code{user_data} for the sheet label, which may contain NA values.}
}
\value{
An \code{ErrorData} object if any blood spot collection's new total extractions exceed its total counts,
detailing the violations. Returns \code{NULL} if all new extraction counts are within the allowed limits.
}
\description{
This function checks if the sum of new extractions for each blood spot collection
in the provided user data does not exceed the total count available for each collection.
It ensures that extractions are within the limits of available blood spots by dynamically
joining user data with related tables to identify the relevant blood_spot_collection_id for each record.
}
