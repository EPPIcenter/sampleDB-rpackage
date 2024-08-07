% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AppData.R
\name{get_sample_action_types}
\alias{get_sample_action_types}
\title{Get action types for a specific sample}
\usage{
get_sample_action_types(
  sample_type,
  sample_file = "samples.json",
  app_file = "app.json"
)
}
\arguments{
\item{sample_type}{A character string representing the type of the sample.}

\item{sample_file}{A character string indicating the path to the samples.json file.}

\item{app_file}{A character string indicating the path to the app.json file.}
}
\value{
A named list of action types or NULL if no actions are defined.
}
\description{
Get action types for a specific sample
}
\examples{
get_sample_action_types("micronix")
}
