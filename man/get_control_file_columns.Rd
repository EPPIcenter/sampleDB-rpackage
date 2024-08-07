% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AppData.R
\name{get_control_file_columns}
\alias{get_control_file_columns}
\title{Retrieve ColumnData for a given control type and action}
\usage{
get_control_file_columns(
  control_type,
  action,
  file_type = "na",
  config_yml = Sys.getenv("SDB_CONFIG"),
  controls_file = "controls.json"
)
}
\arguments{
\item{control_type}{Type of the control for which the columns are needed.}

\item{action}{Desired action.}

\item{file_type}{The file type being used. Default is 'na'.}

\item{config_yml}{(default: Sys.getenv("SDB_CONFIG")) Path to the application config file.}

\item{controls_file}{(default: "controls.json") Path to the JSON file containing controls data.}
}
\value{
A ColumnData S3 object.
}
\description{
This function provides the ColumnData S3 object for a specific control type and action.
}
