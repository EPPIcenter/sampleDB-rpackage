% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampleDB_Setup.R
\name{generate_upgrade_script_path}
\alias{generate_upgrade_script_path}
\title{Generate Path to Database Upgrade Script}
\usage{
generate_upgrade_script_path(current_version_idx, db_versions, pkgname)
}
\arguments{
\item{current_version_idx}{Index of the current database version.}

\item{db_versions}{A vector of available database versions.}

\item{pkgname}{Name of the package where the SQL scripts are located.}
}
\value{
A string representing the file path to the SQL upgrade script.
}
\description{
This function generates the file path to the SQL upgrade script based on the current version index,
available database versions, and package name.
}
