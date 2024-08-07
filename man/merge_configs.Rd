% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampleDB_Setup.R
\name{merge_configs}
\alias{merge_configs}
\title{Merge Configuration Lists}
\usage{
merge_configs(current_config, new_config)
}
\arguments{
\item{current_config}{A list representing the current configuration.}

\item{new_config}{A list representing the new configuration.}
}
\value{
A merged list containing elements from both configurations.
}
\description{
This function merges two configuration lists: a current configuration and a new configuration.
It will return a merged configuration, preferring values from the current configuration when they exist and are not NA.
}
\examples{
\dontrun{
current <- list(version = "1.0.0", key = "old_value")
new <- list(version = "1.1.0", key = NA, new_key = "new_value")
merged <- merge_configs(current, new)
print(merged)
}
}
