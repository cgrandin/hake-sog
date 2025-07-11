% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-load-sample-data.R
\name{load_sample_data}
\alias{load_sample_data}
\title{Load the Canadian commercial sample data}
\usage{
load_sample_data(
  fn = file.path("/srv/hake-sog/other/samples/hake-sog-2025-commercial-samples.rds"),
  gear_type = "midwater trawl"
)
}
\arguments{
\item{fn}{The file name for the sample RDS file}

\item{gear_type}{A vector of fishing gear type (description). Must be
one or more of: \verb{midwater trawl}, \verb{bottom trawl}, \verb{shrimp trawl},
or \verb{unknown trawl}. If \code{NULL}, all type will be included}
}
\value{
A data frame containing filtered Canadian samples
}
\description{
Load the Canadian commercial sample data from an RDS file generated by
\code{\link[=canada_extract_samples_from_db]{canada_extract_samples_from_db()}}. The filename is stored in the package
data variable \link{can_sample_data_rds_fn}
}
\details{
The following filtering is performed in this function for all fleets:
\enumerate{
\item Areas - BC offshore areas + Strait of Juan de Fuca
\item Fishing gear - Midwater trawl only by default
}
}
