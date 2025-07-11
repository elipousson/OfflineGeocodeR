% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docker.R
\name{gc_call}
\alias{gc_call}
\title{call a running geocoding container to geocode an address}
\usage{
gc_call(address, version = getOption("offlinegeocoder.version", "3.4.0"))
}
\arguments{
\item{address}{a string; see vignette for best practices and examples}

\item{version}{Version number. Defaults to \code{getOption("offlinegeocoder.version", "3.4.0")}. Version must match version specified by \code{image_name} parameter}
}
\description{
This is used internally by \code{geocode()} and normally should not be called directly.
}
