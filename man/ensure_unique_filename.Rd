% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{ensure_unique_filename}
\alias{ensure_unique_filename}
\title{Replace given filename by a version that contains an incremental number 
in order to prevent overwriting existing files.}
\usage{
ensure_unique_filename(path, add_num = TRUE)
}
\arguments{
\item{path}{string; Filename including path for which to check uniqueness.}

\item{add_num}{boolean; if TRUE, add the incremental number anyways, even 
if no filename conflict exists.}
}
\value{
A unique filename.
}
\description{
Replace given filename by a version that contains an incremental number 
in order to prevent overwriting existing files.
}
