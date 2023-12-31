% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{get_site_name}
\alias{get_site_name}
\title{Extract the name of a site from a filename}
\usage{
get_site_name(filename)
}
\arguments{
\item{filename}{String of a `ModvegeSite` output filename.}
}
\description{
This function assumes the filenames to begin with the site name, 
potentially followed by an underscore and further characters.
}
