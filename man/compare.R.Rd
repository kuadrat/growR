\name{compare.R}
\alias{compare.R}
\title{Compare simulation results}
\description{
The script \code{compare.R} which ships with the growR package and is 
automatically put into the working directory with 
\link[=setup_directory]{setup_directory()} 
can be used to compare results of growR simulation runs.

It is a simple script, so it can and should be adjusted to your personal needs.
}
\details{
The script makes use of the packages ggplot2, patchwork and some growR 
functions which facilitate data loading and plotting, like 
\link[=load_measured_data]{load_measured_data()} and 
\link[=add_lines]{add_lines()}.
}

