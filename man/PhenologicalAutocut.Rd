% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autocut.R
\name{PhenologicalAutocut}
\alias{PhenologicalAutocut}
\title{Autocut based on phenology}
\description{
An algorithm to determine grass cut dates if none are provided.
This uses empirical data for Switzerland to determine the first and last
cut dates of the season from meteorological data.
The number of cuts is inferred from Huguenen et al. and these cut events
are distributed equally between first and last cut dates.
}
\references{
\insertRef{huguenin2017GrundlagenDuengung}{growR}
}
\seealso{
\link{management_parameters}, \link{PetersenAutocut}

\link{Autocut}
}
\section{Super class}{
\code{growR::Autocut} -> \code{PhenologicalAutocut}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{cut_DOYs}}{vector containing the integer day-of-year's on which cuts
occur.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PhenologicalAutocut-new}{\code{PhenologicalAutocut$new()}}
\item \href{#method-PhenologicalAutocut-determine_cut}{\code{PhenologicalAutocut$determine_cut()}}
\item \href{#method-PhenologicalAutocut-clone}{\code{PhenologicalAutocut$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="growR" data-topic="Autocut" data-id="get_expected_n_cuts"><a href='../../growR/html/Autocut.html#method-Autocut-get_expected_n_cuts'><code>growR::Autocut$get_expected_n_cuts()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PhenologicalAutocut-new"></a>}}
\if{latex}{\out{\hypertarget{method-PhenologicalAutocut-new}{}}}
\subsection{Method \code{new()}}{
Constructor

Valid cut dates are calculated upon initialization.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PhenologicalAutocut$new(MVS)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{MVS}}{The \link{ModvegeSite} object for which cuts shall be
determined.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PhenologicalAutocut-determine_cut"></a>}}
\if{latex}{\out{\hypertarget{method-PhenologicalAutocut-determine_cut}{}}}
\subsection{Method \code{determine_cut()}}{
Does a cut occur on \emph{DOY}?

Check if \emph{DOY} is in \code{self$cut_DOYs}. If so, return \code{TRUE}. Return
\code{FALSE} otherwise.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PhenologicalAutocut$determine_cut(DOY)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DOY}}{Integer day of the year (1-366).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Boolean
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PhenologicalAutocut-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PhenologicalAutocut-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PhenologicalAutocut$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
