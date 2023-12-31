% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/support_functions.R
\name{fCO2_growth_mod}
\alias{fCO2_growth_mod}
\title{CO2 growth modifier}
\usage{
fCO2_growth_mod(c_CO2, b = 0.5, c_ref = 360)
}
\arguments{
\item{c_CO2}{numeric Atmospheric CO2 concentration in ppm}

\item{b}{numeric Strength of CO2 effect on growth. Kellner et al. report
values bewtween 0 and 2 with the interval of highest likelihood
(0.1, 0.3). However, Soltani and Sinclair discuss that b = 0.4 in C4 plants
and b = 0.8 in C3 plants. The difference on the output of this function
of choosing a small (0.1) and large (0.8) value for b has an effect on
the result for an atmospheric concentration of 700 ppm of roughly 40
percent!.}

\item{c_ref}{numeric Reference CO2 concentration in ppm.}
}
\description{
Function describing the effects of elevated CO2 on growth.
}
\details{
The function for the effects on growth is as proposed by Soltani et al (2012)
and later adapted by equation (5) in Kellner et al. (2017)
}
\examples{
fCO2_growth_mod(420)
# The modifier is always relative to *c_ref*. This returns 1.
fCO2_growth_mod(420, c_ref = 420)

}
\references{
\insertRef{soltani2012ModelingPhysiologyCrop}{growR}

\insertRef{kellner2017CoupledHydrologicalplantGrowth}{growR}
}
