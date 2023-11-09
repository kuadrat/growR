#' Radiation limitation
#'
#' Threshold function representing growth limitation due to lack of 
#' photosynthetically active radiation (PAR).
#'
#' @param PAR float Photosynthetically active radiation in MJ/m^2
#'
#' @return A value in the range [0, 1], acting as a multiplicative factor to 
#'   plant growth.
#'
#' @examples
#' fPAR(4)
#'
#' @export
fPAR <- function(PAR) {
  max(0., min(1., 1. - 0.0445 * (PAR - 5.))) }

#' Temperature limitation
#'
#' Threshold function representing growth limitation by temperature.
#'
#' Photosynthesis is suppressed below *T0*, increases until it reaches its 
#' maximum at temperatures in the interval (T1, T2). For temperatures exceeding 
#' *T2*, photosynthetic activity decreases again until it reaches 0 at a 
#' final temperature of 40 degree Celsius.
#'
#' @param t float Temperature in degree Celsius.
#' @param T0 float Photosynthesis activation temperature in degree Celsius.
#' @param T1 float Photosynthesis plateau temperature in degree Celsius.
#' @param T2 float Photosynthesis max temperature in degree Celsius.
#'
#' @return A value in the range (0, 1), acting as a multiplicative factor to 
#'   plant growth.
#'
#' @examples
#' fT(4)
#' fT(10)
#' fT(15)
#'
#' @md
#' @export
fT <- function(t, T0 = 4, T1 = 10, T2 = 20) {
  if (t < T0) {
    return(0.)
  } else if (t < T1) {
    return((t - T0) / (T1 - T0))
  } else if (t < T2) {
    return(1)
  } else if (t < 40) {
    return((40 - t) / (40 - T2))
  } else {
    return(0)
  }
}

#' Water stress
#'
#' Threshold function representing growth limitation due to water stress.
#'
#' After equation (6) in McCall et al. (2003).#' 
#'
#' @param W Water stress given as the ratio of water reserves to water 
#'   holding capacity.
#' @param PET Potential evapotranspiration in mm per day.
#'
#' @return A value in the range (0, 1), acting as a multiplicative factor to 
#'   plant growth.
#'
#' @examples
#' fW(0.5, 7)
#' fW(0.5, 5)
#' fW(0.5, 3)
#'
#' @references
#' \insertRef{mccall2003PastureGrowthModel}{growR}
#'
#' @md
#' @export
fW <- function(W, PET) {
  # High values of PET
  if (PET > 6.5) {
    result = W
  # Values of PET between 3.81 and 6.5
  } else if (PET > 3.81) {
    # Choose a different result, depending on which 0.2 - wide interval W is in.
    selector = 1 + floor(W / 0.2)
    result = switch(selector,
                    2.0 * W,
                    1.5 * W + 0.1,
                    1.0 * W + 0.3,
                    0.5 * W + 0.6,
                    1,
                    1)
  # Values of PET below 3.81
  } else {
    # Choose a different result, depending on which 0.2 - wide interval W is in.
    selector = 1 + floor(W / 0.2)
    result = switch(selector,
                    4.00 * W,
                    0.75 * W + 0.65,
                    0.25 * W + 0.85,
                    1,
                    1,
                    1)

  }
  return(result)
}

#' Seasonal effect on growth
#'
#' Function representing the strategy of plants adjusting their roots:shoots 
#' ratios during the season.
#'
#' @param ST float Temperature sum in degree(C)-days.
#' @param minSEA float < 1. Minimum value of SEA.
#' @param maxSEA float > 1. Maximum value of SEA.
#' @param ST1 float Temperature sum after which SEA declines from the maximum 
#'   plateau.
#' @param ST2 float Temperature sum after which SEA reaches and remains at 
#'   its minimum.
#'
#' @examples
#' SEA(800)
#'
#' @export
SEA <- function(ST, minSEA = 0.65, maxSEA = 1.35, ST1 = 800, ST2 = 1450) {
  if (ST < 200.) {
    return(minSEA)
  } else if (ST < (ST1 - 200.)) {
    return(minSEA + (maxSEA - minSEA) * ( ST - 200. ) / ( ST1 - 400. ))
  } else if (ST < (ST1 - 100.)) {
    return(maxSEA) 
  } else if (ST < ST2) {
    return(maxSEA + (minSEA - maxSEA) * (ST - ST1 + 100) / (ST2 - ST1 + 100))
  } else {
    return(minSEA)
  }
}

#-CO2-concentration-------------------------------------------------------------

#' Atmospheric CO2 concentration
#'
#' Retrieve CO2 concentration (in ppm) for given calendar year.
#'
#' This function defines the CO2 concentration as a function of calendar
#' year. it is based on a polynomial fit to the annual CO2 data
#' published by NOAA
#' <https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_gl.txt>
#'
#' @note This is only approximately valid for years in the range 1949 - 2020
#' @param year Calender year for which to extract CO2 concentration.
#' @return Approximate CO2 concentration in ppm for given year.
#'
#' @examples
#' atmospheric_CO2(1990)
#' atmospheric_CO2(2020)
#' # Insensible
#' atmospheric_CO2(1800)
#'
#' @export
atmospheric_CO2 = function(year) {
  1.56961E-02*year^2 - 6.09620E+01*year + 5.95087E+04
}

#' Concentration representative year
#'
#' Inverse of `atmospheric_CO2`: retrieve the year by which a given CO2 
#' concentration is reached.
#'
#' Does not give a reasonable result for values below 317ppm, corresponding 
#' to the year 1949, as this is where the minimum of the parabola is located 
#' in the second order fit to the data that was used in aCO2.fct.
#'
#' @param aCO2 Target CO2 concentration in ppm.
#' @return year Approximate year (as floating point number) by which target 
#'   concentration is reached.
#'
#' @examples
#' aCO2_inverse(420)
#' aCO2_inverse(700)
#' # Insensible
#' aCO2_inverse(100)
#'
#' @export
aCO2_inverse = function(aCO2) {
  a = 1.56961E-02
  b = -6.09620E+01
  c = 5.95087E+04 - aCO2
  return((-b + sqrt(b^2 - 4*a*c)) / (2*a))
}

#' CO2 growth modifier
#'
#' Function describing the effects of elevated CO2 on growth.
#'
#' @details
#' The function for the effects on growth is as proposed by Soltani et al (2012)
#' and later adapted by equation (5) in Kellner et al. (2017)
#'
#' @param c_CO2 numeric Atmospheric CO2 concentration in ppm
#' @param b numeric Strength of CO2 effect on growth. Kellner et al. report 
#'   values bewtween 0 and 2 with the interval of highest likelihood 
#'  (0.1, 0.3). However, Soltani and Sinclair discuss that b = 0.4 in C4 plants 
#'  and b = 0.8 in C3 plants. The difference on the output of this function 
#'  of choosing a small (0.1) and large (0.8) value for b has an effect on 
#'  the result for an atmospheric concentration of 700 ppm of roughly 40 
#'  percent!.
#' @param c_ref numeric Reference CO2 concentration in ppm.
#'
#' @examples
#' fCO2_growth_mod(420)
#' # The modifier is always relative to *c_ref*. This returns 1.
#' fCO2_growth_mod(420, c_ref = 420)
#'
#' @references
#' \insertRef{soltani2012ModelingPhysiologyCrop}{growR}
#'
#' \insertRef{kellner2017CoupledHydrologicalplantGrowth}{growR}
#'
#' @md
#' @export
fCO2_growth_mod = function(c_CO2, b = 0.5, c_ref = 360) {
  return(1 + b * log(c_CO2 / c_ref))
}

#' CO2 transpiration modifier
#'
#' Function describing the effects of elevated CO2 on transpiration.
#'
#' The function for the effect on transpiration is from equations (2-6) in 
#' Kruijt et al.
#'
#' It appears that this paper that said equations are most likely incorrect. 
#' With the stated values, I cannot reproduce tabulated values of c close to 
#' 1, as in their table 3. Instead,I conclude that equation (4) should read:
#' ```
#'   c = 1 + s_gs * s_T * F_T * deltaCO2
#' ```
#'
#' with the multiplicative terms giving small negative numbers.
#' The factors s_gs, s_T and F_T for grasslands are taken from pages 260 and 
#' 261 in Kruijt et al. where we averaged over the stated ranges to get:
#' ```
#'   c ~= 1 + 0.0001 * deltaCO2
#' ```
#'
#' @param c_CO2 numeric Atmospheric CO2 concentration in ppm
#' @param c_ref numeric Reference CO2 concentration in ppm.
#'
#' @md
#' @examples
#' fCO2_transpiration_mod(420)
#' # The modifier is always relative to *c_ref*. This returns 1.
#' fCO2_transpiration_mod(420, c_ref = 420)
#'
#' @references
#' \insertRef{kruijt2008EffectsRisingAtmospheric}{growR}
#'
#' @md
#' @export
fCO2_transpiration_mod = function(c_CO2, c_ref = 360) {
  return(1 - 0.0001 * (c_CO2 - c_ref))
}

# These other functions, which address the effect of elevated CO2 on
# stomatal conductace and temperature responses, are not used. They are taken 
# from:
#
# Thornley, J.H.M., 1998: Grassland Dynamics -  An Ecosystem Simulation Model.
#    CABI, Wallingford, 241 pp.
fC.gs = function(x){1} # function(x){1.5/(1 + .5*x/360)}
fC.Tt = function(x){0} # function(x){5*(x - 360)/(720 - 360)}
fC.ST = function(x){1} # function(x){1 + .1*(x - 360)/(720 - 360)}

#' Lookup table returning expected annual gross yields as function of 
#' elevation and management intensity.
#'
#' Based on data from Table 1a in
#' Lookup Table of expected yield as functions of height and management 
#' intensity after Olivier Huguenin et al. (2017).
#'
#' @param elevation The elevation of the considered site in meters above sea 
#'   level.
#' @param intensity One of ("high", "middle", "low", "extensive"). Management 
#'   intensity for considered site.
#'
#' @return Annual gross yield in t / ha (metric tons per hectare). Note that 
#'   1 t/ha = 0.1 kg/m^2.
#'
#' @md
#' @examples
#' get_annual_gross_yield(1200)
#' get_annual_gross_yield(1200, intensity = "low")
#'
#' @references
#' \insertRef{huguenin2017GrundlagenDuengung}{growR}
#' @md
#' @export
get_annual_gross_yield = function(elevation, intensity = "high") {
  mask = yield_parameters$intensity == intensity
  a = yield_parameters[mask, ]$a
  b = yield_parameters[mask, ]$b
  return(a + b * max(elevation, 500))
#  return(1)
}

#' Get number of expected cuts
#'
#' Return the number of expected cuts for a site at a given *elevation* and 
#' management *intensity*.
#'
#' This uses data.frame `management_parameters` as a lookup table and 
#' interpolates linearly in between the specified values.
#'
#' @param elevation The elevation of the considered site in meters above sea 
#'   level.
#' @param intensity One of ("high", "middle", "low", "extensive"). Management 
#'   intensity for considered site.
#'
#' @return Number of expected cuts per season.
#'
#' @md
#' @examples
#' get_expected_n_cuts(1200)
#' get_expected_n_cuts(1200, intensity = "low")
#'
#' @export
get_expected_n_cuts = function(elevation, intensity = "high") {
  mask = management_parameters$intensity == intensity
  # Find the altitudes below and above given elevation
  altitudes = management_parameters[mask, ]$altitude
  n_cuts = management_parameters[mask, ]$n_cuts
  i0 = which.min(abs(altitudes - elevation))
  alt0 = altitudes[i0]
  n0 = n_cuts[i0]
  # Check for second closest altitude
  i1 = which.min(abs(altitudes[-i0] - elevation))
  alt1 = altitudes[-i0][i1]
  n1 = n_cuts[-i0][i1]
  # Approximate linearly
  n = (n1 - n0) / (alt1 - alt0) * (elevation - alt0) + n0
  return(n)
}

#' Relative cut contribution
#'
#' Get the fraction of the total annual harvested biomass that a cut at given 
#' *DOY* is expected to contribute.
#'
#' The regression for the target biomass is based on Fig. S2 in the 
#' supplementary material of Petersen et al. (2021).
#'
#' @param DOY Integer representing the day of the year on which a cut occurs.
#'
#' @return The fraction (between 0 and 1) of biomass harvested at the cut at 
#'   given *DOY* divided by the total annual biomass.
#'
#' @md
#' @examples
#' get_relative_cut_contribution(1)
#' get_relative_cut_contribution(150)
#' get_relative_cut_contribution(365)
#' # DOYs larger than 365 are insensible
#' get_relative_cut_contribution(600)
#'
#' @references
#' \insertRef{petersen2021DynamicSimulationManagement}{growR}
#' @md
#' @export
get_relative_cut_contribution = function(DOY) {
  return((-0.1228 * DOY + 48.96) * 1e-2)
}

#' Last day of cutting season
#'
#' Estimate the last day on which it still makes sense to cut. This is done 
#' by checking at which point the expected target biomass (see 
#' [get_relative_cut_contribution()]) goes below the minimally harvestable 
#' standing biomass.
#'
#' @param min_biomass float A standing biomass below this value cannot even 
#'   be harvested,
#' @param elevation float Altitude in m.a.s.l.
#' @param intensity string Management intensity. One of "high", "middle", "low"
#'
#' @return float Last (fractional) day of the year on which a cut still makes 
#'   sense.
#' 
#' @seealso [get_relative_cut_contribution()]
#' 
#' @md
#' @examples
#' get_end_of_cutting_season(50, 1200)
#' get_end_of_cutting_season(50, 1200, intensity = "low")
#'
#' @export
get_end_of_cutting_season = function(min_biomass, elevation, 
                                     intensity = "high") {
  m_tot = get_annual_gross_yield(elevation, intensity) * 1000
  d_end = (min_biomass - m_tot * 48.96e-2) / (m_tot * -0.1228e-2)
  return(d_end)
}

