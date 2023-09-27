#'
#' The functions fPAR, fT, fW and SEA are as defined in
#'
#' Jouven, M., P. CarrEre and R. Baumont, 2006:
#'    Model predicting dynamics of biomass, structure and digestibility of
#'    herbage in managed permanent pastures. 1. Model description.
#'    Grass Forage Sci. 61, 112-124.

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
#' @export
fPAR <- function(PAR) {
  max(0., min(1., 1. - 0.0445 * (PAR - 5.))) }

#' Temperature limitation
#'
#' Threshold function representing growth limitation by temperature.
#'
#' Photosynthesis is suppressed below *T0*, increases until it reaches its 
#' maximum at temperatures in the interval [T1, T2]. For temperatures exceeding 
#' *T2*, photosynthetic activity decreases again until it reaches 0 at a 
#' final temperature of 40 degree Celsius.
#'
#' @param t float Temperature in degree Celsius.
#' @param T0 float Photosynthesis activation temperature in degree Celsius.
#' @param T1 float Photosynthesis plateau temperature in degree Celsius.
#' @param T2 float Photosynthesis max temperature in degree Celsius.
#'
#' @return A value in the range [0, 1], acting as a multiplicative factor to 
#'   plant growth.
#'
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
#' After equation (6) in McCall, D. G, und G. J Bishop-Hurley. A 
#' Pasture Growth Model for Use in a Whole-Farm Dairy Production Model, 
#' Agricultural Systems 76, Nr. 3 (1. Juni 2003): 1183 1205. 
#' https://doi.org/10.1016/S0308-521X(02)00104-X.
#' 
#' @param W Water stress given as the ratio of water reserves to water 
#'   holding capacity.
#' @param PET Potential evapotranspiration in mm per day.
#'
#' @return A value in the range [0, 1], acting as a multiplicative factor to 
#'   plant growth.
#'
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
#' (https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_gl.txt)
#'
#' @param year Calender year for which to extract CO2 concentration.
#' @return Approximate CO2 concentration in ppm for given year.
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
#' The function for the effects on growth is as proposed by 
#'
#' Soltani A. and T. R. Sinclair, 2012: Modeling Physiology of Crop
#'    Development, Growth and Yield. CABI, Wallingford, 322 pp.
#'
#' and later adapted by [equation (5)]
#'
#' Kellner, J., S. Multsch, T. Houska, P. Kraft, C. Mueller and L. Breuer, 2017:
#'    A coupled hydrological-plant growth model for simulating the effect of
#'    elevated CO2 on a temperate grassland. Agric. For. Meteotol., 246, 42-50.
#'
#' @param c_CO2 numeric Atmospheric CO2 concentration in ppm
#' @param b numeric Strength of CO2 effect on growth. Kellner et al. report 
#'   values bewtween 0 and 2 with the interval of highest likelihood 
#'  [0.1, 0.3]. However, Soltani and Sinclair discuss that b = 0.4 in C4 plants 
#'  and b = 0.8 in C3 plants. The difference on the output of this function 
#'  of choosing a small (0.1) and large (0.8) value for b has an effect on 
#'  the result for an atmospheric concentration of 700 ppm of roughly 40\%!.
#' @param c_ref numeric Reference CO2 concentration in ppm.
#'
#' @export
fCO2_growth_mod = function(c_CO2, b = 0.5, c_ref = 360) {
  return(1 + b * log(c_CO2 / c_ref))
}

#' CO2 transpiration modifier
#'
#' Function describing the effects of elevated CO2 on transpiration.
#'
#' The function for the effect on transpiration is from equations (2-6) in:
#'
#' Kruijt, B., J.-P. M. Witte, C. M. J. Jacobs and T. Kroon, 2008:
#'    Effects of rising atmospheric CO2 on evapotranspiration and soil
#'    moisture: A practical approach for the Netherlands. J. Hydrol.,
#'    349, 257-267.
#'
#' It appears that this paper that said equations are most likely incorrect. 
#' With the stated values, I cannot reproduce tabulated values of c close to 
#' 1, as in their table 3. Instead,I conclude that equation (4) should read::
#'
#'   c = 1 + s_gs * s_T * F_T * deltaCO2
#'
#' with the multiplicative terms giving small negative numbers.
#' The factors s_gs, s_T and F_T for grasslands are taken from pages 260 and 
#' 261 in Kruijt et al. where we averaged over the stated ranges to get:
#' c ~= 1 + 0.0001 * deltaCO2
#'
#' @param c_CO2 numeric Atmospheric CO2 concentration in ppm
#' @param c_ref numeric Reference CO2 concentration in ppm.
#'
#' @export
fCO2_transpiration_mod = function(c_CO2, c_ref = 360) {
  return(1 - 0.0001 * (c_CO2 - c_ref))
}

fCO2_transpiration_mod_pier = function(c_CO2, c_ref = 360) {
  return(1.1 / (1 + 0.1*c_CO2/c_ref))
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
#' intensity after Olivier Huguenin et al. 
#' Grundlagen für die Düngung landwirtschaftlicher Kulturen in der Schweiz 
#' (GRUD), Kapitel 9: Düngung von Grasland
#' ISBN 1663-7852
#' https://www.agrarforschungschweiz.ch/2017/06/9-duengung-von-grasland-grud-2017/
#'
#' @param elevation The elevation of the considered site in meters above sea 
#'   level.
#' @param intensity One of ["high", "middle", "low", "extensive". Management 
#'   intensity for considered site.
#'
#' @return Annual gross yield in t / ha (metric tons per hectare). Note that 
#'   1 t/ha = 0.1 kg/m^2.
#'
#' @export
annual_gross_yield = function(elevation, intensity = "high") {
  mask = yield_parameters$intensity == intensity
  a = yield_parameters[mask, ]$a
  b = yield_parameters[mask, ]$b
  return(a + b * max(elevation, 500))
}

#' Return the number of expected cuts for a site at a given *elevation* and 
#' management *intensity*.
#'
#' This uses :data.frame:`management_parameters` as a lookup table and 
#' interpolates linearly in between the specified values.
#'
#' @param elevation The elevation of the considered site in meters above sea 
#'   level.
#' @param intensity One of ["high", "middle", "low", "extensive". Management 
#'   intensity for considered site.
#'
#' @return Number of expected cuts per season.
#'
#' @export
expected_n_cuts = function(elevation, intensity = "high") {
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
#' supplementary material of 
#' Petersen, Krischan, David Kraus, Pierluigi Calanca, Mikhail A. 
#' Semenov, Klaus Butterbach-Bahl, and Ralf Kiese. “Dynamic Simulation 
#' of Management Events for Assessing Impacts of Climate Change on 
#' Pre-Alpine Grassland Productivity.” European Journal of Agronomy 
#' 128 (August 1, 2021): 126306. 
#' https://doi.org/10.1016/j.eja.2021.126306.
#'
#' @param DOY Integer representing the day of the year on which a cut occurs.
#'
#' @return The fraction ([0, 1]) of biomass harvested at the cut at given *DOY* 
#'   divided by the total annual biomass.
#'
#' @export
relative_cut_contribution = function(DOY) {
  return((-0.1228 * DOY + 48.96) * 1e-2)
}

#' Last day of cutting season
#'
#' Estimate the last day on which it still makes sense to cut. This is done 
#' by checking at which point the expected target biomass (see 
#' `relative_cut_contribution`) goes below the minimally harvestable standing 
#' biomass.
#'
#' @param min_biomass float A standing biomass below this value cannot even 
#'   be harvested,
#' @param elevation float Altitude in m.a.s.l.
#' @param intensity string Management intensity. One of "high", "middle", "low"
#'
#' @return float Last (fractional) day of the year on which a cut still makes 
#'   sense.
#' 
#' @seealso [relative_cut_contribution()]
#' @export
end_of_cutting_season = function(min_biomass, elevation, intensity = "high") {
  m_tot = annual_gross_yield(elevation, intensity) * 1000
  d_end = (min_biomass - m_tot * 48.96e-2) / (m_tot * -0.1228e-2)
  return(d_end)
}

