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
  # Values of PET between 3.8 and 6.5
  } else if (PET > 3.8) {
    # Choose a different result, depending on which 0.2 - wide interval W is in.
    selector = 1 + floor(W / 0.2)
    result = switch(selector,
                    2.0 * W,
                    1.5 * W + 0.1,
                    1.0 * W + 0.3,
                    0.5 * W + 0.6,
                    1,
                    1)
  # Values of PET below 3.8
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
#' It appears in this paper that there is a small formal mistake in said 
#' equations. 
#' With the stated values, it is not possible to reproduce the tabulated 
#' values of $c$ close to 1, as in their table 3. Instead, we conclude that 
#' equation (4) should read:
#'
#' ```
#'   c = 1 + s_gs * s_T * F_T * deltaCO2
#' ```
#'
#' with the multiplicative terms giving small negative numbers.
#' The factors $s_gs$, $s_T$ and $F_T$ for grasslands are taken from pages 
#' 260 and 261 in Kruijt et al. where we averaged over the stated ranges to 
#' get: 
#'
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

#' Create a weighted temperature sum
#'
#' A temperature sum is constructed by summing the average daily temperature 
#' for each day, but applying a weight factor of 0.5 for January and 0.75 for 
#' February.
#'
#' @param temperatures vector Daily average temperatures in degree Celsius.
#' @param negative boolean Whether to include negative temperature values in 
#'   the summation. By default, negative values are set to 0, meaning that 
#'   the temperature sum is monotonically increasing.
#'
#' @return Weighted temperature sum.
#'
#' @examples
#' # Use fake temperatures
#' ts = rep(2, 365)
#' weighted_temperature_sum(ts)
#' 
#' @export
weighted_temperature_sum = function(temperatures, negative = FALSE) {
  weights = c(rep(0.5, 31), rep(0.75, 28), rep(1, length(temperatures)-28-31))
  if (!negative) {
    # Set negative values to 0
    temperatures = sapply(temperatures, function(t) { max(t, 0) })
  }
  weighted = temperatures * weights
  return(cumsum(weighted))
}

#' Determine start of growing season
#'
#' This implements a conventional method for the determination of the start of 
#' the growing season (SGS) based on daily average temperatures.
#'
#' A temperature sum is constructed using [weighted_temperature_sum()], i.e. 
#' by summing the average daily temperature for each day, but applying a 
#' weight factor of 0.5 for January and 0.75 for February.
#'
#' The SGS is defined as the first day where the so constructed temperature sum 
#' crosses 200 degree days.
#' 
#' @param temperatures vector Daily average temperatures in degree Celsius.
#'
#' @seealso [start_of_growing_season_mtd()], [weighted_temperature_sum()]
#'
#' @examples
#' ts = rep(2, 365)
#' start_of_growing_season(ts)
#'
#' @export
start_of_growing_season = function(temperatures) {
  # Create weighted temperature sum
  TS = weighted_temperature_sum(temperatures)
  # Find where TS > 200
  j0 = min(which(TS > 200))    
  return(j0)
}

#' Multicriterial Thermal Definition
#'
#' Find the start of the growing season based on daily average temperatures.
#'
#' This function implements the *multicriterial thermal definition* (MTD) 
#' as described in chapter 2.3.1.3 of the dissertation of Andreas 
#' Schaumberger:
#' Räumliche Modelle zur Vegetations- und Ertragsdynamik im 
#' Wirtschaftsgrünland, 2011, ISBN-13: 978-3-902559-67-8
#'
#' @param temperatures vector Daily average temperatures in degree Celsius.
#' @param first_possible_DOY int Only consider days of the year from this 
#'   value onward.
#' @return int DOY of the growing season start according to the MTD.
#'
#' @seealso [start_of_growing_season()]
#'
#' @examples
#' # Create fake temperatures
#' ts = c(0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 6, 6, 6, 6, 3, 3, 3, 3, 3, 6, 6, 6, 
#' 6, 5, 6, 7, 8, 9, 10, 11, 12)
#' start_of_growing_season_mtd(ts)
#'
#' @export
start_of_growing_season_mtd = function(temperatures,
                                       first_possible_DOY = 1) {
  n_days = length(temperatures)
  result = 0
  outer_window_width = 10
  inner_window_width = 5
  critical_temperature = 5.
  min_daily_temperature = 2.
  min_window_temperature = 6.
  # For the multicriterial thermal definition, start with  an outer 
  # sliding window of 10 days
  for (j in first_possible_DOY:(n_days - outer_window_width)) {
    outer_window = temperatures[j:(j + outer_window_width - 1)]
    # If there are frosts, move the outer window.
    # Likewise, if the average temperature is too low, move the outer window.
    if (any(outer_window < min_daily_temperature) | 
        (mean(outer_window) < min_window_temperature)) {
      next
    }
    # If there are no frosts and the average T is high enough, check if 
    # there is a suitable inner window.
    for (j_inner in 1:(outer_window_width - inner_window_width + 1)) {
      inner_window = outer_window[j_inner:(j_inner + inner_window_width - 1)]
      if (all(inner_window > critical_temperature)) {
        result = j + j_inner - 1
        break
      }
    }
    # Leave the outer loop if j_t_critical has been found.
    if (result != 0) {
      break
    }
  }
  return(result)
}
