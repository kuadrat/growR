#' Autocut
#'
#' @description
#' An algorithm to determine grass cut dates if none are provided.
#' This is an abstract class and not intended for direct use. Instead, use 
#' its subclasses that implement a `determine_cut()` method.
#'
#' The expected number of cuts is estimated from management intensity and 
#' site altitude based on data for Swiss grasslands by Huguenin et al.
#'
#' @seealso
#' [management_parameters]
#'
#' @references
#' \insertRef{huguenin2017GrundlagenDuengung}{growR}
#'
#' @md
#' @export
Autocut = R6Class(
  "Autocut",
  public = list(
#' @field MVS The [ModvegeSite] object for which to take the cut decision.
    MVS = NULL,
#' @field n_cuts Number of cuts expected for this altitude and management 
#'   intensity.
    n_cuts = NULL,

    #' @description Constructor
    #'
    #' @param MVS The [ModvegeSite] object for which cuts shall be 
    #'   determined.  
    #'
    initialize = function(MVS) {
      self$MVS = MVS
      self$n_cuts = self$get_expected_n_cuts(MVS$parameters$ELV, 
                                             MVS$management$intensity)
    },

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
    },

    #' @description
    #' Empty method stub intended for overriding by inheriting subclasses.
    #'
    #' @param DOY Integer day-of-the-year.
    determine_cut = function(DOY) {
      stop(paste("`Autocut` is an abstract base class that does not implement the",
           "`determine_cut` method. Use a subclass of `Autocut` instead."))
    }
## End of public attributes
  )
)

#' Autocut based on phenology
#'
#' @description
#' An algorithm to determine grass cut dates if none are provided.
#' This uses empirical data for Switzerland to determine the first and last 
#' cut dates of the season from meteorological data.
#' The number of cuts is inferred from Huguenen et al. and these cut events 
#' are distributed equally between first and last cut dates.
#'
#' @seealso [management_parameters], [PetersenAutocut]
#'
#' @references
#' \insertRef{huguenin2017GrundlagenDuengung}{growR}
#'
#' @md
#' @export
PhenologicalAutocut = R6Class(
  "PhenologicalAutocut",
  inherit = Autocut,
  public = list(
#' @field cut_DOYs vector containing the integer day-of-year's on which cuts 
#'   occur.
    cut_DOYs = NULL,

    #' @description Constructor
    #'
    #' Valid cut dates are calculated upon initialization.
    #'
    #' @param MVS The [ModvegeSite] object for which cuts shall be 
    #'   determined.  
    #'
    #' @seealso [Autocut]
    initialize = function(MVS) {
      super$initialize(MVS)

      # Cut 200 degree-days before ST2
      target_temperature_sum = self$MVS$parameters$ST2 - 200
      first_cut_DOY = which.min(abs(self$MVS$ST - target_temperature_sum))

      # Get last day of growth
      i0 = 250
      i1 = 350
      threshold = 1
      deltas = diff(self$MVS$ST)[i0:i1]
      lower = which(deltas < threshold)
      if (length(lower) == 0) {
        last_cut_DOY = i1
      } else {
        last_cut_DOY = i0 + lower[[1]]
      }

      # Calculate time between cuts
      # Round n_cuts but apply at least 1
      integer_n_cuts = max(1, round(self$n_cuts))
      if (integer_n_cuts != 1) {
        delta = floor((last_cut_DOY - first_cut_DOY) / (integer_n_cuts - 1))
        self$cut_DOYs = (((1:integer_n_cuts) - 1) * delta) + first_cut_DOY
      } else {
        # If there's just one cut, apply it at end of season
        self$cut_DOYs = c(last_cut_DOY)
      }
    },

    #' @description Does a cut occur on *DOY*?
    #'
    #' Check if *DOY* is in `self$cut_DOYs`. If so, return `TRUE`. Return 
    #' `FALSE` otherwise.
    #' @param DOY Integer day of the year (1-366).
    #' @return Boolean
    determine_cut = function(DOY) {
      if (DOY %in% self$cut_DOYs) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )
)

#' Petersen autocut algorithm
#'
#' @description
#' Simulation routine to realistically predict grass cutting events.
#' This follows an implementation described in Petersen et al. (2021).
#'
#' @details
#' The decision to cut is made based on two criteria.
#' First, it is checked whether a *target biomass* is reached on given 
#' DOY. The defined target depends on the DOY and is given through 
#' :func:`get_target_biomass`. If said biomass is present, return `TRUE`.
#' 
#' Otherwise, it is checked whether a given amount of time has passed 
#' since the last cut. Depending on whether this is the first cut of 
#' the season or not, the relevant parameters are 
#' :int:`last_DOY_for_initial_cut` and :int:`max_cut_period`.
#' If that amount of time has passed, return `TRUE`, otherwise return 
#' `FALSE`.
#'  
#' The target biomass for a given day is determined following the principles 
#' described in Petersen et al.
#' 
#' The exact regression for the target biomass is based on Fig. S2 in the 
#' supplementary material of Petersen et al.
#' 
#' A refinement to expected yield as function of altitude has been 
#' implemented according to Table 1a in Huguenin et al. (2017).
#'
#' @references
#' \insertRef{huguenin2017GrundlagenDuengung}{growR}
#' \insertRef{petersen2021DynamicSimulationManagement}{growR}
#' 
#' @seealso [PhenologicalAutocut]
#' @md
#' @export
PetersenAutocut = R6Class(
  "PetersenAutocut",
  inherit = Autocut,
  public = list(
#' @field last_DOY_for_initial_cut Start cutting after this DOY, 
#'   even if yield target is not reached.
    last_DOY_for_initial_cut = 150,
#' @field max_cut_period Maximum period to wait between 
#'   subsequent cuts.
    max_cut_period = 55,
#' @field dry_precipitation_limit Maximum amount of allowed 
#'   precipitation (mm) to consider a day.
    dry_precipitation_limit = 1,
#' @field dry_days_before_cut Number of days that shold be dry 
#'   before a cut is made.
    dry_days_before_cut = 1,
#' @field dry_days_after_cut Number of days that shold be dry 
#'   after a cut is made.
    dry_days_after_cut = 2,
#' @field max_cut_delay Number of days a farmer is willing to 
#'   wait for dry conditions before a cut is made anyways.
    max_cut_delay = 5,
#' @field cut_delays Vector to keep track of cut delay times.
    cut_delays = c(0),
#' @field dry_window Logical that indicates if DOY at index is 
#'   considered dry enough to cut.
    dry_window = NULL,
#' @field target_biomass Biomass amount that should to be reached 
#'   by given DOY for a cut to be made.
    target_biomass = NULL,
#' @field end_of_cutting_season Determined DOY after which no 
#'   more cuts are made.
    end_of_cutting_season = NULL,

    #' @description Constructor
    #'
    #' @param MVS The [ModvegeSite] object for which cuts shall be 
    #'   determined.  
    initialize = function(MVS) {
      super$initialize(MVS)
      # Precalculate target biomass
      self$target_biomass = sapply(1:MVS$days_per_year,
        function(day) {
          self$get_target_biomass(day, intensity = MVS$management$intensity)
        }
      )
      # Calculate end of the season and max cut interval.
      self$end_of_cutting_season = 
        get_end_of_cutting_season(MVS$BM_after_cut, MVS$parameters$ELV, 
                                  MVS$management$intensity)
      delta = self$end_of_cutting_season - self$last_DOY_for_initial_cut
      self$max_cut_period = round(delta / self$n_cuts)
      # Prepare the intervals of dry-enough cut conditions.
      dry_days = MVS$weather$PP < self$dry_precipitation_limit
      self$dry_window = logical(MVS$days_per_year)
      for (i in 1:MVS$days_per_year) {
        i0 = max(i - self$dry_days_before_cut, 1)
        i1 = min(i + self$dry_days_after_cut, MVS$days_per_year)
        all_dry = all(dry_days[i0:i1])
        self$dry_window[i] = all_dry
      }
    },

    #' @description
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
    #' @references
    #' \insertRef{huguenin2017GrundlagenDuengung}{growR}
    get_annual_gross_yield = function(elevation, intensity = "high") {
      mask = yield_parameters$intensity == intensity
      a = yield_parameters[mask, ]$a
      b = yield_parameters[mask, ]$b
      return(a + b * max(elevation, 500))
    },

    #' @description
    #' Get target value of biomass on given *DOY*, which determines whether 
    #' a cut is to occur.
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
    #' A refinement to expected yield as function of altitude has been 
    #' implemented according to Table 1a in
    #' Huguenen-Elie et al. "Düngung von Grasland", Agrarforschung Schweiz, 
    #' 8, (6), 2017, 
    #' https://www.agrarforschungschweiz.ch/2017/06/9-duengung-von-grasland-grud-2017/
    #'
    #' @param DOY Integer day of the year to consider.
    #' @param intensity One of ("high", "middle", "low") specifying 
    #'   management intensity.
    #' @return target Biomass (kg / ha) that should be reached on day *DOY* 
    #'   for this management *intensity*.
    #'
    get_target_biomass = function(DOY, intensity = "high") {
      # For DOY < 130, use the value at DOY = 130
      DOY = max(130, DOY)
      # The relative fraction is taken from Petersen et al. (Fig. S2).
      f = self$get_relative_cut_contribution(DOY)
      # The absolute annual yield is approximated by Petersen's equation 19,
      # refined according to the source they cite.
      # The factor 1000 is to convert from t/ha to kg/ha.
      gross_yield = self$get_annual_gross_yield(self$MVS$parameters$ELV, 
                                                intensity = intensity) * 1000
      target = f * gross_yield
      # The target must not be smaller than the remainder after a cut
      return(max(target, self$MVS$BM_after_cut))
    },

    #' @description
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
    #' @references
    #' \insertRef{petersen2021DynamicSimulationManagement}{growR}
    get_relative_cut_contribution = function(DOY) {
      return((-0.1228 * DOY + 48.96) * 1e-2)
    },

    #' @description
    #' Last day of cutting season
    #'
    #' Estimate the last day on which it still makes sense to cut. This is done 
    #' by checking at which point the expected target biomass (see 
    #' `self$get_relative_cut_contribution()`) goes below the minimally harvestable 
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
    #' @seealso `get_relative_cut_contribution()`
    #' 
    #' @export
    get_end_of_cutting_season = function(min_biomass, elevation, 
                                         intensity = "high") {
      m_tot = self$get_annual_gross_yield(elevation, intensity) * 1000
      d_end = (min_biomass - m_tot * 48.96e-2) / (m_tot * -0.1228e-2)
      return(d_end)
    },

    #' @description
    #' Decide based on simple criteria whether day of year *DOY* would be a 
    #' good day to cut.
    #'
    #' This follows an implementation described in
    #' Petersen, Krischan, David Kraus, Pierluigi Calanca, Mikhail A. 
    #' Semenov, Klaus Butterbach-Bahl, and Ralf Kiese. “Dynamic Simulation 
    #' of Management Events for Assessing Impacts of Climate Change on 
    #' Pre-Alpine Grassland Productivity.” European Journal of Agronomy 
    #' 128 (August 1, 2021): 126306. 
    #' https://doi.org/10.1016/j.eja.2021.126306.
    #'
    #' The decision to cut is made based on two criteria.
    #' First, it is checked whether a *target biomass* is reached on given 
    #' DOY. The defined target depends on the DOY and is given through 
    #' :func:`get_target_biomass`. If said biomass is present, return `TRUE`.
    #'
    #' Otherwise, it is checked whether a given amount of time has passed 
    #' since the last cut. Depending on whether this is the first cut of 
    #' the season or not, the relevant parameters are 
    #' :int:`last_DOY_for_initial_cut` and :int:`max_cut_period`.
    #' If that amount of time has passed, return `TRUE`, otherwise return 
    #' `FALSE`.
    #'
    #' @param DOY Integer day of the year for which to make a cut decision.
    #' @return Boolean `TRUE` if a cut happens on day *DOY*.
    #'
    #' @seealso `get_target_biomass()`
    #'
    determine_cut = function(DOY) {
      # Get a shortcut
      MVS = self$MVS
      # Don't cut close to end-of-year
      if (DOY > self$end_of_cutting_season) {
        return(FALSE)
      }
      cut_target_reached = FALSE
      # Check if target biomass is reached.
      if (MVS$BM[DOY] >= self$target_biomass[DOY]) {
        cut_target_reached = TRUE
      }
      # Otherwise, check if enough time has passed to warrant a cut.
      n_previous_cuts = length(MVS$cut_DOYs)
      if (!cut_target_reached) {
        if (n_previous_cuts == 0) {
          cut_target_reached = DOY > self$last_DOY_for_initial_cut
        } else {
          last_cut_DOY = MVS$cut_DOYs[n_previous_cuts]
          cut_target_reached = DOY - last_cut_DOY > self$max_cut_period
        }
      }
      # If we're not ready to cut, exit.
      if (!cut_target_reached) {
        return(FALSE)
      }
      cut_index = n_previous_cuts + 1
      # Otherwise, check if the weather holds up...
      if (self$dry_window[DOY] ||
          # or if we just cannot wait any longer.
          self$cut_delays[cut_index] >= self$max_cut_delay) {
        # We're cutting. Prepare cut_delays vector for next cut.
        self$cut_delays = c(self$cut_delays, 0)
        return(TRUE)
      } else {
        # Wait another day.
        self$cut_delays[cut_index] = self$cut_delays[cut_index] + 1
        return(FALSE)
      }
    }
  )
)

