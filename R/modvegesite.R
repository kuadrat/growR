# Empty values for state variable and initial condition initialization
initial_run_variables = list(
  SENGV = NULL,
  SENGR = NULL,
  ABSDV = NULL,
  ABSDR = NULL,
  AgeGVp = NULL,
  AgeGRp = NULL,
  AgeDVp = NULL,
  AgeDRp = NULL,
  BMGVp = NULL,
  BMGRp = NULL,
  BMDRp = NULL,
  BMDVp = NULL,
  cBMp = NULL,
  WRp = NULL,
  GROGV = NULL,
  GROGR = NULL
)
initial_state_variables = list(
  AgeGV = NULL,
  AgeGR = NULL,
  AgeDV = NULL,
  AgeDR = NULL,
  BMGV = NULL,
  BMGR = NULL,
  BMDV = NULL,
  BMDR = NULL,
  OMDGV = NULL,
  OMDGR = NULL,
  OMDDV = NULL,
  OMDDR = NULL,
  BM = NULL,
  BMG = NULL,
  cBM = NULL,
  dBM = NULL,
  hvBM = NULL,
  OMD = NULL,
  OMDG = NULL,
  ST = NULL,
  REP = NULL,
  PGRO = NULL,
  GRO = NULL,
  LAI = NULL,
  LAIGV = NULL,
  AET = NULL,
  WR = NULL,
  ENV = NULL,
  ENVfPAR = NULL,
  ENVfT = NULL,
  ENVfW = NULL
)

#' ModvegeSite
#'
#' @description
#' Implements the ModVege grass growth model based off of Jouven et al. (2006).
#' 
#' This class contains model and site parameters and state variables as 
#' attributes and has methods for running ModVege with weather and management 
#' input.
#'
#' Use the `run()` method to carry out a simulation for a given year. The 
#' results are stored in the state variables in this instance and can be written 
#' to file using `write_output()`.
#'
#' @details
#' # Model variables
#' See Jouven et al. (2006) for a thorough description of all model variables.
#' 
#' ## State Variables
#' ```{r child = "vignettes/children/state_variables.Rmd"}
#' ```
#'
#' ## Initial conditions
#' ```{r child = "vignettes/children/initial_conditions.Rmd"}
#' ```
#' @seealso \link[=autocut]{autocut}
#'
#' @references
#' \insertRef{jouven2006ModelPredictingDynamics}{growR}
#'
#' @md
#' @export
ModvegeSite = R6Class(
  "ModvegeSite",

  public = c(
    initial_state_variables,
    initial_run_variables,

    list(
  #-Public-attributes-------------------------------------------------------------
#' @field time_step Used time step in the model in days (untested).
      time_step = 1.,
#' @field state_variable_names Vector containing the names of the model's 
#'   state variables.
      state_variable_names = NULL,
#' @field n_state_variables Number of state variables.
      n_state_variables = NULL,
#' @field version Version number of the growR package. Is written into 
#'   output files.
      version = packageVersion("growR"),
#' @field site_name Name of the site to be simulated.
      site_name = NULL,
#' @field run_name Name of the simulation run. Used to distinguish between 
#'   different runs at the same site.
      run_name = NULL,
#' @field year Year to be simulated.
      year = NULL,
#' @field days_per_year Number of days in this year.
      days_per_year = 365,
#' @field j_start_of_growing_season Index (DOY) of the day the growing season 
#'   was determined to begin.
      j_start_of_growing_season = NULL,
#' @field cut_height Height of remaining grass after cut in m.
      cut_height = 0.05,
#' @field parameters A [ModvegeParameters] object.
      parameters = NULL,
#' @field determine_cut Function used to decide whether a cut occurs on a 
#'   given DOY. Is overloaded depending on whether management data is 
#'   provided or not.
      determine_cut = NULL,
#' @field cut_DOYs List of DOYs on which a cut occurred.
      cut_DOYs = c(),
#' @field cut_during_growth_preriod Boolean to indicate whether a cut 
#'   occurred during the growth period, in which case reproductive growth is 
#'   stopped.
      cut_during_growth_preriod = NULL,
#' @field last_DOY_for_initial_cut [autocut] Start cutting after this DOY, 
#'   even if yield target is not reached.
      last_DOY_for_initial_cut = 150,
#' @field max_cut_period [autocut] Maximum period to wait between 
#'   subsequent cuts.
      max_cut_period = 55,
#' @field dry_precipitation_limit [autocut] Maximum amount of allowed 
#'   precipitation (mm) to consider a day.
      dry_precipitation_limit = 1,
#' @field dry_days_before_cut [autocut] Number of days that shold be dry 
#'   before a cut is made.
      dry_days_before_cut = 1,
#' @field dry_days_after_cut [autocut] Number of days that shold be dry 
#'   after a cut is made.
      dry_days_after_cut = 2,
#' @field max_cut_delay [autocut] Number of days a farmer is willing to 
#'   wait for dry conditions before a cut is made anyways.
      max_cut_delay = 5,
#' @field cut_delays [autocut] Vector to keep track of cut delay times.
#'   wait for dry conditions before a cut is made anyways.
      cut_delays = c(0),
#' @field dry_window [autocut] Logical that indicates if DOY at index is 
#'   considered dry enough to cut.
      dry_window = NULL,
#' @field target_biomass [autocut] Biomass amount that should to be reached 
#'   by given DOY for a cut to be made.
      target_biomass = NULL,
#' @field end_of_cutting_season [autocut] Determined DOY after which no 
#'   more cuts are made.
      end_of_cutting_season = NULL,
#' @field BM_after_cut [autocut] Amount of biomass that remains after a cut 
#'   (determined through cut_height and biomass densities BDGV, BDDV, BDGR, 
#'   BDDR).
      BM_after_cut = NULL,
#' @field weather A list created by a [WeatherData] object's 
#'   `get_weather_for_year()` method.
      weather = NULL,
#' @field management A list containing management data as returned by 
#'    [ModvegeEnvironment]'s `get_environment_for_year()` method. If its 
#'    `is_empty` field is `TRUE`, the [autocut] routine will be employed.
      management = NULL,

  #-Public-methods----------------------------------------------------------------

      #' @description Constructor
      #'
      #' @param site_name string Name of the simulated site.
      #' @param run_name string Name of the simulation run. Used to 
      #'   differentiate between different simulation conditions at the same site.
      #'   Defaults to "-", which indicates no specific run name.
      #' @param parameters A [ModvegeParameters] object.
      #'
      initialize = function(parameters, site_name = "-", run_name = "-") {
        self$site_name = site_name
        self$run_name = run_name
        self$parameters = parameters
        self$set_SGS_method(self$parameters$SGS_method)

        self$state_variable_names = names(initial_state_variables)
        self$n_state_variables = length(self$state_variable_names)

        # Precalculate constants
        private$REP_ON = (0.25 + (0.75 * (parameters$NI - 0.35)) / 0.65) 
        # Limit for biomass that remains after a cut
        self$BM_after_cut = self$cut_height * 10. * sum(parameters$BDGV,
                                                        parameters$BDGR,
                                                        parameters$BDDV,
                                                        parameters$BDDR)
      },

      #' @description Return weather data if it exists
      #'
      #' @return The WeatherData object, if it exists.
      get_weather = function() {
        if (is.null(self$weather)) {
          stop("ModvegeSite$weather has not been set.")
        } else {
          return(self$weather)
        }
      },

      #' @description Return management data if it exists
      #'
      #' @return The ManagementData object, if it exists.
      get_management = function() {
        if (is.null(self$management)) {
          stop("ModvegeSite$management has not been set.")
        } else {
          return(self$management)
        }
      },

      #' @description Choose which method to be used for determination of SGS
      #' 
      #' Options for the determination of the start of growing season (SGS) are:
      #' \describe{
      #'   \item{MTD}{Multicriterial thermal definition, 
      #'     [start_of_growing_season_mtd()]}
      #'   \item{simple}{Commonly used, simple SGS definition based on 
      #'     temperature sum, [start_of_growing_season()]}
      #' }
      #'
      #' @param method str Name of the method to use. Options: "MTD", "simple".
      #' @return none
      #'
      #' @seealso [start_of_growing_season_mtd()], [start_of_growing_season()]
      #'
      set_SGS_method = function(method) {
        if (method %in% private$SGS_options) {
          private$SGS_method = method
        } else {
          warning(sprintf("Unrecognized SGS method `%s`. Use one of %s.",
                  paste(private$SGS_options, collapse = ", ")))
        }
      },

      #' @description Read from the input whether a cut occurs on day *DOY*.
      #'
      #' @param DOY Integer day of the year for which to check.
      #' @return Boolean `TRUE` if a cut happens on day *DOY*.
      #'
      determine_cut_from_input = function(DOY) {
        M = self$get_management()
        return(M$n_cuts > 0 && (DOY %in% M$cut_DOY))
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
      determine_cut_automatically = function(DOY) {
        # Don't cut close to end-of-year
        if (DOY > self$end_of_cutting_season) {
          return(FALSE)
        }
        cut_target_reached = FALSE
        # Check if target biomass is reached.
        if (self$BM[DOY] >= self$target_biomass[DOY]) {
          cut_target_reached = TRUE
        }
        # Otherwise, check if enough time has passed to warrant a cut.
        n_previous_cuts = length(self$cut_DOYs)
        if (!cut_target_reached) {
          if (n_previous_cuts == 0) {
            cut_target_reached = DOY > self$last_DOY_for_initial_cut
          } else {
            last_cut_DOY = self$cut_DOYs[n_previous_cuts]
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
        f = get_relative_cut_contribution(DOY)
        # The absolute annual yield is approximated by Petersen's equation 19,
        # refined according to the source they cite.
        # The factor 1000 is to convert from t/ha to kg/ha.
        gross_yield = get_annual_gross_yield(self$parameters$ELV, 
                                             intensity = intensity) * 1000
        target = f * gross_yield
        # The target must not be smaller than the remainder after a cut
        return(max(target, self$BM_after_cut))
      },

      #-Wrapper-function----------------------------------------------------------

      #' @description Carry out a ModVege simulation for one year.
      #'
      #' @param year Integer specifying the year to consider.
      #' @param weather Weather list for given year as returned by 
      #'   [WeatherData]`$get_weather_for_year`.
      #' @param management Management list for given year as provided by 
      #'   [ModvegeEnvironment]`$get_environment_for_year()`.
      #' @return None Fills the state variables of this instance with the 
      #'   simulated values. Access them programmatically or write them to 
      #'   file using `write_output()`.
      #'
      run = function(year, weather, management) {
        logger("Start of ModvegeSite$run()", level = TRACE)
        self$weather = weather
        # Infer days of this year from weather data
        self$days_per_year = weather$ndays
        logger(sprintf("days_per_year: %s", self$days_per_year), level = DEBUG)
        self$management = management
        self$year = year
        private$initialize_state_variables()
        private$calculate_temperature_sum()
        private$get_start_of_growing_season()

        # Determine whether cuts are to be read from input or to be 
        # determined algorithmically.
        if (management$is_empty) {
          # Precalculate target biomass
          self$target_biomass = sapply(1:self$days_per_year,
            function(day) {
              self$get_target_biomass(day, intensity = management$intensity)
            }
          )
          # Calculate end of the season and max cut interval.
          self$end_of_cutting_season = 
            get_end_of_cutting_season(self$BM_after_cut, self$parameters$ELV, 
                                      management$intensity)
          n_cuts = get_expected_n_cuts(self$parameters$ELV, management$intensity)
          delta = self$end_of_cutting_season - self$last_DOY_for_initial_cut
          self$max_cut_period = round(delta / n_cuts)
          # Prepare the intervals of dry-enough cut conditions.
          dry_days = weather$PP < self$dry_precipitation_limit
          self$dry_window = logical(self$days_per_year)
          for (i in 1:self$days_per_year) {
            i0 = max(i - self$dry_days_before_cut, 1)
            i1 = min(i + self$dry_days_after_cut, self$days_per_year)
            all_dry = all(dry_days[i0:i1])
            self$dry_window[i] = all_dry
          }
          # Point the cut determination function to the autocut routine.
          self$determine_cut = self$determine_cut_automatically
        } else {
          self$determine_cut = self$determine_cut_from_input
        }

        # Loop over days of the year.
        # :TODO: Avoid if j==1 statements by increasing vector sizes by one.
        logger("Starting loop over days of the year.", level = TRACE)
        for (j in 1:self$days_per_year) {
          private$current_DOY = j
          private$carry_over_from_last_day()
          private$calculate_growth()
          private$calculate_ageing()
          private$update_biomass()
          private$calculate_digestibility()
          private$apply_cuts()

          # Re-align LAI to BMGV
          P = self$parameters
          self$LAI[j] = P$SLA * P$pcLAM * (self$BMGV[j] + self$BMGR[j]) / 10.
          self$LAIGV[j] = P$SLA * P$pcLAM * (self$BMGV[j]) / 10.
        }
        logger("End of ModvegeSite$run()", level = TRACE)
      },

      #' @description Write values of ModVege results into given file.
      #'
      #' A header with metadata is prepended to the actual data.
      #'
      #' @param filename Path or name of filename to be created or overwritten.
      #' @param force Boolean If `TRUE`, do not prompt user before writing.
      #' @return None Writes simulation results to file *filename*.
      #'
      write_output = function(filename, force = FALSE) {
        # Build the header containing metadata
        header = private$make_header()
        
        ndays = self$days_per_year
        col_names = c("DOY", self$state_variable_names)
        # Create vectors for pseudo-state variables OMDDV and OMDDR
        self$OMDDV = rep(self$parameters$OMDDV, ndays)
        self$OMDDR = rep(self$parameters$OMDDR, ndays)
        # Put all state variable vectors into a data.frame for writing
        outf = data.frame(1:ndays)
        for (variable_name in self$state_variable_names) {
          outf = cbind(outf, round(self[[variable_name]], 2))
        }
        if (!force) {
          response = prompt_user(sprintf("Writing to file `%s`. Continue? [Y/n]",
                                         filename))
          if (!response %in% c("y", "Y", "")) {
            logger("Not writing file.", level = INFO)
            return()
          }
        }
        logger(paste("Writing to file", filename, "."), level = TRACE)
        write(header, file = filename)
        append_to_table(format(outf, digits = 2, nsmall = 2),
                        filename,
                        quote = FALSE,
                        sep = "\t",
                        row.names = FALSE,
                        col.names = col_names)
        logger(paste("Wrote to file", filename, "."), level = DEBUG)
      },

      #' @description Savely update the values in `self$parameters`.
      #'
      #' This is just a shorthand to the underlying `ModvegeParameters` 
      #' object's `set_parameters()` function. Special care is taken to 
      #' account for potential changes to functional group weights.
      #'
      #' @param params List of name-value pairs of the parameters to update.
      #' @return None Updates this object's parameter values.
      #'
      #' @seealso `ModvegeParameters$set_parameters()`
      #'
      set_parameters = function(params) {
        self$parameters$set_parameters(params)
      },

      #' @description Create an overview plot for 16 state variables.
      #'
      #' Creates a simple base R plot showing the temporal evolution of 16 
      #' modeled state variables.
      #'
      #' Can only be sensibly run *after* a simulation has been carried out, 
      #' i.e. after this instance's `run()` method has been called.
      #'
      #' @param ... Further arguments are discarded.
      #' @return NULL Creates a plot of the result in the active device.
      #'
      plot = function(...) {
        oldpar = par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mfrow = c(4, 4))
        vars_to_plot = c("BM", "cBM", "hvBM", "dBM",
                         "ENV", "ENVfT", "ENVfW", "ENVfPAR",
                         "WR", "AET", "LAI", "LAIGV",
                         "PGRO", "REP", "GRO", "ST")
        n_vars_to_plot = length(vars_to_plot)
        # Plot the first variable with a title indicating site name and year.
        self$plot_var(vars_to_plot[1])
        # Plot the remaining variables with the variable name as title.
        for (i in 2:n_vars_to_plot) {
          var = vars_to_plot[i]
          self$plot_var(var, main = var)
        }
      },

      #' @description Create an overview plot for biomass.
      #'
      #' Creates a simple base R plot showing the BM with cutting events and,
      #' if applicable, target biomass, dBM, cBM and hvBM.
      #' Can only be sensibly run *after* a simulation has been carried out, 
      #' i.e. after this instance's `run()` method has been called.
      #'
      #' @param smooth_interval Int. Number of days over which the variable 
      #'   `dBM` is smoothened.
      #' @param ... Further arguments are discarded.
      #' @return NULL Creates a plot of the result in the active device.
      #'
      plot_bm = function(smooth_interval = 28, ...) {
        private$check_if_simulation_has_run()
        oldpar = par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mfrow = c(2, 2))
        xlab = "DOY"
        # BM with cuts and target biomass
        plot(self$BM, type = "l", xlab = xlab, ylab = "BM (kg / ha)")
        title(paste(self$site_name, self$run_name, self$year))
        abline(v = self$cut_DOYs, col = "blue")
        if (self$get_management()$is_empty) {
          lines(self$target_biomass, col = "grey")
        }
        # dBM, cBM, hBM
        plot(box_smooth(self$dBM, smooth_interval), type = "l", 
             xlab = xlab, ylab = "smoothened dBM (kg / ha)")
        plot(self$cBM, type = "l", xlab = xlab, ylab = "cBM (kg / ha)")
        plot(self$hvBM, type = "l", xlab = xlab, ylab = "hvBM (kg / ha)")
      },

      #' @description Create an overview plot of limiting factors.
      #'
      #' Creates a simple base R plot showing the different environmental 
      #' limitation functions over time.
      #' Can only be sensibly run *after* a simulation has been carried out, 
      #' i.e. after this instance's `run()` method has been called.
      #'
      #' @param ... Further arguments are discarded.
      #' @return NULL Creates a plot of the result in the active device.
      #'
      plot_limitations = function(...) {
        oldpar = par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mfrow = c(2, 2))
        ylim = c(0, 1)
        self$plot_var("ENV", ylim = ylim)
        self$plot_var("ENVfW", ylim = ylim, main = "limitation functions")
        self$plot_var("ENVfT", ylim = ylim, main = "limitation functions")
        self$plot_var("ENVfPAR", ylim = ylim, main = "limitation functions")
      },

      #' @description Create an overview plot of the water balance.
      #'
      #' Creates a simple base R plot showing different variables pertaining to
      #' the water balance, namely water reserves *WR*, actual 
      #' evapotranspiration *AET*, leaf area index *LAI* and LAI of the green 
      #' vegetative compartment *LAIGV*. 
      #'
      #' Can only be sensibly run *after* a simulation has been carried out, 
      #' i.e. after this instance's `run()` method has been called.
      #'
      #' @param ... Further arguments are discarded.
      #' @return NULL Creates a plot of the result in the active device.
      #'
      plot_water = function(...) {
        oldpar = par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mfrow = c(2, 2))
        self$plot_var("WR")
        self$plot_var("AET", main = "water balance")
        self$plot_var("LAI", main = "")
        self$plot_var("LAIGV", main = "")
      },

      #' @description Create an overview plot of growth dynamics.
      #'
      #' Creates a simple base R plot showing different variables pertaining to
      #' the growth dynamics, namely potential growth *PGRO*, effective 
      #' growth *GRO*, the reproductive function *REP* and the temperature 
      #' sum *ST*. 
      #'
      #' Can only be sensibly run *after* a simulation has been carried out, 
      #' i.e. after this instance's `run()` method has been called.
      #'
      #' @param ... Further arguments are discarded.
      #' @return NULL Creates a plot of the result in the active device.
      #'
      plot_growth = function(...) {
        oldpar = par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mfrow = c(2, 2))
        self$plot_var("PGRO")
        self$plot_var("REP", main = "growth dynamics")
        self$plot_var("GRO", main = "")
        self$plot_var("ST", main = "")
      },

      #' @description Plot the temporal evolution of a modeled state variable.
      #'
      #' @param var String. Name of the state variable to plot.
      #' @param ... Further arguments are passed to the base [plot()] function.
      #' @return None, but plots to the current device.
      #'
      plot_var = function(var, ...) {
        private$check_if_simulation_has_run()
        # Check for valid input.
        if (!var %in% self$state_variable_names) {
          message = "Variable `%s` is invalid for plotting. Use one of:\n%s"
          vars = paste(self$state_variable_names, collapse = ", ")
          warning(sprintf(message, var, vars))
          return()
        }
        arguments = list(...)
        # Set default values
        if (!"type" %in% names(arguments)) {
          arguments[["type"]] = "l"
        }
        if (!"main" %in% names(arguments)) {
          arguments[["main"]] = paste(self$site_name, self$run_name, self$year)
        }
        arguments[["xlab"]] = "DOY"
        arguments[["ylab"]] = private$ylabels[[var]]
        arguments[["x"]] = self$weather$DOY
        arguments[["y"]] = self[[var]]
        do.call(plot, arguments)
      }
    )
  ), # End of public attributes

  private = list(
    #-Private-fields------------------------------------------------------------
#    vars_to_exclude = c("PET", "OMDDV", "OMDDR"),
    vars_to_exclude = c("OMDDV", "OMDDR"),
    current_DOY = 1,
    REP_ON = NULL,
    SGS_method = "MTD",
    SGS_options = c("MTD", "simple"),
    # List of labels to use for different variables when plotting.
    ylabels  =  list(AgeGV = "AgeGV (degree days)",
                   AgeGR = "AgeGR (degree days)",
                   AgeDV = "AgeDV (degree days)",
                   AgeDR = "AgeDR (degree days)",
                   BMGV = "BMGV (kg/ha)",
                   BMGR = "BMGR (kg/ha)",
                   BMDV = "BMDV (kg/ha)",
                   BMDR = "BMDR (kg/ha)",
                   BM = "standing biomass BM (kg/ha)",
                   BMG = "standing green biomass BMG (kg/ha)",
                   cBM = "cumulative biomass cBM (kg/ha)",
                   dBM = "daily biomass growth dBM (kg/ha/d)",
                   hvBM = "total harvested biomass growth hvBM (kg/ha)",
                   OMD = "organic matter digestibility OMD (kg/kg)",
                   OMDG = "OMDG (kg/kg)",
                   OMDGV = "OMDGV (kg/kg)",
                   OMDGR = "OMDGR (kg/kg)",
                   OMDDV = "OMDDV (kg/kg)",
                   OMDDR = "OMDDR (kg/kg)",
                   ST = "temperature sum (degree days)",
                   REP = "reproductive function REP",
                   GRO = "daily growth (kg/ha/d)",
                   PGRO = "potential daily growth (kg/ha/d)",
                   LAI = "leaf area index LAI",
                   LAIGV = "LAIGV",
                   AET = "actual evapotranspiration AET (mm)",
                   WR = "water reserves WR (mm)",
                   ENV = "environmental limitation ENV",
                   ENVfPAR = "radiation limitation ENVfPAR",
                   ENVfT = "temperature limitation ENVfT",
                   ENVfW = "water limitation ENVfW"
                   ),

    #-Private-methods-----------------------------------------------------------

    ## @description Initialize all relevant state variables
    ##
    ## Firstly, an empty vector of length *days_per_year* is created for each 
    ## variable name in the vector `self$state_variable_names`.
    ## Then, some additional variables relating to management and water balance 
    ## are also initialized.
    ##
    ## @note Technically, not all of the variables listed in 
    ## `state_variable_names` are re-initialized, as some of them already hold 
    ## the right values for the simulation. Particularly, this is the case for:
    ## OMDDV, OMDDR
    ## 
    initialize_state_variables = function() {
      # Create empty state variable vectors
      for (i in 1:self$n_state_variables) {
        var_name = self$state_variable_names[i] 
        if (!var_name %in% private$vars_to_exclude) {
          self[[var_name]] = rep(0, self$days_per_year)
        }
      } 
      P = self$parameters
      # For some vectors the initial value is known
      self[["hvBM"]][1] = 0
      self[["OMDGV"]][1] = P$OMDGV0
      self[["OMDGR"]][1] = P$OMDGR0

      # Management:
      # Initialize a pointer that indicates whether there has been a cut 
      # during reproductive growth. In this case, reproductive growth is
      # ceased.
      self$cut_during_growth_preriod = FALSE
      self$cut_DOYs = c()
      self$cut_delays = c(0)

      # Water balance and growth
      self[["SENGV"]] = P$SENGV0
      self[["SENGR"]] = P$SENGR0
      self[["ABSDV"]] = P$ABSDV0
      self[["ABSDR"]] = P$ABSDR0

      # Further initial values
      self[["AgeGVp"]] = P$AgeGV0
      self[["AgeGRp"]] = P$AgeGR0
      self[["AgeDRp"]] = P$AgeDR0
      self[["AgeDVp"]] = P$AgeDV0
      self[["BMGVp"]]  = P$BMGV0
      self[["BMGRp"]]  = P$BMGR0
      self[["BMDRp"]]  = P$BMDR0
      self[["BMDVp"]]  = P$BMDV0
      self[["cBMp"]]   = P$cBM0
      self[["WRp"]]    = P$WR0
    },

    ## @description
    ## Pre-calculate the temperature sum based on the daily average 
    ## temperatures stored in self$weather using the method defined by 
    ## private$SGS_method (which can be set trhough [self$set_SGS_method()]).
    ##
    ## Sets self$ST.
    ##
    calculate_temperature_sum = function() {
      W = self$get_weather()

      # Set negative values to 0
      non_negative = sapply(W$Ta, function(t) { max(t, 0) })

      if (private$SGS_method == "MTD") {
        # Use simple temperature sum
        self$ST = cumsum(non_negative)
      } else if (private$SGS_method == "simple") {
        # Use weighted temperature sum
        self$ST = weighted_temperature_sum(non_negative)
      }
    },

    ## @description
    ## Get index (or, equivalently, DOY) of the start of the growing season.
    ##
    ## This function implements the *multicriterial thermal definition* (MTD) 
    ## as described in chapter 2.3.1.3 of the dissertation of Andreas 
    ## Schaumberger:
    ## Räumliche Modelle zur Vegetations- und Ertragsdynamik im 
    ## Wirtschaftsgrünland, 2011, ISBN-13: 978-3-902559-67-8
    ##
    ## @param first_possible_DOY int Only consider days of the year from this 
    ##   value onward.
    ## @param consider_snow Toggle whether the effect of snow cover is to be 
    ##   considered.
    ## @param critical_snow Minimum daily snowfall in mm for a day to be 
    ##   considered snowy.
    ##
    get_start_of_growing_season = function(first_possible_DOY = 30,
                                           consider_snow = FALSE,
                                           critical_snow = 1.) {
      W = self$get_weather()
      # Find the last day of the first half of the year that still had snow 
      # cover. Assume that growth can only start after that day.
      if (consider_snow) {
        # Go one day beyond the last day with too much snow
        j_snow = max(which(W[["snow"]][1:180] > critical_snow)) + 1
      } else {
        j_snow = 1
      }

      if (private$SGS_method == "MTD") {
        j_t_critical = start_of_growing_season_mtd(W$Ta, first_possible_DOY)
      } else if (private$SGS_method == "simple") {
        # :TODO: [Performance/Redundancy] This internally calculates ST, 
        # which would already be accessible in self$ST.
        j_t_critical = start_of_growing_season(W$Ta)
      }

      self$j_start_of_growing_season = max(j_snow, j_t_critical)
      return(self$j_start_of_growing_season)
    },

    ## @description Set calculation values to the values stored in 
    ## previously used position.
    ##
    carry_over_from_last_day = function() {
      j = private$current_DOY
      if (j == 1) { return(0) }
      self$AgeGVp = self$AgeGV[j - 1]
      self$AgeGRp = self$AgeGR[j - 1]
      self$AgeDRp = self$AgeDR[j - 1]
      self$AgeDVp = self$AgeDV[j - 1]
      self$BMGVp  = self$BMGV[j - 1]
      self$BMGRp  = self$BMGR[j - 1]
      self$BMDRp  = self$BMDR[j - 1]
      self$BMDVp  = self$BMDV[j - 1]
      self$cBMp   = self$cBM[j - 1]
      self$WRp    = self$WR[j - 1]
    },

    ## @description Update all variables which change on a daily basis and 
    ## calculate biomass production.
    ##
    ## This function updates the following attributes:
    ## LAIGV
    ## AET
    ## WR
    ## ENVfPAR
    ## ENVfT
    ## ENVfW
    ## ENV
    ## PGRO
    ## GRO
    ##
    calculate_growth = function() {
      # Create shorthands
      W = self$get_weather()
      P = self$parameters
      j = private$current_DOY

      # LAI for computing PGRO
      self$LAIGV[j] = P$SLA * P$pcLAM * self$BMGVp / 10.

      # LAI for computing AET
      LAI.ET = P$SLA * P$pcLAM * (self$BMGVp + self$BMGRp) / 10.

      # Actual evapotranspiration.
      # PET is first partitioned into potential transpiration, PTr, and
      # potential soil evaporation, PEv, using the same factor,
      # 1. - exp(-0.6*LAIGVGR), as used to compute intercepted radiation.
      # Then impose water stress limitation, fW. In the case of soil
      # evaporation use an fW appropriate for high values of PET (PETmx).
      PETmn  = .2
      PETmx  = 8.

    #  PETeff = ifelse(snow[j] > 5, PETmn, 
    #                    ifelse(PP[j] > 1, 0.7*PET[j], PET[j])) 

      if (W$snow[j] > 5) {
        # Snow cover
        PETeff = PETmn
      } else {
        # Less evapotranspiration when there is precipitation.
        PETeff = P$crop_coefficient * W$PET[j]
        PETeff = ifelse(W$PP[j] > 1, 0.7 * PETeff, PETeff)
      }
      PETeff = PETeff * fCO2_transpiration_mod(W$aCO2)
      PTr = PETeff * (1. - exp(-0.6 * LAI.ET))

      ATr = PTr * fW(self$WRp / P$WHC, PETeff)
      PEv = PETeff -  PTr
      AEv = PEv * self$WRp / P$WHC
      self$AET[j] = ATr + AEv

      # Soil moisture budget
      self$WR[j] = max(0., 
                       min(P$WHC, 
                           self$WRp + W$liquidP[j] + W$melt[j] - self$AET[j]))

      # Environmental constraints.
      self$ENVfPAR[j] = fPAR(W$PAR[j])
      self$ENVfT[j]   = fT(W$Ta[j], P$T0, P$T1, P$T2)
      self$ENVfW[j]   = fW(self$WR[j] / P$WHC, PETeff)
      self$ENV[j]     = self$ENVfPAR[j] * self$ENVfT[j] * self$ENVfW[j]

      if (j < self$j_start_of_growing_season) {
        # Growing season has not started yet.
        self$PGRO[j] = 0
        self$GRO[j] = 0
      } else {
        self$PGRO[j] = W$PAR[j] * P$RUEmax * 
          (1. - exp(-0.6 * self$LAIGV[j])) * 10. * 
          fCO2_growth_mod(W$aCO2, P$CO2_growth_factor)
        self$GRO[j]  = P$NI * self$PGRO[j] * self$ENV[j] * 
          SEA(self$ST[j], P$minSEA, P$maxSEA, P$ST1, P$ST2)
      }
    },

    ## @description
    ## Calculate the ageing, senescence and abscission of different biomass 
    ## compartments.
    ##
    calculate_ageing = function() {
      # Create shorthands
      j = private$current_DOY
      P = self$parameters
      W = self$get_weather()
      T_average = W$Ta[j]

      # Calculate proportion of reproductive growth (REP):
      # REP only occurs during the degree-time interval [ST1, ST2] and is 
      # stopped if there has been a cut during that interval.
      if (!self$cut_during_growth_preriod & 
          self$ST[j] >= P$ST1 & 
          self$ST[j] <= P$ST2) {
        self$REP[j] = private$REP_ON
      } else {
        self$REP[j] = 0
      }

      # Split growth into vegetative and reproductive compartments.
      self$GROGV = self$GRO[j] * (1. - self$REP[j])
      self$GROGR = self$GRO[j] * self$REP[j]

      # Calculate ageing of GV & GR.
      # The ifelse's are to prevent division by 0.
      if (self$BMGVp - self$SENGV + self$GROGV != 0.) {
        dAgeGV = (self$BMGVp - self$SENGV) / 
                 (self$BMGVp - self$SENGV + self$GROGV) * 
                 (self$AgeGVp + max(0., T_average)) - 
                 self$AgeGVp
      } else {
        # Reset age to 0
        dAgeGV = -self$AgeGVp
      }
      self$AgeGV[j] = self$AgeGVp + dAgeGV * self$time_step
      
      if (self$BMGRp - self$SENGR + self$GROGR != 0.) {
        dAgeGR = (self$BMGRp - self$SENGR) / 
                 (self$BMGRp - self$SENGR + self$GROGR) *
                 (self$AgeGRp + max(0., T_average)) - 
                 self$AgeGRp
      } else {
        dAgeGR = -self$AgeGRp
      }
      self$AgeGR[j] = self$AgeGRp + dAgeGR * self$time_step
     
      # Begin preliminary calculations for determination of senescence.
      # :TODO: Certain if statements and calculation blocks can be avoided.
      ratio1 = self$AgeGV[j] / P$LLS
      fAgeGV = if (ratio1 < 1./3.) {
        fAgeGV = 1.
      } else if (ratio1 < 1.) {
        fAgeGV = 3. * ratio1
      } else {
        fAgeGV =  3.
      }
      ratio2 = self$AgeGR[j] / (P$ST2 - P$ST1)
      if (ratio2 < 1./3.) {
        fAgeGR = 1.
      } else if (ratio2 < 1.) {
        fAgeGR = 3. * ratio2 
      } else {
        fAgeGR = 3.
      }
      
      # Calculate senescence for next day.
      if (T_average > P$T0) {
        # Warm enough for photosynthesis
        self$SENGV = P$KGV * self$BMGVp * T_average * fAgeGV
        self$SENGR = P$KGR * self$BMGRp * T_average * fAgeGR
      } else if (T_average > 0.) {
        # Too cold for photosynthesis, but above freezing.
        self$SENGV = 0.
        self$SENGR = 0.
      } else {
        # Below 0 temperature-> freezing damage.
        self$SENGV = P$KGV * self$BMGVp * abs(T_average)
        self$SENGR = P$KGR * self$BMGRp * abs(T_average)
      }

      # Put a cap on senescence.
      # :NOTE: The following two lines were not part of the original model 
      # formulation.
      if (abs(self$SENGV) > P$senescence_cap * abs(self$GROGV)) { 
        self$SENGV = P$senescence_cap * self$GROGV 
      }
      if (abs(self$SENGR) > P$senescence_cap * abs(self$GROGR)) { 
        self$SENGR = P$senescence_cap * self$GROGR 
      }
     
      # Calculate ageing for compartments DV & DR.
      # The ifelse is to prevent zerodivision.
      if (self$BMDVp - self$ABSDV + self$SENGV != 0.) {
        dAgeDV = (self$BMDVp - self$ABSDV) / 
          (self$BMDVp - self$ABSDV + self$SENGV) *
          (self$AgeDVp + max(0., T_average)) - self$AgeDVp
      } else {
        dAgeDV = -self$AgeDVp
      }
      self$AgeDV[j] = self$AgeDVp + dAgeDV * self$time_step
      
      if (self$BMDRp - self$ABSDR + self$SENGR != 0.) {
        dAgeDR = (self$BMDRp - self$ABSDR) /
          (self$BMDRp - self$ABSDR + self$SENGR) *
          (self$AgeDRp + max(0., T_average)) - self$AgeDRp
      } else {
        dAgeDR = -self$AgeDRp
      }
      self$AgeDR[j] = self$AgeDRp + dAgeDR * self$time_step
      
      # Preliminary calculations for abscission.
      ratio3 = self$AgeDV[j] / P$LLS
      if (ratio3 < 1./3.) {
        fAgeDV = 1.
      } else if (ratio3 < 2./3.) {
        fAgeDV = 2.
      } else {
        fAgeDV = 3.
      }

      ratio4 = self$AgeDR[j] / (P$ST2 - P$ST1)
      if (ratio4 < 1./3.) {
        fAgeDR = 1.
      } else if (ratio4 < 2./3.) {
        fAgeDR = 2.
      } else {
        fAgeDR = 3.
      }

      # Calculate the abscission for the next day.
      self$ABSDV = ifelse(T_average > 0., 
                          P$KlDV * self$BMDVp * T_average * fAgeDV,
                          0.)
      self$ABSDR = ifelse(T_average > 0.,
                          P$KlDR * self$BMDRp * T_average * fAgeDR,
                          0.)
    },

    ## @description Calculate and update the amount of biomass in each 
    ## compartment.
    ##
    update_biomass = function() {
      j = private$current_DOY
      P = self$parameters
      # SEN is always >= 0
      dBMGV = self$GROGV - self$SENGV
      self$BMGV[j] = max(self$BMGVp + dBMGV * self$time_step, 
                         self$parameters$stubble_height * 10. * P$BDGV)
      dBMGR = self$GROGR - self$SENGR
      self$BMGR[j] = max(self$BMGRp + dBMGR * self$time_step, 
                         self$parameters$stubble_height * 10. * P$BDGR)
      
      dBMDV = (1. - P$sigmaGV) * self$SENGV - self$ABSDV
      self$BMDV[j] = max(self$BMDVp + dBMDV * self$time_step, 0.)
      dBMDR = (1. - P$sigmaGR) * self$SENGR - self$ABSDR
      self$BMDR[j] = max(self$BMDRp + dBMDR * self$time_step, 0.)
      
      # Current (BM) and cumulative (cBM) biomass and today's biomass change (dBM).
      self$BM[j] = self$BMGV[j] + self$BMGR[j] + self$BMDV[j] + self$BMDR[j]
      self$dBM[j] = dBMGV + dBMGR + dBMDV + dBMDR
      self$cBM[j] = self$cBMp + max(0., self$dBM[j])

      # Green biomass
    ## :TODO: These derived values can be calculated vectorially upon request.
      self$BMG[j] = self$BMGV[j] + self$BMGR[j]
    },

    ## @description Caclulate the organic matter digestibility of each compartment.
    ##
    ## :TODO: These derived values can be calculated vectorially upon 
    ## request and thus do not need to be part of a for loop.
    ##
    calculate_digestibility = function() {
      P = self$parameters
      j = private$current_DOY
      # OMDGV[j] = max(minOMDGV,
      #                ifelse(j == 1, maxOMDGV,
      #                       maxOMDGV - AgeGV[j]*(maxOMDGV - minOMDGV)/LLS))
      # OMDGR[j] = max(minOMDGR,
      #                ifelse(j == 1, maxOMDGR,
      #                       maxOMDGR - AgeGR[j]*(maxOMDGR - minOMDGR)/(ST2 - ST1)))
      self$OMDGV[j] = P$maxOMDGV - self$AgeGV[j] * 
        (P$maxOMDGV - P$minOMDGV) / P$LLS
      self$OMDGR[j] = P$maxOMDGR - self$AgeGR[j] * 
        (P$maxOMDGR - P$minOMDGR) / (P$ST2 - P$ST1)
      
      # Total digestibility
      self$OMD[j] = (self$OMDGV[j] * self$BMGV[j] + self$OMDGR[j] * 
                     self$BMGR[j] + P$OMDDV * self$BMDV[j] + P$OMDDR * 
                     self$BMDR[j]) / self$BM[j]
      
      # Digestibility of green biomass
      self$OMDG[j] = (self$OMDGV[j] * self$BMGV[j] + 
                      self$OMDGR[j] * self$BMGR[j]) / self$BMG[j]
    },

    ## @description Simulate the effects of cuts on the grass growth.
    ##
    apply_cuts = function() {
      j = private$current_DOY
      P = self$parameters
      M = self$get_management()

      # If there's no cut today, nothing is harvested.
      cut_occurred = self$determine_cut(j)

      if (!cut_occurred) {
        # No cuts occurred
        if (j != 1) {
          self$hvBM[j] = self$hvBM[j - 1]
        }
        return(0)
      }

      # Keep track of applied cuts
      self$cut_DOYs = c(self$cut_DOYs, j)

      # If defoliation occurs when ST1 < ST < ST2, reproductive growth is 
      # stopped, if it hasn't already.
      if (!self$cut_during_growth_preriod & 
          self$ST[j] >= P$ST1 & 
          self$ST[j] <= P$ST2) { 
        # This only happens once. Afterwards, above conditional always 
        # gives FALSE.
        self$cut_during_growth_preriod = TRUE 
      }
      
      # Calculate the harvested biomass.
      # Can only harvest if the respective compartments have grown larger than 
      # the cut_height. In this case, cut down biomass to cut_height * density
      # :NOTE: This implicitly discards today's growth by using BMXXp instead of 
      # BMXX[i].
      self$BMGV[j] = ifelse(self$BMGVp > self$cut_height * 10. * P$BDGV,
                            self$cut_height * 10. * P$BDGV,
                            self$BMGVp)
      self$BMDV[j] = ifelse(self$BMDVp > self$cut_height * 10. * P$BDDV,
                            self$cut_height * 10. * P$BDDV,
                            self$BMDVp)
      self$BMGR[j] = ifelse(self$BMGRp > self$cut_height * 10. * P$BDGR,
                            self$cut_height * 10. * P$BDGR,
                            self$BMGRp)
      self$BMDR[j] = ifelse(self$BMDRp > self$cut_height * 10. * P$BDDR,
                            self$cut_height * 10. * P$BDDR,
                            self$BMDRp)
      
      # Harvested biomass is difference to BM before and after cut.
      self$hvBM[j] = self$hvBM[j - 1] + (self$BMGVp - self$BMGV[j]) +
                                (self$BMGRp - self$BMGR[j]) +
                                (self$BMDVp - self$BMDV[j]) +
                                (self$BMDRp - self$BMDR[j])
      # Update total biomass after cut.
      self$BM[j] = self$BMGV[j] + self$BMGR[j] + self$BMDV[j] + self$BMDR[j]
    },

    ## @description Construct a string full of meta information on a ModVege 
    ## run that can be inserted as a header to an output file.
    ##
    make_header = function() {
      logger("Start of ModVegeSite$make_header()", level = TRACE)
      # Start with the date and growR version ...
      header = sprintf("#date;%s", date())
      header = sprintf("%s\n#version;%s", header, self$version)
      # ... then add all parameters ...
      for (name in self$parameters$parameter_names) {
        parameter_value = self$parameters[[name]]
        header = sprintf("%s\n#%s;%s", header, name, parameter_value)
      }
      # Add additional info
      for (name in c("j_start_of_growing_season", "cut_DOYs", "SGS_method")) {
        # Access private or public values
        if (name %in% names(self)) {
          value = self[[name]]
        } else {
          value = private[[name]]
        }
        # Handle vectors
        if (length(value) > 1) {
          value = paste0(value, collapse = ", ")
        }
        # Safeguard against NULL values
        if (is.null(value)) {
          value = "NULL"
        }
        header = sprintf("%s\n#%s;%s", header, name, value)
      }

      # Finally, for double consistency, simulation year, site and run names
      header = sprintf("%s\n#year;%s\n#site_name;%s\n#run_name;%s", 
                       header, self$year, self$site_name, self$run_name)
      logger("End of ModVegeSite$make_header()", level = TRACE)
      return(header)
    },

    ## Plot only works, after the simulation has run.
    ##
    check_if_simulation_has_run = function() {
      if (private$current_DOY == 1) {
        warning("Cannot plot results because simulation has not yet been run.")
        return()
      }
    }
  ) # End of private attributes
)

## S3 dispatch methods

#' Plot ModVege simulation result overview
#'
#' This wraps the `ModvegeSite` instance's `plot()` method.
#'
#' @param x A [ModvegeSite] instance.
#' @param ... Arguments are passed on to [ModvegeSite]`$plot()`.
#' @return NULL, but plots to the active device.
#'
#' @seealso The different `[modvegeSite]$plot_XXX()` methods.
#'
#' @md
#' @export
plot.ModvegeSite = function(x, ...) {
  x$plot(...)
}

