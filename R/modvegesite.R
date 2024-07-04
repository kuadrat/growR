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
#' @seealso \link[=Autocut]{Autocut}
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
#' @field BM_after_cut Amount of biomass that remains after a cut #'   
#'   (determined through cut_height and biomass densities BDGV, BDDV, BDGR, 
#'   BDDR).
      BM_after_cut = NULL,
#' @field weather A list created by a [WeatherData] object's 
#'   `get_weather_for_year()` method.
      weather = NULL,
#' @field management A list containing management data as returned by 
#'    [ModvegeEnvironment]'s `get_environment_for_year()` method. If its 
#'    `is_empty` field is `TRUE`, the [Autocut] routine will be employed.
      management = NULL,
#' @field Autocut A subclass of [Autocut]. The algorithm used to determine
#'    cut events.
      Autocut = NULL,

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
        private$minBMGV = parameters$stubble_height * 10. * parameters$BDGV
        private$minBMGR = parameters$stubble_height * 10. * parameters$BDGR
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
        # Calculate everything that can be done before the loop
        P = self$parameters
        private$initialize_state_variables()
        private$calculate_temperature_sum()
        private$calculate_PETeff()
        self$ENVfPAR = fPAR(weather$PAR)
        self$ENVfT = fT(weather$Ta, P$T0, P$T1, P$T2)
        private$get_start_of_growing_season()

        # Determine whether cuts are to be read from input or to be 
        # determined algorithmically.
        if (management$is_empty) {
#          self$Autocut = PetersenAutocut$new(self)
          self$Autocut = PhenologicalAutocut$new(self)
          # Point the cut determination function to the autocut routine.
          self$determine_cut = self$Autocut$determine_cut
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
          private$apply_cuts()

          # Re-align LAI to BMGV
          self$LAI[j] = P$SLA * P$pcLAM * (self$BMGV[j] + self$BMGR[j]) / 10.
          self$LAIGV[j] = P$SLA * P$pcLAM * (self$BMGV[j]) / 10.
        }
        # Vectorially calculate derived quantities
        self$BM = self$BMGV + self$BMGR + self$BMDV + self$BMDR
        self$BMG = self$BMGV + self$BMGR
        private$calculate_digestibility()
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
        vars_to_plot = c("BM", "BMG", "hvBM", "dBM",
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
      #' if applicable, target biomass, dBM, BMG and hvBM.
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
        # BMG, dBM, hBM
        plot(self$BMG, type = "l", xlab = xlab, ylab = "BMG (kg / ha)")
        plot(box_smooth(self$dBM, smooth_interval), type = "l", 
             xlab = xlab, ylab = "smoothened dBM (kg / ha)")
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
    PETeff = NULL,
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
    minBMGV = NULL,
    minBMGR = NULL,

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

    ## @description Precalculate PETeff
    ##
    ## This is a purely meteorological variable and can thus be calculated 
    ## vectorially.
    calculate_PETeff = function() {
      W = self$weather

      # Apply crop coefficient
      private$PETeff = self$parameters$crop_coefficient * W$PET

      # Reduce PT when there is snow or precipitation
      pp_mask = W$PP > 1
      snow_mask = W$snow > 5
      private$PETeff[pp_mask] = 0.7 * private$PETeff[pp_mask]
      private$PETeff[snow_mask] = 0.2
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
                                           consider_snow = TRUE,
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
      PETeff = private$PETeff[j]
      PTr = PETeff * (1. - exp(-0.6 * LAI.ET))

      ATr = PTr * fW(self$WRp / P$WHC, PETeff)
      PEv = PETeff -  PTr
      AEv = PEv * self$WRp / P$WHC
      self$AET[j] = ATr + AEv

      # Soil moisture budget
      self$WR[j] = max(0., 
                       min(P$WHC, 
                           self$WRp + W$liquidP[j] + W$melt[j] - self$AET[j]))

      # Environmental constraints which cannot be calculated vectorially.
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
      next_BMGV = self$BMGVp + dBMGV * self$time_step
      dBMGR = self$GROGR - self$SENGR
      next_BMGR = self$BMGRp + dBMGR * self$time_step
      self$BMGV[j] = next_BMGV
      self$BMGR[j] = next_BMGR
      # Prevent biomass collapse if we are beyond ST2
      if (self$ST[j] >= P$ST2) {
        if (next_BMGV < private$minBMGV) {
          self$BMGV[j] = private$minBMGV
          dBMGV = self$BMGV[j] - self$BMGV[j-1]
        }
        if (next_BMGR < private$minBMGR) {
          self$BMGR[j] = private$minBMGR
          dBMGR = self$BMGR[j] - self$BMGR[j-1]
        }
      }
 
      dBMDV = (1. - P$sigmaGV) * self$SENGV - self$ABSDV
      self$BMDV[j] = self$BMDVp + dBMDV * self$time_step
      dBMDR = (1. - P$sigmaGR) * self$SENGR - self$ABSDR
      self$BMDR[j] = self$BMDRp + dBMDR * self$time_step
      
      # Today's biomass change (dBM) without cuts.
      self$dBM[j] = dBMGV + dBMGR + dBMDV + dBMDR
    },

    ## @description 
    ## Caclulate the organic matter digestibility of each compartment.
    ##
    calculate_digestibility = function() {
      P = self$parameters
      self$OMDGV = P$maxOMDGV - (self$AgeGV * (P$maxOMDGV - P$minOMDGV) / P$LLS)
      self$OMDGR = P$maxOMDGR - (self$AgeGR * (P$maxOMDGR - P$minOMDGR) / 
                                 (P$ST2 - P$ST1))
      # Total digestibility
      self$OMD = (self$OMDGV * self$BMGV + self$OMDGR * self$BMGR + 
                  P$OMDDV * self$BMDV + P$OMDDR * self$BMDR) / self$BM
      
      # Digestibility of green biomass
      self$OMDG = (self$OMDGV * self$BMGV + self$OMDGR * self$BMGR) / self$BMG
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


