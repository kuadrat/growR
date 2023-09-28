#' Data structure that contains inputs to rmodvege simulations
#'
#' This class contains site parameters, weather and management data for 
#' one simulation run of rmodvege on a given site over several years. 
#' Methods are provided to allow access to relevant data for a given year.
#'
#' All inputs are read in from data files through the respective data classes
#' [WeatherData], [ManagementData] and [ModvegeParameters]. These parameters 
#' can be simultaneously specified through a config file.
#'
#' @export
ModvegeEnvironment = R6Class(
  "ModvegeEnvironment",

  public = list(
#-Public-attributes-------------------------------------------------------------

    site_name = NULL,
    run_name = NULL,
    years = NULL,
    param_file = NULL,
    weather_file = NULL,
    management_file = NULL,
    parameters = NULL,
    weather = NULL,
    management = NULL,
    input_dir = NULL,

#-Public-methods----------------------------------------------------------------

    #' @description Instantiate a new ModvegeEnvironment
    #'
    #' @param site_name string Name of the simulated site.
    #' @param run_name string Name of the simulation run. Used to 
    #'   differentiate between different simulation conditions at the same site.
    #' @param years numeric Vector of integer years to be simulated.
    #' @param param_file string Name of file that contains site and 
    #'   vegetation parameters. If not provided, it is assumed to be 
    #'   "SITENAME_parameters.csv".
    #' @param weather_file string Analogous to *param_file*.
    #' @param management_file string Analogous to *param_file*.
    #' @param input_dir string Path to directory containing input files. 
    #'   Defaults to `getOption("rmodvege.input_dir")`.
    #'
    initialize = function(site_name,
                          run_name = "-",
                          years = "all",
                          param_file = NULL,
                          weather_file = NULL,
                          management_file = NULL,
                          input_dir = NULL) {
      # Set instance variables
      self$site_name = site_name
      self$run_name = run_name
      self$years = years
      # Revert to defaults for the not provided values.
      if (is.null(param_file)) {
        self$param_file = paste0(site_name, "_parameters.csv")
      } else {
        self$param_file = param_file
      }
      if (is.null(weather_file)) {
        self$weather_file = paste0(site_name, "_weather.txt")
      } else {
        self$weather_file = weather_file
      }
      if (is.null(management_file)) {
        self$management_file = paste0(site_name, "_management.txt")
      } else {
        self$management_file = management_file
      }
      if (is.null(input_dir)) {
        self$input_dir = getOption("rmodvege.input_dir", 
                                   default = file.path("input"))
      } else {
        self$input_dir = input_dir
      }
      # Read input files.
      self$load_inputs()
    },

    #' @description Load simulation inputs.
    #'
    #' Stores parameters, management and weather data from files specified in 
    #' self$parameter_file, self$weather_file and self$management_file, 
    #' respectively.
    #'
    load_inputs = function() {
      self$parameters = ModvegeParameters$new(file.path(self$input_dir, 
                                                        self$param_file))
      self$weather = WeatherData$new(file.path(self$input_dir,
                                               self$weather_file))
      self$parameters = ModvegeParameters$new(file.path(self$input_dir,
                                                        self$param_file))
    },

    #' Ensure a readable filename for given *run_name*.
    #'
    make_filename_for_run = function(run_name) {
      # Interpret the "-" symbol as an empty run name
      if (run_name == "-") {
        run_name_in_filename = ""
      } else {
        # Insert an underscore for better readability in output filename
        run_name_in_filename = paste0("_", run_name)
      }
      return(run_name_in_filename)
    },

    #' "Static" convenience function to retrieve environmental and management 
    #' inputs for given *year* from multi-year data containers.
    #'
    #' @param year int; year for which to extract data.
    #' @param WD list; contains weather data lists for different years. As 
    #'   present in self$weather_list[[run]].
    #' @param MD list; contains management data lists for different years. As 
    #'   present in self$management_list[[run]].
    #'
    #' @return list(W, M)
    #'
    get_environment_for_year = function(run, year) {
      WD = self$weather_list[[run]]
      MD = self$management_list[[run]]
      W = WD$get_weather_for_year(year)
      iC = which(MD[["cut_years"]] == year)
      M = MD
      M[["cut_DOY"]] = MD[["cut_DOYs"]][iC]
      M[["n_cuts"]] = length(M[["cut_DOY"]])
      return(list(W=W, M=M))
    }

  ) # End of public methods and attributes.
)
