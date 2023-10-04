#' Data structure that contains inputs to rmodvege simulations
#'
#' This class contains site parameters, weather and management data for 
#' one simulation run of rmodvege on a given site over several years. 
#' Methods are provided to allow access to relevant data for a given year.
#'
#' All inputs are read in from data files through the respective data classes
#' [WeatherData], [ManagementData] and [ModvegeParameters]. These parameters 
#' can be simultaneously specified through a config file using [read_config()].
#'
#' @seealso [read_config()]
#'
#' @export
ModvegeEnvironment = R6Class(
  "ModvegeEnvironment",

  public = list(
#-Public-attributes-------------------------------------------------------------

    site_name = NULL,
    run_name = NULL,
    run_name_in_filename = NULL,
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
    #'   Defaults to "-", which indicates no specific run name.
    #' @param years numeric Vector of integer years to be simulated.
    #' @param param_file string Name of file that contains site and 
    #'   vegetation parameters. If default value `"-"` is  provided, it is 
    #'   assumed to be "SITENAME_parameters.csv".
    #' @param weather_file string Analogous to *param_file*.
    #' @param management_file string Analogous to *param_file*.
    #' @param input_dir string Path to directory containing input files. 
    #'   Defaults to `getOption("rmodvege.input_dir")`.
    #'
    initialize = function(site_name,
                          run_name = "-",
                          years = "all",
                          param_file = "-",
                          weather_file = "-",
                          management_file = "-",
                          input_dir = NULL) {
      # Set instance variables
      self$site_name = site_name
      self$run_name = run_name
      self$run_name_in_filename = self$make_filename_for_run(run_name)
      self$years = years
      # Revert to defaults for the not provided values.
      if (param_file == "-") {
        self$param_file = paste0(site_name, "_parameters.csv")
      } else {
        self$param_file = param_file
      }
      if (weather_file == "-") {
        self$weather_file = paste0(site_name, "_weather.txt")
      } else {
        self$weather_file = weather_file
      }
      if (management_file == "-") {
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
      self$management = ManagementData$new(file.path(self$input_dir,
                                                     self$management_file))
    },

    #' @description Ensure a readable filename for given *run_name*.
    #'
    #' @param run_name Name of run to be converted into a filename.
    #'
    #' @return A version of *run_name* that can be used in a filename.
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

    #' @description Get weather and environment for given year
    #'
    #' Convenience function to retrieve environmental and management
    #' inputs for given *year* from multi-year data containers `self$weather` 
    #' and `self$management`.
    #'
    #' @param year int; year for which to extract data.
    #'
    #' @return `list(W, M)` where `W` is the WeatherData and `M` the 
    #'   ManagementData object for given year.
    #'
    #' @seealso [WeatherData$get_weather_for_year()], 
    #'   [ManagementData$get_management_for_year()]
    #'
    get_environment_for_year = function(year) {
      W = self$weather$get_weather_for_year(year)
      M = self$management$get_management_for_year(year)
      return(list(W=W, M=M))
    }

  ) # End of public methods and attributes.
)
