#' growR environment data
#'
#' @description
#' Data structure that contains inputs (parameters pertaining to a site, to 
#' the vegetation, to the weather and to the management) to growR simulations.
#'
#' @details
#' This class contains site parameters, weather and management data for 
#' one simulation run of growR on a given site over several years. 
#' Methods are provided to allow access to relevant data for a given year.
#'
#' All inputs are read in from data files through the respective data classes
#' [WeatherData], [ManagementData] and [ModvegeParameters]. These parameters 
#' can be simultaneously specified through a config file using [read_config()].
#'
#' @seealso [read_config()]
#'
#' @md
#' @export
ModvegeEnvironment = R6Class(
  "ModvegeEnvironment",

  public = list(
#-Public-attributes-------------------------------------------------------------

#' @field site_name Name of site to be simulated.
    site_name = NULL,
#' @field run_name Name of simulation run. Allows distinguishing between 
#'   different simulations at the same site. Defaults to "-" for *no name*.
    run_name = NULL,
#' @field run_name_in_filename How the run name will be represented in an 
#'   output file. If `run_name` is the default "-", indicating no name, this 
#'  will be an empty string. Otherwise, it will be the `run_name` prepended 
#'  by and underscore `_`.
    run_name_in_filename = NULL,
#' @field years Years for which environment data (weather & management) is 
#'   present.
    years = NULL,
#' @field param_file Name of supplied parameter file.
    param_file = NULL,
#' @field weather_file Name of supplied weather file.
    weather_file = NULL,
#' @field management_file Name of supplied management file.
    management_file = NULL,
#' @field parameters A [ModvegeParameters] object.
    parameters = NULL,
#' @field weather A [WeatherData] object.
    weather = NULL,
#' @field management A [ManagementData] object.
    management = NULL,
#' @field input_dir Directory in which parameter, weather and management 
#'   files are searched for. Defaults to `getOption("growR.input_dir").
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
    #'   Defaults to `getOption("growR.input_dir")`.
    #'
    initialize = function(site_name,
                          run_name = "-",
                          years = NULL,
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
        self$input_dir = getOption("growR.input_dir", 
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
      # Assume files are already given with complete paths, if input_dir is 
      # empty
      if (self$input_dir == "") {
        build_path = function(filename) {
          return(filename)
        }
      } else {
        build_path = function(filename) {
          return(file.path(self$input_dir, filename))
        }
      }
      self$parameters = ModvegeParameters$new(build_path(self$param_file))
      self$weather = WeatherData$new(build_path(self$weather_file), self$years)
      self$management = ManagementData$new(build_path(self$management_file))
      if (is.null(self$years)) {
        self$years = self$weather$years
      }
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
    #' @seealso [WeatherData]`$get_weather_for_year()`, 
    #'   [ManagementData]`$get_management_for_year()`
    #'
    get_environment_for_year = function(year) {
      W = self$weather$get_weather_for_year(year)
      M = self$management$get_management_for_year(year)
      return(list(W=W, M=M))
    }

  ) # End of public methods and attributes.
)

#' Provide an example ModvegeEnvironment
#'
#' This is intended for testing and for the examples in the documentation.
#'
#' @param site Choose for which example site an environment is to be created. 
#'   Options: `"posieux"`, `"sorens"`.
#' @return E A `[ModvegeEnvironment]` instance based on the example data for 
#'   *site* which is shipped with this package.
#'
#' @examples
#' extdata = system.file("extdata", package = "growR") 
#' print(extdata)
#' list.files(extdata, recursive = TRUE)
#' create_example_environment()
#'
#' @md
#' @export
create_example_environment = function(site = "posieux") {
  extdata = system.file("extdata", package = "growR")
  param_file = file.path(extdata, paste0(site, "_parameters.csv"))
  weather_file = file.path(extdata, paste0(site, "_weather.txt"))
  management_file = file.path(extdata, paste0(site, "_management1.txt"))
  return(ModvegeEnvironment$new(paste0(site, "1"), 
                                param_file = param_file,
                                weather_file = weather_file,
                                management_file = management_file,
                                years = 2013:2016,
                                input_dir = ""))
}
