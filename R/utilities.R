## Helper to get user input from scripts or interactive sessions
prompt_user = function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n = 1))
  }
}

#' Check if *package* is available
#'
#' Some functions not pertaining to the package core require additional 
#' libraries. These libraries are listed as *suggested* in the `DESCRIPTION` 
#' When such a function is called by a user who does not have the respective 
#' libraries installed, we should notice that and notify the user. This is the 
#' purpose of this *function* `check_for_package`.
#'
#' The function checks if *package* is installed and loaded. If not, it 
#' either produces a warning or throws an error, depending on the value of 
#' *stop*.
#'
#' @param package Name of the package to check for.
#' @param stop Toggle whether an error should be thrown (`TRUE`) or a warning 
#'   generated (`FALSE`).
#'
#' @return `TRUE` if the package was found. `FALSE` if it wasn't found and 
#'   *stop* is `FALSE`. Otherwise, an error will be thrown.
#'
check_for_package = function(package, stop = TRUE) {
  if (!requireNamespace(package, quietly = TRUE)) {
    message = sprintf("Package `%s` is required for this function to run, but 
                      it was not found.")
    if (stop) {
      stop(paste(message, "Exiting."), call. = FALSE)
    } else {
      warning(paste(message, "Using alternative code."))
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Constants representing different debug levels to be used internally.
ERROR = 1
WARNING = 2
INFO = 3
DEBUG = 4
TRACE = 5

#' Names of debug levels.
DEBUG_LEVELS = c("ERROR", "WARNING", "INFO", "DEBUG", "TRACE")

#' Set verbosity of growR output.
#'
#' @param level Integer representing one of the following levels:
#'   1: ERROR, 2: WARNING, 3: INFO, 4: DEBUG, 5: TRACE
#'   Messages with a level higher than the specified *level* are suppressed.
#'   In other words, higher values of *level* lead to more output and vice 
#'   versa.
#' @return None Sets the option `"growR.verbosity".
#'
#' @examples
#' # At level 3, only one of the three following messages are printed.
#' set_growR_verbosity(3)
#' logger("Message on level 5.", level = 5)
#' logger("Message on level 4.", level = 4)
#' logger("Message on level 3.", level = 3)
#' # At level 5, all three are printed.
#' set_growR_verbosity(5)
#' logger("Message on level 5.", level = 5)
#' logger("Message on level 4.", level = 4)
#' logger("Message on level 3.", level = 3)
#' # Reset to default.
#' set_growR_verbosity()
#'
#' @export
set_growR_verbosity = function(level = 3) {
  options("growR.verbosity" = level)
}

#' Primitive logger for debugging.
#'
#' @param msg The message to print.
#' @param level The message will only be printed if its *level* is lower 
#'        or equal to *DEBUG_LEVEL*
#' @param stop_on_error Can be set to FALSE in order to continue 
#' execution despite emitting a message of *level* ERROR.
#' @return None Prints console output.
#'
#' @seealso [set_growR_verbosity()]
#'
#' @examples
#' logger("A standard message", level = 3)
#' logger("A debug message", level = 4)
#' logger("A deep debug message", level = 5)
#'
#' @md
#' @export
logger = function(msg = "", level = DEBUG, stop_on_error = TRUE) {
  DEBUG_LEVEL = getOption("growR.verbosity", default = DEBUG)
   # Do nothing if the supplied level is insufficient
  if (level > DEBUG_LEVEL) {
    return()
  }
  # Construct the message and decide on the severity of how to display it
  message = paste("[", DEBUG_LEVELS[level], "]", msg, "\n", sep="")
  if (level == ERROR & stop_on_error) {
    stop(message)
  } else {
    cat(message)
  }
}

#' Replace given filename by a version that contains an incremental number 
#' in order to prevent overwriting existing files.
#'
#' @param path string; Filename including path for which to check uniqueness.
#' @param add_num boolean; if TRUE, add the incremental number anyways, even 
#'   if no filename conflict exists.
#' @return A unique filename.
#'
ensure_unique_filename = function(path, add_num = TRUE) {
  # If given filename is already unique, do nothing
  if (!add_num && !file.exists(path)) {
    return(path)
  }
  # Otherwise, figure out the highest existing number with this basename
  dir = dirname(path)
  filename = basename(path)
  # Insert a file count number at the end of the filename, before the suffix
  # The following assumes that there are no "." present in the filename
  splits = strsplit(filename, "\\.")[[1]]
  basefilename = splits[1]
  suffix = splits[2]
  # Check for existing files with count number
  file_list = list.files(path = dir, 
                         pattern = paste0("^",
                                          basefilename, 
                                          "_[0-9][0-9][0-9]\\.",
                                          suffix)
                         )
  if (length(file_list) == 0 ) {
    next_number = 1
  } else {
    # Identify the largest number among the ones present
    file_numbers = as.numeric(gsub(paste0(basefilename, "_|", suffix), 
                                   "", 
                                   file_list))
    next_number = max(file_numbers) + 1
  }
  # Build the resulting full filename
  unique_path = sprintf("%s/%s_%03d.%s",
                        dir,
                        basefilename,
                        next_number,
                        suffix)
  return(unique_path)
}

#' Write *data* to supplied file in append mode without generating a warning 
#' message.
#'
#' This function essentially wraps `write.table` with a calling handler that
#' suppresses appending warnings that would appear with the argument 
#' `col.names = TRUE`.
#' 
#' @param data Any object which can be handled by `write.table`.
#' @param filename Name of file to append to.
#' @param ... All additional arguments are passed to `write.table`.
#'
append_to_table = function(data, filename, ...) {
  # Check for English OR (|) German warning message texts
  message_en = "appending column names to file"
  message_de = "Spaltennamen an Datei an"
  messages = sprintf("%s|%s", message_en, message_de)
  withCallingHandlers(
    write.table(data,
                file = filename,
                append = TRUE,
                ...),
    warning = function(w) {
      if (grepl(messages, conditionMessage(w))) { 
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' Read simulation run configurations from file
#'
#' The format of the configuration file is expected to contain 6 
#' space-separated columns representing, in order: 
#'   \describe{
#'     \item{site_name}{Name of simulated site. This is used, for example, 
#'           when an output file is created.}
#'     \item{run_name}{Name of this simulation run. Used to differentiate 
#'           between different runs at the same site. Can be `-` to indicate 
#'           no particular name, in which case nothing will be appended in 
#'           the resulting output file.}
#'     \item{year(s)}{Specification of years to be simulated. Either a single 
#'           number or a sequence in R's `:` notation, i.e. `2013:2022` to 
#'           indicate all years from 2013 to (including) 2022.}
#'     \item{param_file}{Filename (not full path) of parameter file to use. 
#'           The file is assumed to be located in *input_dir* (confer 
#'           documentation for that parameter).}
#'     \item{weather_file}{Filename (not full path) of weather file. See also 
#'           *param_file*.}
#'     \item{management_file}{Filename (not full path) of management file. 
#'           See also *param_file*. Can be set to `high`, `middle`, `low` or 
#'           `-` if no management data is to be used and the *autocut* 
#'           routine shall be employed to simulate cutting events.}
#'  }
#'  Rows starting with a `#` are skipped.
#'
#' @param config_file Path to the configuration file to be read.
#' @param input_dir Path to directory where input files are located. Defaults 
#'   to `getOptions("growR.input_dir", default = file.path("input"))`.
#'
#' @return A list of `ModvegeEnvironment` instances corresponding to the 
#'   configurations in the order they appear in *config_file*.
#'
#' @examples
#' # First, we set up the expected directory structure in a temporary place
#' tmp = file.path(tempdir(), "test-read_config")
#' dir.create(tmp)
#'
#' # We need `force = TRUE` here in order to make the example work in 
#' # non-interactive settings.
#' setup_directory(root = tmp, include_examples = TRUE, force = TRUE)
#' 
#' # Now we can test `read_config`.
#' read_config(file.path(tmp, "example_config.txt"),
#'             input_dir = file.path(tmp, "input"))
#'
#'
#' @md
#' @export
read_config = function(config_file, input_dir = NULL) {
  config = read.table(config_file)
  # Use the running index *I* here instead of concrete indexing. This 
  # makes it easier to adapt to a change in the structure of the config 
  # file.
  I = 1
  site_names = config[[I]]
  I = I + 1
  run_names = config[[I]]
  I = I + 1
  year_strings = config[[I]]
  I = I + 1
  param_files = config[[I]]
  I = I + 1
  weather_files = config[[I]]
  I = I + 1
  management_files = config[[I]]
  n_runs = length(site_names)
  
  run_years = parse_year_strings(year_strings)

  environments = list()
  # Load all inputs from respective files
  for (run in 1:n_runs) {
    sim_years = run_years[[run]]
    E = ModvegeEnvironment$new(site_name = site_names[[run]],
                               run_name = run_names[[run]],
                               years = run_years[[run]],
                               param_file = param_files[[run]],
                               weather_file = weather_files[[run]],
                               management_file = management_files[[run]],
                               input_dir = input_dir)
    environments[[run]] = E
  }
  return(environments)
}

#' Parse and generate lists of years.
#' 
#' @param year_strings A vector of strings that each either represents a 
#'   single year or a sequence of year in the format `start:stop`.
#'
#' @return run_years List of integer vectors, representing the years to 
#'   simulate for each run.
#'
parse_year_strings = function(year_strings) {
  run_years = list()
  max_n_years = 0
  for (year_string in year_strings) {
    # Check if a range is given
    if (grepl(":", year_string)) {
      split = strsplit(year_string, ":")[[1]]
      start_year = as.integer(split[1])
      stop_year = as.integer(split[2])
    } else {
      start_year = as.integer(year_string)
      stop_year = start_year
    }
    years = start_year:stop_year
    # Keep track of maximum number of years for core assignment
    n_years = length(years)
    if (n_years > max_n_years) {
      max_n_years = n_years
    }
    run_years = append(run_years, list(years))
  }
  return(run_years)
}

#' Endpoint smoother
#' 
#' @description
#' Smooth data in vector *x* to its endpoint.
#'
#' @details
#' Employ an endpoint box filter (aka "running mean" or midpoint smoother) to the 
#' 1-D data in *x*:
#' `x_smoothed[i] = mean(x[i-box_width:i])`
#' Where *x* is considered to be *zero-padded* vor values of *i-box_width* < 1.
#'
#' @param x 1D data to be smoothed.
#' @param box_width Width (in units of vector steps) of the box used for 
#'   smoothing.
#'
#' @return x_smooth Smoothened version of *x*.
#'
#' @examples
#' # Create a sine wave with noise
#' x = seq(0, 4*pi, 0.1)
#' y = sin(x) + runif(length(x))
#' # Apply endpoint smoothing
#' y_smooth = box_smooth(y, box_width = 5)
#'
#' @export
box_smooth = function(x, box_width = 28) {
  # Do nothing for box_width <= 1
  if (box_width <= 1) {
    return(x)
  }
  
  n = length(x)
  x_smooth = numeric(n)
  # Employ the smoothing
  for (i in 1:n) {
    # Endpoint smoothing/integrating
    i_start = max(1, (i-box_width))
    x_smooth[i] = mean(x[i_start:i])
  }
  return(x_smooth)
}

