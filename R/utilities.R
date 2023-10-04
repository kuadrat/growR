#' Constants representing different debug levels to be used internally.
ERROR = 1
WARNING = 2
INFO = 3
DEBUG = 4
TRACE = 5

#' Names of debug levels.
DEBUG_LEVELS = c("ERROR", "WARNING", "INFO", "DEBUG", "TRACE")

#' Set verbosity of rmodvege output.
#'
#' @param level Integer representing one of the following levels:
#'   1: ERROR, 2: WARNING, 3: INFO, 4: DEBUG, 5: TRACE
#'   Messages with a level higher than the specified *level* are suppressed.
#'   In other words, higher values of *level* lead to more output and vice 
#'   versa.
#'
#' @export
set_rmodvege_verbosity = function(level) {
  options("rmodvege.verbosity" = level)
}

#' Primitive logger for debugging.
#'
#' @param msg The message to print.
#' @param level The message will only be printed if its *level* is lower 
#'        or equal to *DEBUG_LEVEL*
#' @param stop_on_error Can be set to FALSE in order to continue 
#' execution despite emitting a message of *level* ERROR.
#'
#' @export
logger = function(msg = "", level = DEBUG, stop_on_error = TRUE) {
  DEBUG_LEVEL = getOption("rmodvege.verbosity", default = DEBUG)
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
#'   to `getOptions("rmodvege.input_dir", default = file.path("input"))`.
#'
#' @return A list of `ModvegeEnvironment` instances corresponding to the 
#'   configurations in the order they appear in *config_file*.
#'
#' @export
#'
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

