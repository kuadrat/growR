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

