#' Management Data Class
#'
#' Data structure that contains management data which can serve as input to 
#' an rmodvege simulation run.
#'
#' @field management_file string The file that was read.
#' @field is_empty boolean Used to determine if management data is present or 
#'   not. In the latter case, rmodvege will simulate management decisions 
#'   automatically.
#' @field cut_years numeric Vector of length *N* where *N* is the total 
#'   number of cuts read from the input file. Gives the year in which 
#'   corresponding cut was made.
#' @field cut_DOY numeric Vector of length *N* giving the day of year (as an 
#'   integer) on which a cut was made.
#' @field intensity string Management intensity for "autocut". One of 
#'   c("high", "middle", "low").
#'
#' @export
ManagementData = R6Class(
  "ManagementData",

  public = list(
  #-Public-attributes-----------------------------------------------------------
    management_file = NULL,
    is_empty = TRUE,
    cut_years = NULL,
    cut_DOY = NULL,
    intensity = NULL,

  #-Public-methods--------------------------------------------------------------
    #' @description Create a new ManagementData object.
    #'
    #' @param management_file string Path to file containing the management 
    #'   data to be read.
    #' @param years numeric Vector of years for which the management is to be 
    #'   extracted.
    #'
    #' @seealso [ManagementData$read_management()]
    initialize = function(management_file = NULL, years = "all") {
      if (!is.null(management_file)) {
        self$read_management(management_file, years)
      }
    },

    #' @description Read management data from supplied *management_file*.
    #'
    #' @param management_file Path to or name of file containing management data.
    #' @param years Years for which the management is to be extracted.  
    #'   Default is "all" to read in all found years.
    #'
    read_management = function(management_file, years = "all") {
      # Prepare the resulting container
      self[["management_file"]] = management_file
      if (file.exists(management_file)) {
        logger(sprintf("Loading management data from %s.", management_file), 
               level = DEBUG)
        cut_data = read.table(management_file, header = TRUE)
        # Only consider specified years
        if (years == "all") {
          selector = TRUE
        } else {
          selector = cut_data$year %in% years
        }
        self$is_empty = FALSE
        self$cut_years = cut_data$year[selector]
        self$cut_DOY = cut_data$DOY[selector]
        self$intensity = NA
      } else {
        logger(sprintf("Management file %s not found. Determining cuts 
                       automatically.", management_file), level = INFO)
        self$is_empty = TRUE
        self$cut_years = c()
        self$cut_DOY = c()
        # Set the parameter for the management intensity according to user 
        # specification or fall back to default value.
        choices = c("high", "middle", "low", "extensive")
        intensity = basename(management_file)
        if (intensity %in% choices) {
          self$intensity = intensity
          logger(sprintf("[autocut]Setting management intensity to `%s`.", 
                         intensity))
        } else {
          self$intensity = "high"
        }
      }
    },

    #' Extract management data for given year
    #'
    #' This simply filters out all data not matching *year* and returns a 
    #' list with the relevant keys, i.e. \describe{
    #'   \item{is_empty}{boolean Used to determine if management data is 
    #'   present or not. In the latter case, rmodvege will simulate 
    #'   management decisions automatically.}
    #'   \item{cut_years}{numeric Vector of length *N* where *N* is the total 
    #'   number of cuts read from the input file. Gives the year in which 
    #'   corresponding cut was made.}
    #'   \item{cut_DOY}{numeric Vector of length *N* giving the day of year (as an 
    #'   integer) on which a cut was made.}
    #'   \item{intensity}{string Management intensity for "autocut". One of 
    #'   c("high", "middle", "low").}
    #'   \item{n_cuts}{integer Number of cuts occurring in given year.}
    #' }
    #'
    #' @param year integer Year for which to extract management data.
    get_management_for_year = function(year) {
      M = list(is_empty = self$is_empty,
               intensity = self$intensity)
      i_year = which(self$cut_years == year)
      M[["cut_DOY"]] = self$cut_DOY[i_year]
      M[["cut_years"]] = self$cut_years[i_year]
      M[["n_cuts"]] = length(M[["cut_DOY"]])
      return(M)
    }
  )
)
