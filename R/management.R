#' Management Data Class
#'
#' Data structure that contains management data which can serve as input to 
#' a [ModvegeSite] simulation run.
#'
#' @md
#' @export
ManagementData = R6Class(
  "ManagementData",

  public = list(
  #-Public-attributes-----------------------------------------------------------
#' @field management_file string The file that was read.
    management_file = NULL,
#' @field is_empty boolean Used to determine if management data is present or 
#'   not. In the latter case, [ModvegeSite] will simulate management decisions 
#'   automatically.
    is_empty = TRUE,
#' @field years List of unique years for which data is available.
    years = NULL,
#' @field cut_years numeric Vector of length *N* where *N* is the total 
#'   number of cuts read from the input file. Gives the year in which 
#'   corresponding cut was made.
    cut_years = NULL,
#' @field cut_DOY numeric Vector of length *N* giving the day of year (as an 
#'   integer) on which a cut was made.
    cut_DOY = NULL,
#' @field intensity string Management intensity for "autocut". One of 
#'   c("high", "middle", "low").
    intensity = NULL,

  #-Public-methods--------------------------------------------------------------
    #' @description Create a new ManagementData object.
    #'
    #' @param management_file string Path to file containing the management 
    #'   data to be read.
    #' @param years numeric Vector of years for which the management is to be 
    #'   extracted.
    #'
    #' @seealso [ManagementData]`$read_management()`
    initialize = function(management_file = NULL, years = NULL) {
      if (!is.null(management_file)) {
        self$read_management(management_file, years)
      }
    },

    #' @description Read management data from supplied *management_file*.
    #'
    #' @param management_file Path to or name of file containing management data.
    #' @param years Years for which the management is to be extracted.  
    #'   Default (NULL) is to read in all found years.
    #' @return None The object's field are filled.
    #'
    read_management = function(management_file, years = NULL) {
      # Prepare the resulting container
      self[["management_file"]] = management_file
      if (file.exists(management_file)) {
        logger(sprintf("Loading management data from %s.", management_file), 
               level = DEBUG)
        cut_data = read.table(management_file, header = TRUE)
        # Carry out basic input checks
        self$ensure_file_integrity(cut_data)

        # Only consider specified years
        if (is.null(years)) {
          selector = TRUE
        } else {
          selector = cut_data$year %in% years
        }
        self$is_empty = FALSE
        self$cut_years = cut_data$year[selector]
        self$years = unique(self$cut_years)
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
        choices = c("high", "middle", "medium", "low", "extensive")
        intensity = basename(management_file)
        if (intensity %in% choices) {
          self$intensity = intensity
          logger(sprintf("[autocut]Setting management intensity to `%s`.", 
                         intensity), level = INFO)
        } else {
          logger(sprintf("[autocut]Management intensity `%s` not recognized.",
                         "Setting intensity to `high`.", 
                         intensity), level = INFO)
          self$intensity = "high"
        }
      }
    },

    #' @description Check that all required columns are present and that cut 
    #' DOYs are only increasing in a given year.
    #'
    #' @param cut_data data.frame containing the cut data.
    #'
    ensure_file_integrity = function(cut_data) {
      required = c("year", "DOY")
      data_name = sprintf("the supplied management file `%s`", 
                          self$management_file)
      ensure_table_columns(required, cut_data, data_name = data_name)

      # EnsureDOYs are always increasing within a year.
      for (year in unique(cut_data$year)) {
        this_years_DOYs = cut_data[cut_data$year == year,]$DOY
#        monotonic = all(cummax(this_years_DOYs) == this_years_DOYs)
        not_monotonic = is.unsorted(this_years_DOYs, strictly = TRUE)
        if (not_monotonic) {
          msg = paste("DOYs are not monotonically increasing in year %s in",
                      "supplied management file `%s`.")
          logger(sprintf(msg, year, self$management_file), level = ERROR)
        }
      }
    },

    #' @description Extract management data for given year
    #'
    #' This simply filters out all data not matching *year* and returns a 
    #' list with the relevant keys.
    #'
    #' @param year integer Year for which to extract management data.
    #' @return M A list containing the keys: 
    #'   \describe{
    #'   \item{is_empty}{boolean Used to determine if management data is 
    #'   present or not. In the latter case, [ModvegeSite] will simulate 
    #'   management decisions automatically.}
    #'   \item{cut_years}{numeric Vector of length *N* where *N* is the total 
    #'   number of cuts for this *year*, as read from the input file. Gives 
    #'   the year in which corresponding cut was made.}
    #'   \item{cut_DOY}{numeric Vector of length *N* giving the day of year (as an 
    #'   integer) on which a cut was made.}
    #'   \item{intensity}{string Management intensity for "autocut". One of 
    #'   c("high", "middle", "low").}
    #'   \item{n_cuts}{integer Number of cuts occurring in given year.}
    #' }
    #'   The two vectors in `cut_DOY` and `cut_years` 
    #'   differ from this object's respective fields in that only data for 
    #'   selected year is present.
    #'
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
