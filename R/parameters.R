# Define the names of the variables in the model. This is also used for a 
# sanity check of the supplied input.
initial_condition_names = c(
  "AgeGV",
  "AgeGR",
  "AgeDV",
  "AgeDR",
  "BMGV",
  "BMGR",
  "BMDV",
  "BMDR",
  "SENGV",
  "SENGR",
  "ABSDV",
  "ABSDR",
  "ST",
  "cBM"
)
parameter_names = c(
  "LAT",
  "LON",
  "ELV",
  "WHC",
  "NI",
  "w_FGA",
  "w_FGB",
  "w_FGC",
  "w_FGD",
  "RUEmax",
  "sigmaGV",
  "sigmaGR",
  "T0",
  "T1",
  "T2",
  "KGV",
  "KGR",
  "KlDV",
  "KlDR",
  "OMDDV",
  "OMDDR",
  "CO2_growth_factor"
)
# Create a list that can be inserted into R6 class to create many fields
all_parameter_names = c(initial_condition_names, parameter_names)
parameters = as.list(all_parameter_names)
names(parameters) = all_parameter_names
sapply(all_parameter_names, function(p) {parameters[[p]] = NA})
# Same procedure for functional group parameters
fg_parameters = as.list(FG_A$fg_parameter_names)
names(fg_parameters) = FG_A$fg_parameter_names
sapply(FG_A$fg_parameter_names, function(p) {fg_parameters[[p]] = NA})
# Similar procedure for initial_conditions:
# For each value that needs to be stored as an initial condition, create a 
# copy of the variable with an appended "0" to the variable name.
initial_condition_values = list()
for (i_param in 1:length(initial_condition_names)) {
  old_name = initial_condition_names[i_param]
  new_name = paste0(old_name, "0")
  initial_condition_values[[new_name]] = NA
}
for (name in c("WR0", "OMDGV0", "OMDGR0")) {
  initial_condition_values[[name]] = NA
}

#' Parameter Data Object
#'
#' @description
#' Data structure that contains site and vegetation parameters necessary for 
#' the configuration of an rmodvege simulation run.
#'
#' @details 
#' # Parameter description
#' The following is a list and description of model parameters, including 
#' the vegetation parameters, which are defined through the functional group 
#' composition.
#'
#' ## Site and model parameters
#' ```{r child = "man/parameters.Rmd"}
#' ```
#' 
#' ## Initial conditions
#' ```{r child = "man/initial_conditions.Rmd"}
#' ```
#'
#' ## Vegetation parameters
#' ```{r child = "man/parameters_fg.Rmd"}
#' ```
#'
#' @field parameter_names Names of all parameters and state variables.
#' @field n_parameters Number of total parameters.
#' @field functional_group The functional group object holding the vegetation 
#'   parameters.
#' @field fg_parameter_names Names of vegetation parameters defined by the 
#'   functional group composition.
#' @field param_file Name of the parameter file from which initial parameter 
#'   values were read.
#'
#' @note
#' Programmatically speaking, all parameters described under *Parameter 
#' description* are also fields of this R6Class.
#'
#' @md
#' @export
ModvegeParameters = R6Class(
  "ModvegeParameters",

  public = c(
    parameters,
    fg_parameters,
    initial_condition_values,

    list(
      #-Public-attributes-----------------------------------------------------------
      parameter_names = all_parameter_names,
      n_parameters = length(parameter_names),
      functional_group = NULL,
      fg_parameter_names = NULL,
      param_file = NULL,

      #-Public-methods--------------------------------------------------------------

      #' @description Constructor
      #'
      #' @param param_file Name of file containing the site and vegetation 
      #'   parameters.
      #'
      initialize = function(param_file = NULL) {
        # Initialize a functional group to access its FG param names
        self$functional_group = FunctionalGroup$new()
        # Make a shorthand
        self$fg_parameter_names = self$functional_group$fg_parameter_names
        if (!is.null(param_file)) {
          self$read_parameters(param_file)
        }
      },

      #' @description
      #' Read parameters from parameter file
      #'
      #' Reads in parameters from the supplied *param_file* and stores them in 
      #' internal fields.
      #'
      #' This function carries out some basic sanity checks of 
      #' the supplied *param_file* and reports on unidentified and missing 
      #' parameter names.
      #'
      #' @param param_file Path or name of file to read parameters from.
      #' @return P List with field names as in the class variable 
      #'   `parameter_names`.
      #'
      read_parameters = function(param_file) {
        if (!file.exists(param_file)) {
          stop(sprintf("Parameter file `%s` not found.", param_file))
        }
        # Read the file and store values and names.
        logger(sprintf("Reading parameters from `%s`.", param_file))
        parameters = read.csv(param_file, sep = ",", stringsAsFactors = FALSE)
        read_parameter_names = parameters$name
        parameter_values = parameters$value
        n_read_parameters = length(read_parameter_names)

        # Sanity check: compare supplied parameters with expected.
        not_present = setdiff(self$parameter_names, read_parameter_names)
        not_known = setdiff(read_parameter_names, self$parameter_names)
        # Warn if some arguments were not recognized.
        if (length(not_known) != 0) {
          unknown_args = paste(not_known, collapse = "\n")
          message = paste("The following unrecognized arguments were present",
                            "in the supplied input file (%s):\n%s")
          logger(sprintf(message, param_file, unknown_args), level = WARNING)
        }
        # Give error if an argument is missing.
        if (length(not_present) != 0 ) {
          # Construct the error message.
          missing_args = paste(not_present, collapse = "\n")
          message = paste("The following arguments were missing",
                          "from the supplied input file (%s):\n%s")
          logger(sprintf(message, param_file, missing_args), level = ERROR)
        }

        # Now that the input looks OK, read it into the internal fields
        self[["param_file"]] = param_file
        for (i_param in 1:n_read_parameters) {
          if (!read_parameter_names[i_param] %in% not_known) {
            self[[read_parameter_names[i_param]]] = parameter_values[i_param]
          }
        }

        # Update the functional group parameters in P.
        self$update_functional_group()

        # Set initial conditions
        for (name in initial_condition_names) {
          old_name = name
          new_name = paste0(old_name, "0")
          self[[new_name]] = self[[old_name]]
        }

        # Set some more initial values 
        self[["WR0"]] = self[["WHC"]]
        self[["OMDGV0"]] = self[["maxOMDGV"]]
        self[["OMDGR0"]] = self[["maxOMDGR"]]
      },

      #' @description Update functional group parameters
      #'
      #' Should be run whenever the functional group composition is changed in 
      #' order to reflect the changes in the parameter list `self$P`.
      update_functional_group = function() {
        w_A = self[["w_FGA"]]
        w_B = self[["w_FGB"]]
        w_C = self[["w_FGC"]]
        w_D = self[["w_FGD"]]
        w_tot = w_A + w_B + w_C + w_D

        FG_eff = w_A/w_tot * FG_A +
                 w_B/w_tot * FG_B +
                 w_C/w_tot * FG_C +
                 w_D/w_tot * FG_D

        # Update to the new values in P
        fg_parameters = FG_eff$get_parameters()
        for (name in self$fg_parameter_names) {
          self[[name]] = fg_parameters[[name]]
        }
      }
    )
  )
)

