# Define the names of the variables in the model. This is also used for a 
# sanity check of the supplied input.
initial_conditions = list(
  AgeGV = 100,
  AgeGR = 2000,
  AgeDV = 500,
  AgeDR = 500,
  BMGV = 420,
  BMGR = 0,
  BMDV = 300,
  BMDR = 30,
  SENGV = 0,
  SENGR = 0,
  ABSDV = 0,
  ABSDR = 0,
  ST = 0,
  cBM = 0
)
required_parameters = list(
  WHC = NA,
  NI = NA,
  w_FGA = NA,
  w_FGB = NA,
  w_FGC = NA,
  w_FGD = NA
)
parameter_defaults = list(
  LAT = NA,
  LON = NA,
  ELV = NA,
  RUEmax = 3,
  sigmaGV = 0.4,
  sigmaGR = 0.2,
  T0 = 5,
  T1 = 10,
  T2 = 20,
  KGV = 0.002,
  KGR = 0.001,
  KlDV = 0.001,
  KlDR = 0.0005,
  OMDDV = 0.45,
  OMDDR = 0.4,
  CO2_growth_factor = 0.5,
  crop_coefficient = 1.15,
  senescence_cap = 0.7,
  stubble_height = 0.02,
  SGS_method = "MTD"
)
# Create a list that can be inserted into R6 class to create many fields
initial_condition_names = names(initial_conditions)
parameter_names = names(parameter_defaults)
required_parameter_names = names(required_parameters)
all_parameter_names = c(initial_condition_names, parameter_names, 
                        required_parameter_names)
parameters = c(initial_conditions, parameter_defaults, required_parameters)
# Similar procedure for functional group parameters, using the values of FG_A as 
# defaults.
fg_parameters = FG_A$get_parameters()
# Similar procedure for initial_conditions:
# For each value that needs to be stored as an initial condition, create a 
# copy of the variable with an appended "0" to the variable name.
initial_condition_values = list()
for (i_param in 1:length(initial_condition_names)) {
  old_name = initial_condition_names[i_param]
  new_name = paste0(old_name, "0")
  initial_condition_values[[new_name]] = initial_conditions[[old_name]]
}
for (name in c("WR0", "OMDGV0", "OMDGR0")) {
  initial_condition_values[[name]] = NA
}

#' Parameter Data Object
#'
#' @description
#' Data structure that contains site and vegetation parameters necessary for 
#' the configuration of an growR simulation run.
#'
#' @details 
#' # Parameter description
#' The following is a list and description of model parameters, including 
#' the vegetation parameters, which are defined through the functional group 
#' composition.
#'
#' ## Site and model parameters
#' ```{r child = "vignettes/children/parameters.Rmd"}
#' ```
#' 
#' ## Initial conditions
#' ```{r child = "vignettes/children/initial_conditions.Rmd"}
#' ```
#'
#' ## Vegetation parameters
#' ```{r child = "vignettes/children/parameters_fg.Rmd"}
#' ```
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
#' @field required_parameter_names Names of parameters that do not have a 
#'   default value and are therefore strictly required.
      required_parameter_names = required_parameter_names,
#' @field parameter_names Names of all required and optional parameters and 
#'   state variables.
      parameter_names = all_parameter_names,
#' @field n_parameters Number of total parameters.
      n_parameters = length(all_parameter_names),
#' @field functional_group The [FunctionalGroup] instance holding the 
#'   vegetation parameters.
      functional_group = NULL,
#' @field fg_parameter_names Names of vegetation parameters defined by the 
#'   functional group composition.
      fg_parameter_names = NULL,
#' @field initial_condition_names Names of initial conditions.
      initial_condition_names = c(initial_condition_names, 
                                  "WHC", "maxOMDGV", "maxOMDGR"),
#' @field param_file Name of the parameter file from which initial parameter 
#'   values were read.
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
        not_known = self$check_parameters(read_parameter_names, 
                                          check_for_completeness = TRUE)

        # Now that the input looks OK, read it into the internal fields
        self[["param_file"]] = param_file
        for (i_param in 1:n_read_parameters) {
          if (!read_parameter_names[i_param] %in% not_known) {
            self[[read_parameter_names[i_param]]] = parameter_values[i_param]
          }
        }

        # Update the functional group parameters in P.
        self$update_functional_group()

        private$update_initial_conditions()
      },

      #' @description Savely update the given parameters
      #'
      #' This is the preferred method for changing the internal parameter 
      #'  values, because special care is taken to account for potential 
      #' changes to functional group weights.
      #'
      #' @param params List of name-value pairs of the parameters to update.
      #'
      set_parameters = function(params) {
        param_names = names(params)
        for (i in 1:length(params)) {
          name = param_names[[i]]
          self[[name]] = params[[i]]
          # :TODO: Also update initial values XXX0.
        }
        # Check if FG composition needs to be updated
        if (any(param_names %in% c("w_FGA", "w_FGB", "w_FGC", "w_FGD"))) {
          self$update_functional_group()
        }

        # Check if initial conditions need updating
        if (any(param_names %in% self$initial_condition_names)) { 
          private$update_initial_conditions()
        }
      },

      #' @description Update functional group parameters
      #'
      #' Should be run whenever the functional group composition is changed in 
      #' order to reflect the changes in the parameter list `self$P`.
      update_functional_group = function() {
        p = list()
        for (fg_weight in c("w_FGA", "w_FGB", "w_FGC", "w_FGD")) {
          p[[fg_weight]] = self[[fg_weight]]
        }
        new_FG = build_functional_group(p)
        self$functional_group = new_FG

        # Update to the new values
        for (name in self$fg_parameter_names) {
          self[[name]] = self$functional_group[[name]]
        }
      },

      #' @description 
      #' Parameter Sanity Check
      #' Ensure that the supplied *params* are valid ModVege parameters and, 
      #' if requested, check that all required parameters are present.
      #' Issues a warning for any invalid parameters and throws an error if 
      #' completeness is not satisfied (only when `check_for_completeness = 
      #' TRUE`).
      #' 
      #' @param param_names A list of parameter names to be checked.
      #' @param check_for_completeness Boolean Toggle whether only the 
      #'   validity of supplied *param_names* is checked or whether we 
      #'   want to check that all required parameters to be present 
      #'   (default). In the latter case, if any required parameter is 
      #'   missing, an error is thrown.
      #' @return not_known The list of unrecognized parameter names.
      #'
      #' @md
      check_parameters = function(param_names, check_for_completeness = TRUE) {
        # Check for duplicate param names
        if (length(param_names) != length(unique(param_names))) {
          logger("Non-unique parameter names in supplied parameters.",
                 level = ERROR)
        }
        param_file = self$param_file
        if (check_for_completeness) {
          # Give error if an argument is missing.
          not_present = setdiff(self$required_parameter_names, param_names)
          if (length(not_present) != 0 ) {
            # Construct the error message.
            missing_args = paste(not_present, collapse = "\n")
            msg = paste("The following parameters were missing",
                        "from the supplied input file (%s):\n%s")
            logger(sprintf(msg, param_file, missing_args), level = ERROR)
          }
        }

        not_known = setdiff(param_names, self$parameter_names)
        # Warn if some arguments were not recognized.
        if (length(not_known) != 0) {
          unknown_args = paste(not_known, collapse = "\n")
          msg = paste("The following unrecognized parameters were present",
                      "in the supplied input file (%s):\n%s")
          logger(sprintf(msg, param_file, unknown_args), level = WARNING)
        }
        return(not_known)
      }
    )
  ),

  private = list(
    ## Set initial conditions
    update_initial_conditions = function() {
      for (name in initial_condition_names) {
        old_name = name
        new_name = paste0(old_name, "0")
        self[[new_name]] = self[[old_name]]
      }

      # Set some more initial values 
      self[["WR0"]] = self[["WHC"]]
      self[["OMDGV0"]] = self[["maxOMDGV"]]
      self[["OMDGR0"]] = self[["maxOMDGR"]]
    }
  )
)

