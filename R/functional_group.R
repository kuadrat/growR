#' Representation of a grassland plant population 
#'
#' @description
#' A functional group is a representation of a grassland plant population 
#' with certain functional attributes.
#'
#' It contains many plant parameters that are collected under the hood of
#' functional groups. The class implements S3 style operator overloading such 
#' that one can do things like 
#' ```
#' mixed_functional_group = 0.8 * FG_A + 0.2 * FG_B
#' ```
#' 
#' @details
#' # Public fields
#' ```{r child = "man/parameters_fg.Rmd"}
#' ```
#'
#' Default values for parameters are taken from functional group A in
#' Jouven et al.
#'
#' @field fg_parameter_names Names of the vegetation parameters governed by 
#'   functional group composition.
#' 
#' @references{
#'  \insertRef{jouven2006ModelPredictingDynamics}{rmodvege}
#' }
#' 
#' @md
#' @export
FunctionalGroup = R6Class(
  "FunctionalGroup",
  public = list(
    #-Public-fileds-------------------------------------------------------------
    SLA = 0.033,
    pcLAM = 0.68,
    ST1 = 600,
    ST2 = 1200,
    maxSEA = 1.2,
    minSEA = 0.8,
    LLS = 500,
    maxOMDGV = 0.9,
    minOMDGV = 0.75,
    maxOMDGR = 0.9,
    minOMDGR = 0.65,
    BDGV = 850,
    BDDV = 500,
    BDGR = 300,
    BDDR = 150,
    fg_parameter_names = c("SLA", "pcLAM", "ST1", "ST2", "maxSEA", "minSEA", 
                           "LLS", "maxOMDGV", "minOMDGV", "maxOMDGR", 
                           "minOMDGR", "BDGV", "BDDV", "BDGR", "BDDR"),

    #-Public-methods------------------------------------------------------------

    #' @description
    #' Constructor
    #'
    #' @param ... Key-value pairs of parameters to be set.
    #'
    initialize = function(...) {
      self$set_parameters(...)
    },

    #' @description
    #' Convenient getter
    #' 
    #' Returns all parameters with their names in a list.
    #'
    get_parameters = function() {
      P = list()
      for (name in self$fg_parameter_names) {
        P[[name]] = self[[name]]
      }
      return(P)
    },

    #' @description
    #' Ordered getter
    #' 
    #' Returns all parameters in reproducible order in a vector.
    #'
    get_parameters_ordered = function() {
      P = c()
      for (name in self$fg_parameter_names) {
        P = c(P, self[[name]])
      }
      return(P)
    },

    #' @description
    #' Convenient setter
    #' 
    #' Set all specified parameters.
    #' @param ... Key-value pairs of parameters to be set.
    #'
    set_parameters = function(...) {
      args = list(...)
      arg_names = names(args)
      for (arg_name in arg_names) {
        if (arg_name %in% self$fg_parameter_names) {
          self[[arg_name]] = args[[arg_name]]
        } else {
          warning(sprintf("[FunctionalGroup]Parameter not recognized: %s",
                          arg_name))
        }
      }
    },

    #' @description
    #' Efficient setter, assumes parameters come in known order.
    #'
    #' @param ordered_parameter_values Parameter values to be set. Need to be 
    #'   in the same order as `FunctionalGroup$fg_parameter_names`.
    #'
    set_parameters_ordered = function(ordered_parameter_values) {
      names = self$fg_parameter_names
      for (i in 1:length(names)) {
        self[[names[i]]] = ordered_parameter_values[i]
      }
    }
  )  
)

#' Addition of two functional groups
#'
#' Addition occurs by adding all FG parameters separately.
#' 
#' @param A First functional group.
#' @param B Second functional group.
#' @export
#'
`+.FunctionalGroup` = function(A, B) {
  parameters_A = A$get_parameters_ordered()
  parameters_B = B$get_parameters_ordered()
  new_parameters = parameters_A + parameters_B
  C = FunctionalGroup$new()
  C$set_parameters_ordered(new_parameters)
  return(C)
}

#' Scalar multiplication of functional group (order matters: scalar * FG)
#'
#' @param scalar float to multiply all FG parameters with.
#' @param fg Functional group whose parameters are to be multiplied.
#' @export
`*.FunctionalGroup` = function(scalar, fg) {
  parameters = fg$get_parameters_ordered()
  C = FunctionalGroup$new()
  C$set_parameters_ordered(scalar * parameters)
  return(C)
}

# Define the four functional groups

#' Functional group A
#'
#' @seealso [FunctionalGroup]
#'
#' @export
FG_A = FunctionalGroup$new()

#' Functional group B
#'
#' @seealso [FunctionalGroup]
#'
#' @export
FG_B = FunctionalGroup$new(
  SLA = 0.025,
  ST1 = 700,
  ST2 = 1350,
  maxSEA = 1.3,
  minSEA = 0.7,
  LLS = 800,
  maxOMDGV = 0.9,
  minOMDGV = 0.6,
  maxOMDGR = 0.9,
  minOMDGR = 0.45,
  BDGV = 850,
  BDDV = 500,
  BDGR = 300,
  BDDR = 150
)

#' Functional group C
#'
#' @seealso [FunctionalGroup]
#'
#' @export
FG_C = FunctionalGroup$new(
  SLA = 0.022,
  ST1 = 850,
  ST2 = 1550,
  maxSEA = 1.4,
  minSEA = 0.6,
  LLS = 900,
  maxOMDGV = 0.85,
  minOMDGV = 0.65,
  maxOMDGR = 0.85,
  minOMDGR = 0.45,
  BDGV = 1200,
  BDDV = 1800,
  BDGR = 200,
  BDDR = 300
)

#' Functional group D
#'
#' @seealso [FunctionalGroup]
#'
#' @export
FG_D = FunctionalGroup$new(
  SLA = 0.019,
  ST1 = 1000,
  ST2 = 1850,
  maxSEA = 1.5,
  minSEA = 0.5,
  LLS = 1400,
  maxOMDGV = 0.75,
  minOMDGV = 0.65,
  maxOMDGR = 0.75,
  minOMDGR = 0.45,
  BDGV = 800,
  BDDV = 2200,
  BDGR = 150,
  BDDR = 450
)

#' Build the effective functional group as a weighted linear combination.
#'
#' Uses the weights found in :param:`P` to construct the effective functional 
#' groups and updates functional group parameters in P.
#'
#' @param P list; name-value pairs of parameters. Has to contain w_FGX with X 
#'   in (A, B, C, D).
#' @return A modified list, where all functional group parameters are updated 
#'   to the values of the effective functional group according to the weights 
#'   w_FGX.
#'
#' @seealso FunctionalGroup
#'
#' @export
#'
build_functional_group = function(P) {
  w_A = P[["w_FGA"]]
  w_B = P[["w_FGB"]]
  w_C = P[["w_FGC"]]
  w_D = P[["w_FGD"]]
  w_tot = w_A + w_B + w_C + w_D

  FG_eff = w_A/w_tot * FG_A +
           w_B/w_tot * FG_B +
           w_C/w_tot * FG_C +
           w_D/w_tot * FG_D

  # Update to the new values in P
  P = modifyList(P, FG_eff$get_parameters())
  return(P)
}

