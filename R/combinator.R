#' Combinator
#'
#' @description
#' Helps to find all possible combinations for a given set of values.
#'
#' @seealso [create_combinations()]
#' @md
Combinator = R6Class(
  "Combinator",
  public = list(

    #' @field combinations list Once run, holds all valid parameter combinations 
    #'   as named lists.
    combinations = list(),
    #' @field eps float Numerical precision to require when checking the 
    #'   functional group weight sum criterion.
    eps = 2e-2,

    #' @description
    #' Find possible combinations
    #'
    #' @param param_values A list giving all options for the parameter values 
    #'   which are to be combined. As an example: 
    #'   ```
    #'   list(w_FGA = c(0, 0.5, 1), w_FGB = c(0, 0.5, 1), NI = c(0.5, 0.9))
    #'   ```
    #'   This would generate the combinations
    #'   | w_FGA | w_FGB | NI  |
    #'   | ----- | ----- | --- |
    #'   |     0 |     1 | 0.5 |
    #'   |     0 |     1 | 0.9 |
    #'   |   0.5 |   0.5 | 0.5 |
    #'   |   0.5 |   0.5 | 0.9 |
    #'   |     1 |     0 | 0.5 |
    #'   |     1 |     0 | 0.9 |
    #'
    #' @param eps Precision to be used when checking if the sum citerion of the 
    #'   functional groups (w_FGA + w_FGB + w_FGC + w_FGD = 1) is fulfilled.
    #'
    #' @return combinations A list containing vectors of parameter value 
    #'   combinations.
    #'
    #' @md
    create_combinations = function(param_values) {
      # Take note of functional group values
      private$param_names = names(param_values)
      private$fg_mask = private$param_names %in% c("w_FGA", 
                                                   "w_FGB", 
                                                   "w_FGC", 
                                                   "w_FGD")
      private$recurse(param_values, c())
      return(self$combinations)
    }
  ),
  private = list(

    fg_mask = NULL,
    param_names = NULL,
    recurse = function(values, loop_indices = c()) {
      if (length(values) > 1) {
        # Start a loop and go one recursion level deeper.
        for (value in values[[1]]) {
          private$recurse(values[-1], c(loop_indices, value))
        }
      } else {
        # In the deepest recursion level, collect a combination, if it  
        # fulfills the functional group sum condition.
        for (value in values[[1]]) {
          # Recombine to get a vector where fg_mask applies correctly.
          all_values = c(loop_indices, value)
          if (abs(sum(all_values[private$fg_mask]) - 1) > self$eps) {
            # Sum rule not satisfied - continue to next combination.
            next
          } else {
            names(all_values) = private$param_names
            self$combinations[[length(self$combinations) + 1]] = all_values
          }
        }
      }
    }
  )
)

#' Create Valid Combinations
#'
#' @description
#' Generate a list which contains all possible combinations of the provided 
#' parameter values. This excludes combinations that are invalid because the 
#' sum criterion for functional groups `w_FGA + w_FGB + w_FGC + w_FGD = 1` is 
#' not fulfilled.
#'
#' @details
#' Assume for example the following list as argument *param_values*: 
#' ```
#' list(w_FGA = c(0, 0.5, 1), w_FGB = c(0, 0.5, 1), NI = c(0.5, 0.9))
#' ```
#'
#' This would generate the combinations
#'
#' | w_FGA | w_FGB | NI  |
#' | ----- | ----- | --- |
#' |     0 |     1 | 0.5 |
#' |     0 |     1 | 0.9 |
#' |   0.5 |   0.5 | 0.5 |
#' |   0.5 |   0.5 | 0.9 |
#' |     1 |     0 | 0.5 |
#' |     1 |     0 | 0.9 |
#'
#' @param param_values A list giving all options for the parameter values 
#'   which are to be combined. The format is `list[[param_name]] = 
#'   param_values` where `param_values` is a vector with the values for the 
#'   respective parameter. The parameter names for functional group weights 
#'   (`w_FGX` with `X` in (A, B, C, D)) receive special treatment and 
#'   therefore need to be spelled correctly.
#' @param eps Float specifying the precision to which the sum criterion for 
#'   functional group has to be satisfied. The criterion is considered 
#'   satisfied, if ```
#'   abs(w_FGA + w_FGB + w_FGC + w_FGD) - 1) <= eps
#'   ```
#' 
#' @return combinations An unnamed list where every entry is a list 
#'   containing the parameter values (named as in the input *param_values*) 
#'   for a valid combination.
#'
#' @examples
#' # Define the parameter steps you want to explore. This is a minimal example.
#' # A more realistic one follows below.
#' param_values = list(w_FGA = c(0, 0.5, 1),
#'                     w_FGB = c(0, 0.5, 1),
#'                     NI = c(0.5, 0.9)
#' )
#' # Create all valid combinations of the defined steps
#' create_combinations(param_values)
#'
#' # More realistic example for an initial exploration of parameter space, 
#' # where we suspect that functional groups A and B should be more prevalent 
#' # than C and D. This produces 54 parameter combinations, which is a number 
#' # of model evaluations that can run within a reasonable timeframe 
#' # (depending on your system).
#' param_values = list(w_FGA = seq(0, 1, 0.33),
#'                     w_FGB = seq(0, 1, 0.33),
#'                     w_FGC = seq(0, 0.7, 0.33),
#'                     w_FGD = seq(0, 0.7, 0.33),
#'                     NI = seq(0.5, 1.0, 0.25)
#' )
#' length(create_combinations(param_values))
#'
#' # The default value for *eps* made sure that combinations of 0.33 + 0.66 = 
#' # 0.99 etc. are considered "valid". If we make *eps* too small, no valid 
#' # combinations can be found:
#' length(create_combinations(param_values, eps = 1e-3))
#'
#' @md
#' @export
create_combinations = function(param_values, eps = 2e-2) {
  C = Combinator$new()
  C$eps = eps
  C$create_combinations(param_values)
  return(C$combinations)
}

