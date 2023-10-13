#-metrics.R---------------------------------------------------------------------
# Statistical tools and measures for evaluating model performance.
#-------------------------------------------------------------------------------

## Helper function to return a normalization factor if *relative* is TRUE.
##
get_norm = function(x, relative) {
  if (relative) {
    return(mean(x))
  } else {
    return(1)
  }
}

get_bias_core = function(predicted, observed, relative = TRUE) {
  r = get_norm(observed, relative)
  return(mean(predicted - observed)/r)
}

mean_absolute_error_core = function(predicted, observed, relative = TRUE) {
  r = get_norm(observed, relative)
  return(mean(abs(predicted - observed))/r)
}

root_mean_squared_core = function(predicted, observed, relative = TRUE) {
  r = get_norm(observed, relative)
  return(sqrt(sum((predicted - observed)^2) / length(predicted))/r)
}

willmott_core = function(predicted, observed, c = 2) {
  numerator = sum(abs(predicted - observed))
  o_mean = mean(observed)
  denominator = c * sum(abs(observed - o_mean))
  if (numerator <= denominator) {
    return(1 - numerator/denominator)
  } else {
    return(denominator/numerator - 1)
  }
}

## Since many of the statistical measure function require the same kind of 
## preprocessing of data (i.e. removal of NAs), generate them using this 
## function factory.
##
metric_factory = function(core) {
  wrapped_function = function(predicted, observed, ...) {
    # Skip NA entries
    w = !is.na(observed)
    p = predicted[w]
    o = observed[w]
    return(core(p, o, ...))
  }
  return(wrapped_function)
}

#' Metric Functions
#'
#' @description Functions to calculate different performance metrics.
#'
#' In the case of *get_bias*: Calculate the bias *b*, i.e. the average 
#' difference between predicted *y* and observed *z* values: 
#' ```
#'   bias = mean(y - z)
#' ```
#' @note NA values are completely ignored.
#'
#' @param predicted Vector containing the predictions *y*.
#' @param observed Vector containing the observations *z*.
#' @param ... **relative** Boolean. If true give the result as a ratio to the 
#'   average observation `mean(ovserved)`.
#'
#' @return m A number representing the relative or absolute value for the 
#'   metric.
#'
#' @examples
#' predicted = c(21.5, 22.2, 19.1)
#' observed = c(20, 20, 20)
#' get_bias(predicted, observed)
#' get_bias(predicted, observed, relative = FALSE)
#'
#' root_mean_squared(predicted, observed)
#' root_mean_squared(predicted, observed, relative = FALSE)
#'
#' mean_absolute_error(predicted, observed)
#' mean_absolute_error(predicted, observed, relative = FALSE)
#'
#' @seealso [willmott()]
#'
#' @md
#' @export
get_bias = metric_factory(get_bias_core)

#' Root Mean Squared Error
#'
#' @describeIn get_bias
#' Calculate the square root of the average squared difference between 
#' prediction and observation:
#' ```
#' RMSE = sqrt(sum(predicted - observed)^2) / length(predicted)
#' ```
#'
#' @md
#' @export
root_mean_squared = metric_factory(root_mean_squared_core)

#' Mean Absolute Error
#'
#' @describeIn get_bias
#' Calculate the average of the absolute differences between
#' prediction and observation:
#' ```
#' MAE = mean(abs(predicted - observed))
#' ```
#'
#' @md
#' @export
mean_absolute_error = metric_factory(mean_absolute_error_core)

#' Willmott Index
#'
#' Willmott's index of model performance as described in
#' Willmott (2012).
#'
#' This index takes on values from -1 to 1, where values closer to 1 are 
#' generally indicating better model performance. Values close to -1 can 
#' either mean that the model predictions differ strongly from the 
#' observation, or that the observations show small variance (or both).
#'
#' @param predicted Vector containing the predictions *y*.
#' @param observed Vector containing the observations *z*.
#' @param ... Scaling factor **c** in the denominator in the Willmott index. 
#'   The originally proposed value of 2 should be fine.
#'
#' @return willmott Value between -1 and 1
#'
#' @examples
#' predicted = c(21.5, 22.2, 19.1)
#' observed = c(20, 20, 20)
#' # The Willmott index "fails" in this case, as the variance in the 
#' # observation is 0.
#' willmott(predicted, observed)
#'
#' # Try with more realistic observations
#' observed = c(20.5, 19.5, 20.0)
#' willmott(predicted, observed)
#'
#' @seealso [get_bias()]
#'
#' @references
#'   \insertRef{willmott2012RefinedIndexModel}{growR}
#'
#' @md
#' @export
willmott = metric_factory(willmott_core)

#' List of Performance Metrics
#'
#' This list provides some common metrics of model performance along with 
#' their "best value".
#'
#' @format A list where each item is a sublist containing the keys *func* and 
#'   *target*.
#'   \describe{
#'     \item{func}{The function used to calculate given metric.}
#'     \item{target}{The value that would be reached in the case of optimal 
#'       performance.}
#'     \item{limits}{Reasonable limits to be used when plotting.}
#'   }
#'
#' @md
#' @export
metric_map = list(
                  bias = list(func = get_bias, target = 0, limits = c(-1, 1)),
                  RMSE = list(func = root_mean_squared, target = 0, 
                              limits = c(0, 1)),
                  MAE = list(func = mean_absolute_error, target = 0, 
                             limits = c(0, 1)),
                  WIMP = list(func = willmott, target = 1, limits = c(-1, 1))
                  )

