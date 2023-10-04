#' rmodvege: R implementation of the grass growth model ModVege.
#'
#' @import R6
#' @import utils
#' @importFrom Rdpack reprompt
#'
#' @keywords internal
"_PACKAGE"

globalVariables(c("yield_parameters",
                  "management_parameters"))

.onAttach = function(...) {
  rmodvege_version = packageVersion("rmodvege")
  message = sprintf("| Welcome to RModVege Version %s! |", rmodvege_version)
  line = paste0("+", paste(rep("-", nchar(message) - 2), collapse = ""), "+")
  packageStartupMessage(paste0(line, "\n", message, "\n", line, "\n"))
}
