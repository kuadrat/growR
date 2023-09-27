#' Initialize the desired directory structure for working with rmodvege.
#'
#' @param root Path to directory in which to initialize.
#' @param type Choose how things are to be initialized:
#'   \itemize{
#'      \item "empty": Just put empty folders in place.
#'      \item "example": Fill the created folders with example data.
#'   }
#'
#' @export
setup_directory <- function(root = ".", type = "empty") {
  full_path <- path.expand(root)
  # Check if directory is empty
  contents <- list.files(full_path)
  empty = length(contents) == 0
  if (!empty) {
    warning(sprintf("Directory `%s` is not empty.", full_path))
  }

  dirs_to_create <- c("input", "output", "data")
  for (d in dirs_to_create) {
    path = file.path(full_path, d)
    if (dir.exists(path)) {
      warning(sprintf("Directory `%s` already exists. Skipping...", path))
      next
    }
    dir.create(path)
  }
  print(sprintf("Initialized directory structure in `%s`.", full_path))
}

