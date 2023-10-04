#' Initialize the desired directory structure for working with rmodvege.
#'
#' @param root Path to directory in which to initialize.
#' @param include_examples If `TRUE` (default), include example data and 
#'   input parameters in the appropriate directories.
#'
#' @seealso [examples]
#'
#' @export
#'
setup_directory <- function(root = ".", include_examples = TRUE) {
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

  # Only continue if examples are to be provided.
  if (!include_examples) {
    return(0)
  }

  origin = system.file("extdata", package = "rmodvege")
  origins = c()
  destinations = c()

  # First, all files that go to `input/`
  sites = c("posieux", "sorens")
  input_types = c("weather.txt", 
                  "parameters.csv", 
                  "management1.txt", 
                  "management2.txt")
  input_files = unlist(lapply(sites, 
                              function(site) {
                                paste0(site, "_", input_types)
                              }))
  destination = file.path(full_path, "input/")
  for (input_file in input_files) {
    original = file.path(origin, input_file)
    origins = c(origins, original)
    destinations = c(destinations, destination)
  }
  # Then all data files
  subsites = c("1", "2")
  destination = file.path(full_path, "data/")
  for (site in sites) {
    for (subsite in subsites) {
      data_filename = paste0(site, subsite, ".csv")
      original = file.path(origin, data_filename)
      origins = c(origins, original)
      destinations = c(destinations, destination)
    }
  }
  # Finally, an example config file
  origins = c(origins, file.path(origin, "example_config.txt"))
  destinations = c(destinations, file.path(full_path))
  for (i in 1:length(origins)) {
    original = origins[i]
    destination = destinations[i]
    print(sprintf("Copying `%s` to `%s`.", original, destination))
    file.copy(original, destination)
  }
  print(sprintf("Copied example files to respective directories."))
}

