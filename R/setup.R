#' Initialize rmodvege directory structure
#'
#' @description
#' Creates directories in which rmodvege by default looks for or deposits 
#' certain files. Also, optionally populates these directories with example 
#' files, which are useful to familiarize oneself with the rmodvege 
#' simulation framework.
#'
#' @param root Path to directory in which to initialize.
#' @param include_examples If `TRUE` (default), include example data and 
#'   input parameters in the appropriate directories.
#' @param force boolean If `TRUE`, the user will not be asked for permission 
#'   before we write to the filesystem.
#'
#' @examples
#' # Prepare a temporary directory to write to
#' tmp = file.path(tempdir(), "test-setup_directory")
#' dir.create(tmp)
#'
#' # We need `force = TRUE` here in order to make the example work in 
#' # non-interactive settings.
#' setup_directory(root = tmp, include_examples = FALSE, force = TRUE)
#' 
#' # The `input`, `output` and `data` directories are now present.
#' list.files(tmp)
#'
#' # Warnings are issued if directories are already present. Example files 
#' # are still copied and potentially overwritten.
#' setup_directory(root = tmp, include_examples = TRUE, force = TRUE)
#'
#' # Example files are now present
#' list.files(tmp, recursive = TRUE)
#'
#' # End of the example. The following code is for cleaning up.
#' unlink(tmp, recursive = TRUE)
#'
#' @md
#' @export
#'
setup_directory = function(root = ".", include_examples = TRUE, force = FALSE) {
  full_path = path.expand(root)
  # Ask user for permission to write to the file system.
  if (!force) {
    message = "This function is going to create directories and files in `%s`. 
Continue? [y/N]" 
    response = prompt_user(sprintf(message, full_path))
    if (response != "y") {
      cat("Quitting.")
      return()
    }
  }
  # Check if directory is empty
  contents = list.files(full_path)
  empty = length(contents) == 0
  if (!empty) {
    warning(sprintf("Directory `%s` is not empty.", full_path))
  }

  input_dir = getOption("rmodvege.input_dir", default = "input/")
  output_dir = getOption("rmodvege.output_dir", default = "output/")
  data_dir = getOption("rmodvege.data_dir", default = "data/")
  dirs_to_create = c(input_dir, output_dir, data_dir)
  for (d in dirs_to_create) {
    path = file.path(full_path, d)
    if (dir.exists(path)) {
      warning(sprintf("Directory `%s` already exists.", path))
      next
    }
    dir.create(path)
  }
  print(sprintf("Initialized directory structure in `%s`.", full_path))

  # Only continue if examples are to be provided.
  if (!include_examples) {
    return(0)
  }

  extdata = system.file("extdata", package = "rmodvege")
  scripts = system.file("scripts", package = "rmodvege")
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
  destination = file.path(full_path, input_dir)
  for (input_file in input_files) {
    original = file.path(extdata, input_file)
    origins = c(origins, original)
    destinations = c(destinations, destination)
  }
  # Then all data files
  subsites = c("1", "2")
  destination = file.path(full_path, data_dir)
  for (site in sites) {
    for (subsite in subsites) {
      data_filename = paste0(site, subsite, ".csv")
      original = file.path(extdata, data_filename)
      origins = c(origins, original)
      destinations = c(destinations, destination)
    }
  }
  # Finally, files that should go to the rmodvege working dir
  filenames = c(file.path(extdata, "example_config.txt"),
                file.path(scripts, "compare.R"))
  destination = file.path(full_path)
  for (filename in filenames) {
    origins = c(origins, filename)
    destinations = c(destinations, destination)
  }
  # Now that origins and destinations are defined, carry out the copying.
  for (i in 1:length(origins)) {
    original = origins[i]
    destination = destinations[i]
    print(sprintf("Copying `%s` to `%s`.", original, destination))
    file.copy(original, destination)
  }
  print(sprintf("Copied example files to respective directories."))
}

## Used when testing
.tmpdir = function() {
  file.path(tempdir(), "test-directory")
}
