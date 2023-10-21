#' Add data to a ggplot
#'
#' @description
#' Add a lineplot of the *x_key* and *y_key* columns in *data* to the 
#' supplied ggplot object *ax*. If none is supplied, a new one is created.
#'
#' @param data data.frame or similar object interpretable by ggplot.
#' @param ax list as returned by ggplot() and related functions.
#' @param x_key,y_key Column names in *data* to be plotted.
#' @param style XXX in ggplot geom_XXX to use.
#' @param label Codename for this line to be used in legend creation. If 
#'   NULL, use *y_key*.
#' @param ... All further arguments are passed to the selected ggplot geom.
#'
#' @return ax A ggplot list (like the input *ax*).
#'
#' @examples
#' library(ggplot2)
#' # Add first set of data
#' ax = add_lines(mtcars, x_key = "wt", y_key = "mpg", label = "First Line")
#' 
#' # Add one more line to the plot
#' ax = add_lines(mtcars, ax = ax, x_key = "wt", y_key = "qsec", 
#' label = "Second Line")
#'
#' print(ax)
#' @export
add_lines = function(data, 
                     ax = NULL, 
                     y_key = "dBM_smooth", 
                     x_key = "DOY", 
                     style = "line",
                     label = NULL,
                     ...) {
  check_for_package("ggplot2")

  geom_functions = list(line = ggplot2::geom_line,
                        step = function(...) {
                          ggplot2::geom_step(..., direction = 'vh')},
                        point = ggplot2::geom_point
                       )
  geom_styles = names(geom_functions)

  # Leave if input data is empty
  if (0 %in% dim(data)) { 
    return(ax)
  }

  if (is.null(label)) {
    label = y_key
  }

  # Create a container for ggplot
  df = data.frame(x = data[[x_key]],
                  y = data[[y_key]], 
                  color = label)

  if (is.null(ax)) {
    ax = ggplot2::ggplot()
  }
  # Select the geom_XXX function
  if (style %in% geom_styles) {
    geom_function = geom_functions[[style]]
  } else {
    # Use default style
    geom_function = geom_functions[["line"]]
  }

  if (is.null(label)) {
    label = y_key
  }
  ax = ax + geom_function(data = df, 
                          mapping = ggplot2::aes_string(x = "x", 
                                                        y = "y", 
                                                        color = "color"),
                          ...
                         )
  return(ax)
}

#' Extract the name of a site from a filename
#'
#' This function assumes the filenames to begin with the site name, 
#' potentially followed by an underscore and further characters.
#'
#' @param filename String of a `ModvegeSite` output filename.
#'
get_site_name = function(filename) {
  # Strip off path in front
  name = basename(filename)
  # In case no underscores are present, cut off the filename suffix
  without_suffix = tools::file_path_sans_ext(name)
  return(strsplit(without_suffix, "_")[[1]][1])
}

#' Load experimental data
#'
#' @description
#' Load all datasets stored in the supplied files.
#'
#' Upon loading, the cumulative biomass growth *cBM* is automatically 
#' calculated from the given daily biomass growth *dBM* values.
#'
#' @section Data file format:
#' The input data files are expected to consist of four columns containing 
#' the following fileds, in order: \describe{
#' \item{date}{Date of measurement in yyyy-mm-dd format.}
#' \item{year}{Year of measurement. Identical to yyyy in *date* field.}
#' \item{DOY}{Day of year of the measurement. Jan 1st corresponds to 1, Dec 
#'   31st corresponds to 365 (except in gap years).}
#' \item{dBM}{Observed **average daily biomass growth** since last cut in 
#'   kg/ha.}
#' }
#' The first row is expected to be a header row containing the exact field 
#' names as in the description above.
#' Columns may be separated by an arbitrary character, specified by the *sep* 
#' argument.
#' The example data uses a comma (",").
#'
#' @param filenames Vector of paths to datafiles to be loaded.
#' @param sep String Field separator used in the datafiles.
#' @return measured_data list of data.frame each corresponding to one of 
#'  the sites detected in *filenames*. Each data.frame contains the keys
#'  - `dBM`: average daily biomass growth since last observation in kg/ha.
#'  - `cBM`: cumulative biomass growth up to this DOY in kg/ha.
#'  - `year`: year of observation.
#'  - `DOY`: day of year of observation.
#' 
#' @export
#' @md
load_measured_data = function(filenames, sep = ",") {
  # Load relevant datasets
  measured_data = list()
  for (filename in filenames) {
    dt = read.table(file.path(filename), header = TRUE, sep = sep)
    # Ditch all NA rows.
    dt = dt[!is.na(dt$dBM), ]
    # Calculate cumulative harvested biomass
    # Need to multiply the daily biomass growth with number of days 
    # passed since last harvest.
    n_harvests = length(dt$DOY)
    days_since_last_harvest = dt$DOY[2:n_harvests] - dt$DOY[1:(n_harvests-1)]
    # Assume a reasonable growth time for the first harvest
    days_before_first_harvest = 28
    days_since_last_harvest = c(days_before_first_harvest, days_since_last_harvest)
    days_since_last_harvest[days_since_last_harvest < 0] = days_before_first_harvest
    dt[["cBM"]] = dt[["dBM"]] * days_since_last_harvest
    last_year = dt[["year"]][1]
    for (i in 2:n_harvests) {
      current_year = dt[["year"]][i]
      if (current_year == last_year) {
        # As long as we're still in the same year, sum up BM.
        dt[["cBM"]][i] = dt[["cBM"]][i] + dt[["cBM"]][i-1]
      } else {
        # If year has changed, move on.
        last_year = current_year
      }
    }
    site = get_site_name(filename)
    measured_data[[site]] = dt
  }
  return(measured_data)
}

#' Load measured data based on site names
#'
#' @describeIn load_measured_data
#' Data filenames are generated on the convention `SITE.csv` and are searched 
#' for in the subdirectory `getOption("growR.data_dir")`, which defaults 
#' to `data/`.
#'
#' @param sites Vector of site names for which data to load.
#'
#' @export
load_data_for_sites = function(sites) {
  data_dir = getOption("growR.data_dir", default = "data")
  filenames = sapply(sites, function(site) {
                              file.path(data_dir, sprintf("%s.csv", site))
                            }
  )
  return(load_measured_data(filenames))
}

#' Load data matching supplied filenames
#'
#' @describeIn load_measured_data
#' Accepts a vector of output filenames as generated by 
#' [ModvegeSite]`$write_output()` out of which the site names are inferred.
#'
#' @param filenames Vector of strings representing simulation output 
#'   filenames for which matching data files are searched and loaded.
#'
#' @details
#' `load_matching_data()` internally uses [get_site_name()] and makes the same 
#' assumptions about the output filename formats. It further assumes measured 
#' data to be located in "data/" and adhere to the filename format `x.csv`
#' with `x` being the site name.
#'
#' @export
#' @md
load_matching_data = function(filenames) {
  # Detect sites
  sites = c()
  for (filename in filenames) {
    site = get_site_name(filename)
    if (!site %in% sites) {
      sites = c(sites, site)
    }
  }
  return(load_measured_data(sites))
}

