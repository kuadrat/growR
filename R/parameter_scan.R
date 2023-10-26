## Prompt user for confirmation to proceed
ask_for_confirmation = function(n_combinations, force) {
  if (force) {
    return()
  }
  if (!interactive()) { # nocov start
    logger("Quitting run_parameter_scan() in non-interactive context with 
`force == FALSE`.", level = INFO)
    rlang::interrupt()
  }
  print(sprintf("Number of parameter combinations: %s", n_combinations))
  response = prompt_user("Continue? [Y/n] ")
  if (length(response) > 0 && tolower(response) %in% c("x", "n", "c", "q")) {
    logger("Quitting as requested by user.", level = INFO)
    rlang::interrupt()
  } # nocov end
}

#' Parameter Scan
#'
#' Run ModVege for a different sets of parameters.
#'
#' @note
#' Special care has to be taken in the creation of the *param_values* argument.
#' It's possible to choose values that do not allow for any valid combination.
#' Confer [create_combinations()].
#'
#' @param environment Either a [ModvegeEnvironment] instance with all the site, 
#'   management and weather inputs expected by [ModvegeSite]`$run()` **or** a 
#'   string representing the name of a config file to read in order to 
#'   generate the [ModvegeEnvironment] with [read_config()]. Note that, in 
#'   the latter case, only the first found configuration is used if there are 
#'   more than one valid uncommented lines in the config file.
#' @param param_values A named list where each key stands for a ModVege 
#'   parameter, i.e. a member of [ModvegeParameters]`$parameter_names`. Each 
#'   list entry then has to be a vector containing the allowed values for the 
#'   respective parameter. All possible allowed combinations of these 
#'   parameter values are then generated and fed into a ModVege run.
#' @param force Boolean. By default (`force = FALSE`), the function first 
#'   counts the number of parameter combinations that need to be run and asks 
#'   the user, if it should proceed. This can be suppressed by letting `force 
#'   = TRUE`.
#' @param outfilename String. If nonempty, the results are stored as an `rds` 
#'   file with filename *outfilename* using the [saveRDS()] function.
#'
#' @return results A list containing an entry for each supplied parameter set 
#'   in *param_values*. Each entry is itself a list containing the following 
#'   keys:
#'   \describe{
#'   \item{params}{The parameter set that was used to run ModVege for this 
#'   entry.}
#'   \item{data}{A list containing for each simulated year a [ModvegeSite] 
#'   object which was run for the respective year and therefore carries the 
#'   respective results.}
#'   }
#'
#' @seealso [ModvegeParameters], [saveRDS()], [create_combinations()]
#'
#' @examples
#' env = create_example_environment()
#' # We're creating a trivial list of parameters to explore here in order to 
#' # prevent the example from requiring a long time to execute. See 
#' # [create_combinations()] for more realistic uses of param_values.
#' param_values = list(w_FGA = c(0, 1), w_FGB = c(0, 1))
#' run_parameter_scan(env, param_values, force = TRUE)
#' 
#' @md
#' @export
run_parameter_scan = function(environment, param_values, force = FALSE, 
                              outfilename = "") {
  start_time = Sys.time()
  # Read environment from config file, if *environment* is a string.
  if (is.character(environment)) {
    # Only take first run from whatever config is specified
    environment = read_config(environment)[[1]]
  }

  # Ensure all supplied parameters are valid
  environment$parameters$check_parameters(names(param_values),
                                          check_for_completeness = FALSE)
  # Create all parameter combinations
  parameter_sets = create_combinations(param_values)
  n_combinations = length(parameter_sets)
  ask_for_confirmation(n_combinations, force)

  param_names = names(parameter_sets[[1]])
  results = list()
  for (i in 1:n_combinations) {
    logger(sprintf("Parameter combination %i out of %i.", i, n_combinations),
           level = INFO
    )
    environment$parameters$set_parameters(parameter_sets[[i]])
    mvs = growR_run_loop(c(environment), 
                         output_dir = "",
                         store_results = TRUE
    )
    results[[i]] = list(params = parameter_sets[[i]],
                        data = mvs[[1]])
  }
  logger("Completed parameter scan.", level = INFO)
  logger(paste0("Time used: ", Sys.time() - start_time), level = INFO)
  if (outfilename != "") { # nocov start
    question = "About to write results to `%s`. Continue? [Y/n] "
    response = prompt_user(sprintf(question, outfilename))
    if (!response %in% c("y", "Y", "")) {
      logger(sprintf("Storing results as `%s`.", outfilename))
    }
    saveRDS(results, outfilename)
  } # nocov end
  return(results)
}

#' Analyze results of a parameter scan
#'
#' @param parameter_scan_results String or List. If a string, it is 
#'   interpreted as the name of a `rds` file that contains the results of a 
#'   parameter scan which is then loaded using [readRDS()]. Otherwise, it 
#'   should be the output of [run_parameter_scan()] directly.
#' @param datafile Name or path to a file containing measured data. The model 
#'   results in *parameter_scan_results* are compared to the data therein. If 
#'   empty, the site is inferred from the [ModvegeSite] objects in 
#'   *parameter_scan_results* and a corresponding data file is searched for 
#'    in `getOption("growR.data_dir", default = "data").
#' @param smooth_interval Int. Number of days over which the variable `dBM` 
#'   is smoothened. Should be set to make experimental data and simulated 
#'   data to be as comparable as possible.
#' @return analyzed A list with five keys: `dBM`, `cBM`, `cBM_end`, `metrics` 
#'   and `params`.
#'  \describe{
#'    \item{dBM}{A data.frame with `1 + n_params + n_metrics` columns 
#'      where each row represents a different parameter combination. 
#'      The first column (`n`) gives the row number and is used to identify a 
#'      parameter combination. The subsequent `n_params` columns give the 
#'      values of the parameters used in this combination. The final `n_metrics`
#'      columns give the resulting performance score of the model run with 
#'      these parameters for each metric applied to model variable `dBM`.}
#'    \item{cBM}{A data.frame of same format as for the key *dBM*. The first 
#'      `n_params + 1` columns are identical to the data.frame in *dBM*. The 
#'      difference is that the final `n_metrics` columns give performance 
#'      scores with respect to the model variable `cBM`.}
#'    \item{cBM_end}{A data.frame analogous to *dBM* and *cBM*, only this time 
#'      the last `n_metrics` columns give performance scores with respect to 
#'      the variable `cBM_end`, which is the final value of `cBM`, i.e. the 
#'      cumulative grown biomass at the end of the year.}
#'    \item{params}{A vector containing the names of the scanned parameters. 
#'      These are also the column names of columns `2:(n_params+1)` in 
#'      *results*.}
#'    \item{metrics}{A vector containing the names of the employed 
#'      performance metrics. These are also the column names of the last 
#'      `n_metrics` columns in *results*.}
#'  }
#'
#' @seealso [run_parameter_scan()], [readRDS()]
#'
#' @examples
#' # There needs to be data available with which the model is to be compared.
#' # For this example, use data provided by the package.
#' path = system.file("extdata", package = "growR")
#' datafile = file.path(path, "posieux1.csv")
#'
#' # We also use example parameter scan data provided by the package.
#' # In the real world, you would generally create your own data using 
#' # `run_parameter_scan()`.
#' analyze_parameter_scan(parameter_scan_example, datafile = datafile)
#' 
#' @md
#' @export
analyze_parameter_scan = function(parameter_scan_results, datafile = "", 
                                  smooth_interval = 28) {
  supported_variables = c("dBM", "cBM", "cBM_end")
  metrics_to_use = c("bias", "MAE", "RMSE")

  # Read results from .Rds file, if applicable
  if (is.character(parameter_scan_results)) {
    results = readRDS(parameter_scan_results)
  } else {
    results = parameter_scan_results
  }
  n_combinations = length(results)
  first = results[[1]]$data
  years = sapply(first, function(mv) {mv$year})
  n_years = length(years)
  
  # Load measured data for detected site
  if (datafile == "") {
    site = results[[1]]$data[[1]]$site_name
    measured_data = load_data_for_sites(c(site))[[1]]
  } else {
    measured_data = load_measured_data(datafile)[[1]]
  }
  # Reduce to relevant years
  relevant_data = measured_data[measured_data$year %in% years, ]
  # Construct a selector for the DOYs present in relevant data
  mask = relevant_data$DOY + 365 * (relevant_data$year - relevant_data$year[1])
  # Calculate measured cBM_end
  measured_cBM_ends = c()
  for (year in years) {
    mask2 = measured_data$year == year
    measured_cBM_end = sum(measured_data[mask2, ]$cBM)
    measured_cBM_ends = c(measured_cBM_ends, measured_cBM_end)
  }

  # Assemble metrics. 
  for (combination in 1:n_combinations) {
    modvegesites = results[[combination]]$data
    # Collect year curves into one vector and smoothen dBM
    dBM = c()
    cBM = c()
    cBM_end = c()
    for (i_year in 1:n_years) {
      mv = modvegesites[[i_year]]
      cBM = c(cBM, mv$cBM)
      cBM_end = c(cBM_end, mv$cBM[length(mv$cBM)])
      smoothed = box_smooth(mv$dBM, box_width = smooth_interval)
      dBM = c(dBM, smoothed)
    }
    results[[combination]][["cBM"]] = list()
    results[[combination]][["dBM"]] = list()
    results[[combination]][["cBM_end"]] = list()
    # Employ performance metrics
    for (metric in metrics_to_use) {
      # cBM
      m_cBM = metric_map[[metric]][["func"]](cBM[mask], relevant_data$cBM)
      results[[combination]][["cBM"]][[metric]] = m_cBM
      # dBM
      m_dBM = metric_map[[metric]][["func"]](dBM[mask], relevant_data$dBM)
      results[[combination]][["dBM"]][[metric]] = m_dBM
      # cBM_end
      m_cBM_end = metric_map[[metric]][["func"]](cBM_end, measured_cBM_ends)
      results[[combination]][["cBM_end"]][[metric]] = m_cBM_end
    }
  }

  # Reformat results to a data.frame
  params = names(results[[1]][["params"]])
  return_list = list(metrics = metrics_to_use, params = params)
  for (variable in supported_variables) {
    analyzed = data.frame(n = 1:n_combinations)
    for (i_param in 1:length(params)) {
      param_name = params[[i_param]]
      analyzed[[param_name]] = extract_results(results, "params", i_param)
    }
    for (i_metric in 1:length(metrics_to_use)) {
      metric_name = metrics_to_use[[i_metric]]
      analyzed[[metric_name]] = extract_results(results, variable, i_metric)
    }
    return_list[[variable]] = analyzed
  }
  return(return_list)
}

extract_results = function(results, key, index) {
  sapply(results, function(r) {r[[key]][[index]]})
}

#' Plot Parameter Scan Results
#'
#' @description
#' This class facilitates interactive visual analysis of parameter scan 
#' results.
#'
#' @seealso [plot_parameter_scan()]
#'
#' @md
#' @export
PscanPlotter = R6Class(
  "PscanPlotter",
  public = list(
  #-Public-fields---------------------------------------------------------------
    #' @field analyzed List, as output by [analyze_parameter_scan()].
    analyzed = NULL,
    #' @field params Vector of names of scanned parameters.
    params = NULL,
    #' @field metrics Vector of names of model performance metrics to use.
    metrics = NULL,
    #' @field n_params Number of scanned parameters.
    n_params = NULL,
    #' @field n_metrics Number of performance metrics to apply.
    n_metrics = NULL,
    #' @field res data.frame holding parameter scan results. It contains 
    #'   `n_params + n_metrics + 1` columns: one column for each scanned 
    #'   parameter, one for each employed metric and an additional column 
    #'   (name `n`) to give each parameter combination (i.e. each row) an 
    #'   identifying number.
    res = NULL,
    #' @field n_combinations Number of rowns in `res`.
    n_combinations = NULL,
    #' @field sorted List containing copies of `res` which are each sorted by 
    #'   a different performance metric. List keys are the values in 
    #'   `self$metrics`.
    sorted = NULL,
    #' @field selection Vector of integers corresponding to the ID number of 
    #'   combinations (column `n` in `self$res`) that is to be highlighted.
    selection = NULL,

  #-Public-methods--------------------------------------------------------------

    #' @description
    #' Construct and set up a [PscanPlotter] instance.
    #'
    #' @param analyzed List; Output of [analyze_parameter_scan()].
    #' @param variable Str; Name of variable in *analyzed* to visualize. Can 
    #'   be changed later with `set_variable()`. Allowed values are the keys 
    #'   in *analyzed* except for `params` and `metrics`.
    #'
    #' @seealso [analyze_parameter_scan()]
    #'
    #' @md
    initialize = function(analyzed, variable = "dBM") {
      self$analyzed = analyzed
      # Get the number of metrics and the number of parameters from the data
      self$params = analyzed[["params"]]
      self$n_params = length(self$params)
      self$metrics = analyzed[["metrics"]]
      self$n_metrics = length(self$metrics)
      self$set_variable(variable)
    },

    #' @description Choose which variable to visualize.
    #'
    #' @param variable Chosen variable name. One of "dBM", "cBM", "cBM_end"
    set_variable = function(variable) {
      choices = private$get_variable_options()
      if (!variable %in% choices) {
        msg = "Invalid variable `%s`. Choose one of: `%s`."
        stop(sprintf(msg, variable, paste(choices, collapse = "`, `")))
      }
      private$variable = variable
      self$res = self$analyzed[[variable]]
      private$sort_results()
    },

    #' @description Enter analysis loop.
    #'
    #' This plots the analysis results and enters a simple command-line 
    #' interface through which more insights can be gathered. Particularly, 
    #' it allows highlighting specific parameter combinations, either by 
    #' their index number or by selecting the best performers according to a 
    #' given metric.
    #'
    #' @seealso `PscanPlotter$plot()`
    #' @md
    analyze = function() {
      private$quit_requested = FALSE
      self$plot()
      while (!private$quit_requested) {
        private$ask_input()
      }
      cat("[PSP]Goodbye!\n")
    },

    #' @description Plot parameter scan results.
    #'
    #' For every combination of scanned parameter and metric, a subplot is 
    #' generated in which the parameter values are plotted against 
    #' performance score in that metric for every parameter combination.  
    #' 
    #' The result of this is static. Use `PscanPlotter$analyze()`
    #' for an interactive version.
    #'
    #' @md
    plot = function() {
      # Set up the plot window
      oldpar = private$set_par()
      on.exit(par(oldpar))

      # Create new graphics device
#      if (is.null(private$device_number)) {
#        dev.new()
#        private$device_number = dev.cur()
#      }
      for (i_metric in 1:self$n_metrics) {
        for (i_param in 1:self$n_params) {
          private$draw_points(i_metric, i_param)
        }
      }
      # Force creation of Rplots.pdf when run from Rscript
      if (!interactive()) {
        dev.off()
      }
    },

    #' @description Print information on selected parameter combinations.
    #'
    #' The parameter values and performance scores of all combinations 
    #' referred to by the integers in *selection* are printed to console.
    #'
    #' @param selection Vector of integers representing IDs of parameter 
    #'   combinations (i.e. column `n` in `self$res`).
    #'
    print_info = function(selection) {
      cat(sprintf("Performance scores for variable `%s`.\n", private$variable))
      ncol = length(self$res)
      base_string = paste("|", rep("%6.5s", ncol), collapse = " ")
      base_string = paste(base_string, "|\n")
      cat(do.call(sprintf, c(base_string, as.list(colnames(self$res)))))
      for (s in selection) {
        values = self$res[s, ]
        cat(do.call(sprintf, c(base_string, as.list(c(values)))))
      }
    }
  ),

  private = list(
  #-Private-fields--------------------------------------------------------------
    quit_requested = FALSE,
    base_prompt = "\n[PSP] %s \n>>> ",
    base_instructions = "Select combinations to highlight or enter command. 
`?` for help. `q` to quit.",
    help_message = "You can highlight a specific parameter combination and 
print its parameter set and performance scores by issuing the index of the 
combination or a comma-separated list of indices.

The following additional commands are available:

?   Show this help message.
q   Quit the parameter scan analyzer session.
b   Highlight best performers. You will be queried to select a metric.
    The 5 best scoring parameter combinations for the selected metric will 
    then be highlighted.
v   Select model variable for which performance is to be evaluated.
",
    ## @field variable String representing the key in *self.analyzed* for 
    ##   which performance is plotted.
    variable = NULL,
    device_number = NULL,
    
  #-Private-methods-------------------------------------------------------------

    ## Update self$sorted
    sort_results = function() {
      # Create different orderings for every metric
      sorted = list()
      for (metric in self$metrics) {
        performance = abs(self$res[[metric]] - metric_map[[metric]]$target)
        sorted[[metric]] = self$res[order(performance), ]
      }
      self$sorted = sorted
    },

    ## Prompt user for input at the "main" level.
    ask_input = function() {
      input = prompt_user(sprintf(private$base_prompt, 
                                  private$base_instructions))
      # Quit
      if (input == "q") {
        private$quit_requested = TRUE
        return()
      # Help
      } else if (input == "?") {
        cat(private$help_message)
      # Standard: highlight specific combinations
      } else if (private$parse_numbers(input)) {
        self$plot()
        self$print_info(self$selection)
      # Highlight *n* best performers
      } else if (input == "b") {
        private$query_best_performers()
      } else if (input == "v") {
        private$query_variable()
      } else {
        private$input_not_understood(input)
      }
    },

    ## Ask for user input for displaying best scoring combinations.
    query_best_performers = function() {
      # First, ask user to select metric
      msg = "Select metric for which to highlight best performers:"
      input = menu(self$metrics, title = msg)

      if (input == 0) {
        return()
      } else {
        metric = self$metrics[[input]]
      }

      ## Continue by querying number of points to highlight
      n_to_highlight = 5
      best_performers = self$sorted[[metric]][1:n_to_highlight,]
      self$selection = best_performers$n
      # Go!
      self$plot()
      self$print_info(self$selection)
    },

    ## Return the allowed variables, i.e. those variable names that are 
    ## present in self$analyzed.
    get_variable_options = function() {
      return(setdiff(names(self$analyzed), c("params", "metrics")))
    },

    query_variable = function() {
      msg = "Select variable for which to show performance:"
      choices = private$get_variable_options()
      selection = menu(choices, title = msg)
      if (selection == 0) {
        return()
      } else {
        # Input understood as integer
        variable = choices[selection]
      }
      self$set_variable(variable)
      self$plot()
    },

    ## Try parsing a user input of potentially comma-separated numbers
    parse_numbers = function(input) {
      # Check for comma
      if (grepl(",", input)) {
        numbers = strsplit(input, ",")[[1]]
        self$selection = as.integer(numbers)
        return(TRUE)
      } else {
        number = as.integer(input)
        if (is.na(number)) {
          return(FALSE)
        } else {
          self$selection = number
          return(TRUE)
        }
      }
    },

    ## Handle unrecognized user input.
    input_not_understood = function(input) {
      cat(sprintf("Your input `%s` was not understood.\n", input))
    },

    ## Plot points into subplot at *row*, *col*.
    ## Rows correspond to metrics and columns to scan parameters.
    draw_points = function(row, col) {
      # Get data to plot
      param_name = self$params[[col]]
      metric_name = self$metrics[[row]]
      param_values = self$res[[param_name]]
      metric_values = self$res[[metric_name]]

      # Select correct plot
#      graphics::par(mfg = c(row, col))
      graphics::plot(param_values, metric_values, 
           xlab = "", ylab = "",
           ylim = metric_map[[metric_name]][["limits"]],
           axes = FALSE,
           frame.plot = TRUE
      )

      # Highlight points
      graphics::lines(param_values[self$selection],
                      metric_values[self$selection],
                      type = "p", col = "red")

      # Add ticks with or without labels
      xticklabels = ifelse(row == self$n_metrics, TRUE, FALSE)
      yticklabels = ifelse(col == 1, TRUE, FALSE)
      graphics::Axis(side = 1, labels = xticklabels)
      graphics::Axis(side = 2, labels = yticklabels)
      if (row == 1 & col == 1) { #as.integer(self$n_params/2 + 0.5)) {
        ## Global title
        graphics::title(private$variable)
      }
      if (row == self$n_metrics) {
        graphics::mtext(param_name, side = 1, line = 3)
      }
      if (col == 1) {
        graphics::mtext(metric_name, side = 2, line = 3)#, outer = TRUE
      }
    },
    ## Set up plot par() and return original par() for resetting.
    set_par = function() {
      oldpar = par(no.readonly = TRUE)
      graphics::par(mfrow = c(self$n_metrics, self$n_params),
                mar = c(1.0, 1.0, 1.0, 1.0),
                oma = c(4, 4, 1, 1)
      )
      return(oldpar)
    }
  )
)

#' Plot Parameter Scan Results
#'
#' Visualize the results of a parameter scan and allow interactive inspection 
#' of model performance in parameter space.
#'
#' @details
#' Under the hood this function just creates a [PscanPlotter] object and calls its 
#' `analyze` method.
#'
#' @param analyzed List; Output of [analyze_parameter_scan()].
#' @param variable Str; Name of variable in *analyzed* to visualize. Can 
#'   be changed later with `PscanPlotter$set_variable()`. Allowed values are 
#'   the keys in *analyzed* except for `params` and `metrics`.
#' @param interactive boolean; Toggle between just creating a static plot 
#'   (`interactive = FALSE`) or entering a small, interactive analysis 
#'   setting (`interactive = TRUE`, default).
#' @return A [PscanPlotter] object.
#'
#' @seealso [analyze_parameter_scan()], [PscanPlotter]`$analyze()`
#'
#' @examplesIf interactive()
#' # There needs to be data available with which the modle is to be compared.
#' # For this example, use data provided by the package.
#' path = system.file("extdata", package = "growR")
#' datafile = file.path(path, "posieux1.csv")
#'
#' # Analyze example output of `run_parameter_scan()`.
#' results = analyze_parameter_scan(parameter_scan_example, datafile = datafile)
#' # The following plots the results.
#' psp = plot_parameter_scan(results, interactive = FALSE)
#'
#' # The interactive session can still be entered later from the returned 
#' # PscanPlotter object
#' psp$analyze()
#' 
#' @md
#' @export
plot_parameter_scan = function(analyzed, variable = "dBM", interactive = TRUE) {
  PSP = PscanPlotter$new(analyzed, variable = variable)
  if (interactive) {
    PSP$analyze()
  } else {
    PSP$plot()
  }
  return(PSP)
}
