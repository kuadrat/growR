## Helper to get user input from scripts or interactive sessions
prompt_user = function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n = 1))
  }
}

## Prompt user for confirmation to proceed
ask_for_confirmation = function(n_combinations) {
  print(sprintf("Number of parameter combinations: %s", n_combinations))
  response = prompt_user("Continue? [Y/n] ")
  if (length(response) > 0 && tolower(response) %in% c("x", "n", "c", "q")) {
    print("Quitting as requested by user.")
    rlang::interrupt()
  }
}

#' Parameter Scan
#'
#' Run modvege for a different sets of parameters.
#'
#' @param environment Either a `ModvegeEnvironment` instance with all the site, 
#'   management and weather inputs expected by `ModvegeSite$run()` **or** a 
#'   string representing the name of a config file to read in order to 
#'   generate the `ModvegeEnvironment` with [read_config()]. Note that, in 
#'   the latter case, only the first found configuration is used if there are 
#'   more than one valid uncommented lines in the config file.
#' @param param_values A named list where each key stands for a modvege 
#'   parameter, i.e. a member of `ModvegeParameters$parameter_names`. Each 
#'   list entry then has to be a vector containing the allowed values for the 
#'   respective parameter. All possible allowed combinations of these 
#'   parameter values are then generated and fed into a modvege run.
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
#'   \item{params}{The parameter set that was used to run modvege for this 
#'   entry.}
#'   \item{data}{A list containing for each simulated year a ModvegeSite 
#'   object which was run for the respective year and therefore carries the 
#'   respective results.}
#'   }
#'
#' @seealso ModvegeParameters, [saveRDS()]
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
  environment$parameters$check_parameters(names(param_values))
  # Create all parameter combinations
  parameter_sets = create_combinations(param_values)
  n_combinations = length(parameter_sets)
  if (!force) {
    ask_for_confirmation(n_combinations)
  }

  param_names = names(parameter_sets[[1]])
  results = list()
  for (i in 1:n_combinations) {
    logger(sprintf("Parameter combination %i out of %i.", i, n_combinations),
           level = INFO
    )
    environment$parameters$set_parameters(parameter_sets[[i]])
    mvs = modvege_run_loop(c(environment), 
                           write_files = FALSE,
                           store_results = TRUE
    )
    results[[i]] = list(params = parameter_sets[[i]],
                        data = mvs[[1]])
  }
  logger("Completed parameter scan.", level = INFO)
  logger(paste0("Time used: ", Sys.time() - start_time), level = INFO)
  if (length(outfilename) > "") {
    logger(sprintf("Storing results as `%s`.", outfilename))
    saveRDS(results, outfilename)
  }
  return(results)
}

#' Analyze results of a parameter scan
#'
#' @param parameter_scan_results String or List. If a string, it is 
#'   interpreted as the name of a `rds` file that contains the results of a 
#'   parameter scan which is then loaded using [readRDS()]. Otherwise, it 
#'   should be the output of [run_parameter_scan()] directly.
#'
#' @seealso [run_parameter_scan()], [readRDS()]
#'
#' @md
#' @export
analyze_parameter_scan = function(parameter_scan_results) {
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
  site = results[[1]]$data[[1]]$site_name
  measured_data = load_measured_data(c(site))[[1]]
  # Reduce to relevant years
  relevant_data = measured_data[measured_data$year %in% years, ]
  # Construct a selector for the DOYs present in relevant data
  mask = relevant_data$DOY + 365 * (relevant_data$year - relevant_data$year[1])

  # Assemble metrics. 
  for (combination in 1:n_combinations) {
    modvegesites = results[[combination]]$data
    # Collect year curves into one vector and smoothen dBM
    dBM = c()
    cBM = c()
    for (i_year in 1:n_years) {
      mv = modvegesites[[i_year]]
      cBM = c(cBM, mv$cBM)
      smoothed = box_smooth(mv$dBM, box_width = 28)
      dBM = c(dBM, smoothed)
    }
    results[[combination]][["cBM"]] = list()
    results[[combination]][["dBM"]] = list()
    # Employ performance metrics
    for (metric in metrics_to_use) {
      # cBM
      m_cBM = metric_map[[metric]][["func"]](cBM[mask], relevant_data$cBM)
      results[[combination]][["cBM"]][[metric]] = m_cBM
      # dBM
      m_dBM = metric_map[[metric]][["func"]](dBM[mask], relevant_data$dBM)
      results[[combination]][["dBM"]][[metric]] = m_dBM
    }
  }
  return(results)
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
    #' Construct and set up a PscanPlotter instance.
    #'
    #' @param results Output of [analyze_parameter_scan()].
    #'
    #' @seealso [analyze_parameter_scan()]
    #'
    #' @md
    initialize = function(results) {
      # Get the number of metrics and the number of parameters from the data
      first = results[[1]]
      self$params = names(first[["params"]])
      self$n_params = length(self$params)
      self$metrics = names(first[["dBM"]])
      self$n_metrics = length(self$metrics)

      # Reformat results :TODO: Maybe do this in [analyze_parameter_scan()]
      self$n_combinations = length(results)
      res = data.frame(n = 1:self$n_combinations)
      for (i_param in 1:self$n_params) {
        param_name = self$params[[i_param]]
        res[[param_name]] = extract_results(results, "params", i_param)
      }
      for (i_metric in 1:self$n_metrics) {
        metric_name = self$metrics[[i_metric]]
        res[[metric_name]] = extract_results(results, "dBM", i_metric)
      }
      self$res = res

      # Create different orderings for every metric
      sorted = list()
      for (metric in self$metrics) {
        performance = abs(res[[metric]] - metric_map[[metric]]$target)
        sorted[[metric]] = res[order(performance), ]
      }
      self$sorted = sorted
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
      print("Goodbye!")
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
      graphics::par(mfrow = c(self$n_metrics, self$n_params),
                mar = c(1.0, 1.0, 1.0, 1.0),
                oma = c(2, 4, 1, 1)
      )

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
",
    
  #-Private-methods-------------------------------------------------------------
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
      } else {
        private$input_not_understood(input)
      }
    },

    ## Ask for user input for displaying best scoring combinations.
    query_best_performers = function() {
      # First, ask user to select metric
      cat("Select metric for which to highlight best performers: \n")
      index = 1
      for (metric in self$metrics) {
        cat(sprintf("%s: %s\n", index, metric))
        index = index + 1
      }
      input = prompt_user(">>> ")

      if (input == "q") {
        private$quit_requested = TRUE
        return()
      } else {
        selection = as.integer(input)
        if (is.na(selection)) {
          private$input_not_understood(input)
          return()
        }
        if (!selection %in% seq(1:self$n_metrics)) {
          cat(sprintf("Invalid selection: `%s`.\n", selection))
          return()
        }
        # If we reach this point, input is fine and we are ready to move on.
        metric = self$metrics[[selection]]
      }

      ## Continue by querying number of points to highlight
      n_to_highlight = 5
      best_performers = self$sorted[[metric]][1:n_to_highlight,]
      self$selection = best_performers$n
      # Go!
      self$plot()
      self$print_info(self$selection)
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
      graphics::par(mfg = c(row, col))
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
      if (row == 1) {
        graphics::title(param_name)
      }
      if (col == 1) {
        graphics::mtext(metric_name, side = 2, line = 3)#, outer = TRUE
      }
    }
  )
)

#' Plot Parameter Scan Results
#'
#' Visualize the results of a parameter scan and allow interactive inspection 
#' of model performance in parameter space.
#'
#' @details
#' Under the hood this function just creates a PscanPlotter object and calls its 
#' `analyze` method.
#'
#' @param results Output of [analyze_parameter_scan()]
#'
#' @seealso [analyze_parameter_scan()], `PscanPlotter$analyze()`
#'
#' @md
#' @export
plot_parameter_scan = function(results) {
  PSP = PscanPlotter$new(results)
  PSP$analyze()
}