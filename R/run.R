pkgs_for_parallelization = c("parallel")
parallel_error_message = paste0("The following R packages are required in ",
  "order to run simulations in parallel:", pkgs_for_parallelization)

#' Run growR simulations
#'
#' Start the loop over runs specified in the config file.
#'
#' By default, returns an empty list but writes output to the output files 
#' as specified in the *site_name* and *run_name* fields of the supplied 
#' [ModvegeEnvironment] instances. Change this behaviour through the 
#' `write_files` and `store_results` arguments.
#'
#' @param modvege_environments A list of [ModvegeEnvironment] instances.
#' @param output_dir string; name of directory to which output files are to 
#'   be written. If `output_dir == ""` (default), no files are written.
#' @param return_results boolean Whether or not simulation results are to be 
#'   kept in memory and returned at function completion. Can be disabled to 
#'   safe RAM, especially if results are anyways written to file (which is 
#'   the case if `output_dir` is not empty).
#' @param independent boolean; If `TRUE` (default) the simulation for each 
#'   year starts with the same initial conditions, as specified in the 
#'   parameters of the modvege_environments. If `FALSE`, initial conditions 
#'   are taken as the final state values of the simulation of the previous 
#'   year.
#' @param n_threads int; *Experimental* The number of parallel threads to use.
#'   Requires the `parallel` package to be installed and is not tested on 
#'   Windows. The simulation workload  of the supplied *modvege_environments* is 
#'   split evenly across *n_threads* different CPUs.
#' @return A list of the format `[[run]][[year]]` containing clones of 
#'   the [ModvegeSite] instances that were run. Also write to files, if 
#'   *output_dir* is nonempty.
#'
#' @examples
#' env1 = create_example_environment(site = "posieux")
#' env2 = create_example_environment(site = "sorens")
#'
#' growR_run_loop(c(env1, env2), output_dir = "")
#'
#' @md
#' @export
#' 
growR_run_loop = function(modvege_environments, output_dir = "", 
                          return_results = TRUE, independent = TRUE, 
                          n_threads = 1) {
  # Set up parallelisation
  if (n_threads > 1) {
    # Check if suggested packages are present
    all_installed = rlang::is_installed(pkgs_for_parallelization)
    if (!all_installed) {
      stop(parallel_error_message)
    }
  }

  # Ensure target directory exists
  if (output_dir != "") {
    if (!dir.exists(output_dir)) {
      logger(sprintf("Creating output directory `%s`.", output_dir), 
             level = INFO)
      dir.create(output_dir, recursive = TRUE)
    }
  }
  n_runs = length(modvege_environments)
  results = list()

  if (n_threads > 1) {
    # Multi-thread run
    results = parallel::mclapply(modvege_environments, run_loop_parallel, 
                                 output_dir, return_results, independent, 
                                 mc.cores = n_threads)
  } else {
    # Single-thread run
    for (run in 1:n_runs) {
      run_environment = modvege_environments[[run]]
      logger(sprintf("Starting run %s out of %s.", run, n_runs), level = INFO)
      logger(sprintf(" site name: `%s`.", run_environment$site_name), 
             level = INFO)
      logger(sprintf("  run name: `%s`.", run_environment$run_name), 
             level = INFO)
      result = run_loop_core(run_environment, run, n_runs, output_dir, 
                             return_results, independent)
      if (return_results) {
        results[[run]] = result
      }
    } # End of loop over runs
  }
  logger("All runs completed.", level = INFO)
  return(results)
}

#' Run loop in parallel
#' 
#' Simple wrapper around [run_loop_core].
#' 
#' Prints a message and passes dummy values for the *run* and *n_runs* 
#' variables, the former of which is not trivially available from within 
#' mclapply.
#'
#' @param run_environment A ModvegeEnvironment instance; The environment with 
#'   which to call [run_loop_core].
#' @param ... Further arguments (except *run* and *n_runs*) to be passed to 
#'   [run_loop_core].
#' @return A list of [ModvegeSite] instances containing the simulation 
#'   results for each year specified in *run_environment*.
#' @seealso [run_loop_core]
#'
#' @keywords internal
#'
run_loop_parallel = function(run_environment, ...) {
  pid = Sys.getpid()
  message = "[PID:%s]Starting run `%s` for site `%s`."
  logger(sprintf(message, pid, run_environment$run_name, 
                 run_environment$site_name), 
         level = INFO)
  return(run_loop_core(run_environment, "X", "Y", ...))
}

#' Core of growR_run_loop
#'
#' Create the ModvegeSite object from supplied environment and call it's 
#' `$run()` method for each supplied year.
#'
#' @param run_environment A [ModvegeEnvironment] instance containing all the 
#'   information necessary to simulate a site.
#' @param run integer Number of the currently carried out run.
#' @param n_runs integer Total number of runs to carry out.
#' @param output_dir str Path to directory where output files will be written 
#'   to.
#' @param return_results boolean Whether or not simulation results are to be 
#'   kept in memory and returned at function completion. Can be disabled to 
#'   safe RAM, especially if results are anyways written to file (which is 
#'   the case if `output_dir` is not empty).
#' @param independent boolean Whether or not simulations of subsequent years 
#'   are treated as independent.
#' @return A list of [ModvegeSite] instances containing the simulation 
#'   results for each year specified in *run_environment*.
#'
#' @seealso [growR_run_loop]
#'
#' @keywords internal
#'
run_loop_core = function(run_environment, run, n_runs, output_dir, 
                         return_results, independent) {
  results_for_env = list()
  modvege = ModvegeSite$new(run_environment$parameters,
                            site_name = run_environment$site_name,
                            run_name = run_environment$run_name
                            )
  n_years = length(run_environment$years)

  #-Model-core----------------------------------------------------------------
  # For each specified year, run ModVege
  for (i_year in 1:n_years) {
    this_year = run_environment$years[i_year]
    logger(sprintf("[Run %s/%s]Simulating year %s (%s/%s)", run, n_runs, 
                   this_year, i_year, n_years), level = TRACE )
    E = run_environment$get_environment_for_year(this_year)
    # Run the ModVege Core
    modvege$run(this_year, E$W, E$M)

    #-Write-output------------------------------------------------------------

    if (output_dir != "") {
      out_file = sprintf("%s%s_%s.dat",
                         run_environment$site_name,
                         run_environment$run_name_in_filename,
                         this_year)
      out_path = file.path(output_dir, out_file)
      modvege$write_output(out_path, force = TRUE)
    }

    #-Store-output------------------------------------------------------------
    results_for_env[[i_year]] = modvege$clone(deep = FALSE)

    #-Adjust-initial-conditions-for-next-year---------------------------------
    if (!independent) {
      n_days = modvege$days_per_year
      for (initial_condition in c("BMGV", "BMGR", "BMDV", "BMDR", "AgeGV", 
                                  "AgeGR", "AgeDV", "AgeDR")) {
        previous_value = modvege[[initial_condition]][n_days]
        for (ic in c(initial_condition, paste0(initial_condition, "0"))) {
          modvege$parameters[[ic]] = previous_value
        }
      }
    }
  } # End of loop over simulation years
  # Avoid RAM filling by not returning anything.
  if (return_results) {
    return(results_for_env)
  } else {
    return(list())
  }
}

