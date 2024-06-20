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
#' @param independent boolean; If `TRUE` (default) the simulation for each 
#'   year starts with the same initial conditions, as specified in the 
#'   parameters of the modvege_environments. If `FALSE`, initial conditions 
#'   are taken as the final state values of the simulation of the previous 
#'   year.
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
                          independent = TRUE) {
  # Parse output dir
  if (output_dir == "") {
    write_files = FALSE
  } else {
    write_files = TRUE
  }
  n_runs = length(modvege_environments)
  results = list()
  for (run in 1:n_runs) {
    results[[run]] = list()
    logger(sprintf("Starting run %s out of %s.", run, n_runs), level = INFO)
    run_environment = modvege_environments[[run]]
    logger(sprintf(" site name: `%s`.", run_environment$site_name), level = INFO)
    logger(sprintf("  run name: `%s`.", run_environment$run_name), level = INFO)
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
                     this_year, i_year, n_years), level = INFO )
      E = run_environment$get_environment_for_year(this_year)
      # Run the ModVege Core
      modvege$run(this_year, E$W, E$M)

      #-Write-output------------------------------------------------------------

      if (write_files) {
        out_file = sprintf("%s%s_%s.dat",
                           run_environment$site_name,
                           run_environment$run_name_in_filename,
                           this_year)
        out_path = file.path(output_dir, out_file)
        logger("Entering `ModvegeSite$write_output`", level = TRACE)
        modvege$write_output(out_path, force = TRUE)
      }

      #-Store-output------------------------------------------------------------
      results[[run]][[i_year]] = modvege$clone(deep = TRUE)

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
  } # End of loop over runs
  logger("All runs completed.", level = INFO)
  return(results)
}

