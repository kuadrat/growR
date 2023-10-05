#' Run a rmodvege simulation
#'
#' Start the loop over runs specified in the config file.
#'
#' By default, returns an empty list but writes output to the output files 
#' as specified in `modvege_environment`. Change this behaviour through the 
#' `write_files` and `store_results` arguments.
#'
#' @param modvege_environments A list of `ModvegeEnvironment` instances.
#' @param write_files boolean; If TRUE, write results of 
#'   :class:`src.modvege_core.ModvegeSite` runs to files.
#' @param store_results boolean; If TRUE, return a list of the 
#'   :class:`src.modvege_core.ModvegeSite` objects which were run.
#' @param output_dir string; name of directory to which output files are to 
#'   be written if `write_files` is `TRUE`.
#'
#' @return If `store_results == TRUE`, a list of the format 
#'   [[run]][[year]] containing clones of the `ModvegeSite` instances that 
#'   were run. Otherwise an empty list. Defaults to 
#'   getOption("rmodvege.output_dir").
#'
#' @export
#' 
modvege_run_loop = function(modvege_environments,
                            write_files = TRUE,
                            store_results = FALSE,
                            output_dir = NULL) {
  n_runs = length(modvege_environments)
  results = list()
  for (run in 1:n_runs) {
    results[[run]] = list()
    logger(sprintf("Starting run %s out of %s.", run, n_runs), level = INFO)
    run_environment = modvege_environments[[run]]
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
        if (is.null(output_dir)) {
          output_dir = getOption("rmodvege.output_dir", default = "output/")
        }
        out_file = sprintf("%s%s%s_%s.dat",
                           output_dir, 
                           run_environment$site_name,
                           run_environment$run_name_in_filename,
                           this_year)
        logger("Entering `modvege$write_output`", level = TRACE)
        modvege$write_output(out_file)
      }

      #-Store-output------------------------------------------------------------
      if (store_results) {
        results[[run]][[i_year]] = modvege$clone(deep = TRUE)
      }
    } # End of loop over simulation years
  } # End of loop over runs
  logger("All runs completed.", level = INFO)
  return(results)
}

