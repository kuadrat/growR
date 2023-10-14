## This test expects the directory structure and example files to be present, 
## so it has to be run after `setup_directory`, which is run in `test-setup.R`.
test_that("growR_run_loop with example config", {
  envs = read_config(file.path(.tmpdir(), "example_config.txt"), 
                     input_dir = file.path(.tmpdir(), "input"))
  # Reduce run time by reducing the number of years
  for (env in envs) {
    env$years = env$years[[1]]
  }
  expect_no_error(growR_run_loop(envs, 
                                 store_results = FALSE))
  unlink(.tmpdir(), recursive = TRUE)
})
