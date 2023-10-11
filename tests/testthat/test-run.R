
## This test expects the directory structure and example files to be present, 
## so it has to be run after `setup_directory`, which is run in `test-setup.R`.
test_that("modvege_run_loop with example config", {
  envs = read_config("example_config.txt")
  expect_no_error(modvege_run_loop(envs, 
                                   write_files = FALSE, 
                                   store_results = FALSE))
  clean_up_dir()
})
