## This relies on `example_config.txt` being present, which is ensured 
## through the test `test-setup.R`. However, that file is again removed by 
## `test-run.R`, so this test should happen between those two.
test_that("Parameter scan", {
  env = read_config(file.path(.tmpdir(), "example_config.txt"),
                    input_dir = file.path(.tmpdir(), "input"))[[1]]
  # Reduce run time by reducing the number of years
  env$years = env$years[[1]]

  params = list(w_FGA = c(0.4, 0.5),
                w_FGB = c(0.4, 0.5),
                w_FGC = c(0, 0.1, 0.2),
                w_FGD = c(0)
  )
  results = run_parameter_scan(env,
                               params,
                               force = TRUE
                               )
  datafile = file.path(.tmpdir(), "data", sprintf("%s.csv", env$site_name))
  expect_no_error(analyze_parameter_scan(results, datafile = datafile))
})

