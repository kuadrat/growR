## This relies on `example_config.txt` being present, which is ensured 
## through the test `test-setup.R`. However, that file is again removed by 
## `test-run.R`, so this test should happen between those two.
test_that("Parameter scan", {
  env = read_config("example_config.txt")[[1]]
  params = list(w_FGA = c(0.4, 0.5),
                w_FGB = c(0.4, 0.5),
                w_FGC = c(0, 0.1, 0.2),
                w_FGD = c(0)
  )
  results = run_parameter_scan(env,
                               params,
                               force = TRUE
                               )
#  analyzed = analyze_parameter_scan(results)
  expect_no_error(analyze_parameter_scan(results))
})

