test_that("Loading example data", {
  example_path = system.file("extdata", package = "growR")
  example_data = file.path(example_path, "posieux1.csv")
  expect_no_error(load_measured_data(c(example_data)))
})

