test_that("Creation of empty directory structure", {
  unlink(.tmpdir(), recursive = TRUE)
  dir.create(.tmpdir())
  warnings = capture_warnings(setup_directory(.tmpdir(), 
                                              include_examples = FALSE,
                                              force = TRUE))
  for (dir in c("data", "input", "output")) {
    expect_true(file.exists(file.path(.tmpdir(), dir)))
  }
})

test_that("Creation of directory structure with example files", {
  warnings = capture_warnings(setup_directory(.tmpdir(), force = TRUE))
  regexp = ".*already exists.|.*is not empty."
  for (warning in warnings) {
    expect_match(warning, regexp)
  }
  expect_true(file.exists(file.path(.tmpdir(), "example_config.txt")))
  expect_true(file.exists(file.path(.tmpdir(), "input", 
                                    "posieux_parameters.csv")))
})

