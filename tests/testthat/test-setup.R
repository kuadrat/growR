test_that("Creation of empty directory structure", {
  expect_warning(setup_directory(include_examples = FALSE), 
                 regexp = "is not empty.")
  for (dir in dirs) {
    expect_true(file.exists(file.path(".", dir)))
  }
  clean_up_dir()
})

test_that("Creation of directory structure with example files", {
  expect_warning(setup_directory(), 
                 regexp = "is not empty.")

  expect_true(file.exists(file.path(".", "example_config.txt")))
  expect_true(file.exists(file.path(".", "input", "posieux_parameters.csv")))

#  clean_up_dir()
})

