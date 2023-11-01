## Write all non-function field values to a file
save_temp_output = function(M) {
  path1 = tempfile(fileext = ".dat")
  
  tab = data.frame(year = M$year, DOY = M$DOY)
  write.table(tab, file = path1, quote = FALSE)
  return(path1)
}

test_that("ManagementData read example data", {
  md_default_snapshot = "management_example_values.dat"
  announce_snapshot_file(md_default_snapshot)

  extdata = system.file("extdata", package = "growR")
  example_management_file = file.path(extdata, "posieux_management1.txt")
  M = ManagementData$new(example_management_file)

  path = save_temp_output(M)
  expect_snapshot_file(path, md_default_snapshot, compare = compare_file_text)
})

test_that("ManagementData input checking", {
  # Store old options for resetting after test
  old_opts = options()
  on.exit(options(old_opts))
  set_growR_verbosity(5)

  incomplete_management_data = data.frame(year = 1)
  M = ManagementData$new()
  M$management_file = "<testthat>"
  expect_error(M$ensure_file_integrity(incomplete_management_data),
               regexp = "parameters were missing")

  wrong_DOY_management_data = data.frame(year = c(1, 1, 1), DOY = c(1, 3, 2))
  expect_error(M$ensure_file_integrity(wrong_DOY_management_data),
               regexp = "not monotonically increasing")

  wrong_DOY_management_data2 = data.frame(year = c(1, 1, 1), DOY = c(1, 2, 2))
  expect_error(M$ensure_file_integrity(wrong_DOY_management_data2),
               regexp = "not monotonically increasing")

  fake_management_data = data.frame(year = c(1, 1, 1), DOY = c(1, 2, 3))
  expect_no_error(M$ensure_file_integrity(fake_management_data))

})
