## Write all non-function field values to a file
save_temp_output = function(P) {
  path1 = tempfile(fileext = ".dat")

  fields = names(P)
  lines = c()
  for (field in fields) {
    value = P[[field]]
    if (!is.function(value) & !is.environment(value)) {
      line = sprintf("%s = %s", field, value)
      lines = c(lines, line)
    }
  }

  connection = file(path1)
  on.exit(close(connection))
  writeLines(lines, connection)
  return(path1)
}

test_that("ParameterData default values", {
  pd_default_snapshot = "parameterdata_default_values.dat"
  announce_snapshot_file(pd_default_snapshot)
  P = ModvegeParameters$new()

  path = save_temp_output(P)
  expect_snapshot_file(path, pd_default_snapshot, compare = compare_file_text)
})

test_that("ParameterData parameter checking", {
  # Store old options for resetting after test
  old_opts = options()
  on.exit(options(old_opts))
  set_growR_verbosity(5)
  P = ModvegeParameters$new()
  expect_message(P$check_parameters(c("foo_unrecognized_parameter"), 
                                    check_for_completeness = FALSE),
                 regexp = "[WARNING]")
  expect_error(P$check_parameters(c("NI"), check_for_completeness = TRUE),
               regexp = "[ERROR]")
})
