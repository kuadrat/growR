## Write all non-function field values to a file
save_temp_output = function(W) {
  path1 = tempfile(fileext = ".dat")
  
  tab = data.frame(year = W$year, DOY = W$DOY, Ta = W$Ta, precip = W$precip, 
                   PAR = W$PAR, ET0 = W$ET0)
  write.table(tab, file = path1, quote = FALSE)
  return(path1)
}

test_that("WeatherData read example data", {
  wd_default_snapshot = "weather_example_values.dat"
  announce_snapshot_file(wd_default_snapshot)

  extdata = system.file("extdata", package = "growR")
  example_weather_file = file.path(extdata, "posieux_weather.txt")
  W = WeatherData$new(example_weather_file)

  path = save_temp_output(W)
  expect_snapshot_file(path, wd_default_snapshot, compare = compare_file_text)
})

test_that("WeatherData input checking", {
  # Store old options for resetting after test
  old_opts = options()
  on.exit(options(old_opts))
  set_growR_verbosity(5)

  W = WeatherData$new()
  W$weather_file = "<testthat>"

  incomplete_data = data.frame(year = 1)
  expect_error(W$ensure_file_integrity(incomplete_data),
               regexp = "parameters were missing")

  # NA correction
  na_data = data.frame(year = 1, DOY = 1, Ta = NA, precip = NA, PAR = NA, 
                       ET0 = NA)
  checked_data = W$ensure_file_integrity(na_data)
  expect_false(any(is.na(checked_data)))

})
