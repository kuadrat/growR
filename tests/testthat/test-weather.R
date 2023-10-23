test_that("Reading of weather data", {
  datapath = system.file("extdata", package = "growR")
  weather_file = file.path(datapath, "posieux_weather.txt")
  WD = WeatherData$new(weather_file)
  expect_false(is.null(WD$years))
  W = WD$get_weather_for_year(WD$years[1])
  # Check that none of the important keys are NULL
  keys = c("aCO2", "year", "DOY", "Ta", "Ta_sm", "PAR", "PP", "PET", 
           "liquidP", "melt", "snow", "ndays")
  for (key in keys) {
    if (is.null(W[[key]])) {
      logger(sprintf("Weather variable is NULL: %s.", key), level = 2)
    }
    expect_false(is.null(W[[key]]))
  }
})
