get_example_environment = function(site = "posieux") {
  input_dir = system.file("extdata", package = "growR")
  param_file = sprintf("%s_parameters.csv", site)
  weather_file = sprintf("%s_weather.txt", site)
  management_file = sprintf("%s_management1.txt", site)
  E = ModvegeEnvironment$new(paste0(site, "1"),
                             param_file = param_file,
                             weather_file = weather_file,
                             management_file = management_file,
                             input_dir = input_dir
  )
}

## Test modvegesite$run when no explicit cut dates are provided.
test_that("modvegesite$run() with autocut", {
  E = get_example_environment("posieux")
  MV = ModvegeSite$new(E$parameters)
  year = E$years[[1]]
  E1 = E$get_environment_for_year(year)
  management = E1$M
  # Replace with empty management
  management$is_empty = TRUE
  management$cut_DOY = NULL
  management$cut_years = NULL
  management$n_cuts = NULL
  for (intensity in c("high", "middle", "low")) {
    management$intensity = intensity
    expect_no_error(MV$run(year, E1$W, management))
  }
})

