get_param_file_path = function(site = "posieux") {
  input_dir = system.file("extdata", package = "growR")
  param_file = file.path(input_dir, sprintf("%s_parameters.csv", site))
  return(param_file)
}

instantiate_MV = function(param_file) {
  P = ModvegeParameters$new(param_file)
  MV = ModvegeSite$new(P)
  return(MV)
}

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

save_temp_output = function(MV) {
  path1 = tempfile(fileext = ".dat")
  MV$write_output(path1, force = TRUE)
  # Remove #date and #version
  path2 = remove_lines(path1, c("date", "version"))
  return(path2)
}

remove_lines = function(infile, to_remove) {
  new_path = tempfile(fileext = ".dat")
  connection = file(infile, open = 'r')
  grepstring = paste(sprintf("#%s;", to_remove), collapse = "|")
  while(TRUE) {
    line <- readLines(connection, n = 1)
    if(length(line) == 0) {
      break
    } else {
      if(!grepl(grepstring, line)) {
        write(line, file = new_path, append = TRUE)
      }
    } 
  }
  on.exit(close(connection))
  return(new_path)
}

test_that("ModvegeSite initialization with package example data", {
  # Load example parameters  
  param_file = get_param_file_path("posieux")
  expect_true(file.exists(param_file))

  # Instantiate parameters and modvegesite
  MV = instantiate_MV(param_file)
  expect_contains(class(MV), "ModvegeSite")
})

test_that("ModvegeSite methods: run(), write_to_file()", {
  mv_output_snapshot = "modvege_reference_output.dat"
  announce_snapshot_file(mv_output_snapshot)

  E = get_example_environment("posieux")
  MV = ModvegeSite$new(E$parameters)
  year = E$years[[1]]
  E1 = E$get_environment_for_year(year)
  # Standard run
  expect_no_error(MV$run(year, E1$W, E1$M))
  path = save_temp_output(MV)
  expect_snapshot_file(path, mv_output_snapshot, compare = compare_file_text)
})

