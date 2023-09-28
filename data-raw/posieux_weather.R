library(usethis)
posieux_weather = read.table("inst/extdata/posieux_weather.txt", 
                              comment = "#", 
                              header = TRUE)
usethis::use_data(posieux_weather)
