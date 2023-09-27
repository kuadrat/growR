library(usethis)
yield_parameters = read.table("data-raw/yield_huguenin.dat", 
                              comment = "#", 
                              header = T)
usethis::use_data(yield_parameters)
