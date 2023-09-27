library(usethis)
management_parameters = read.table("data-raw/management_huguenin.dat", 
                              comment = "#", 
                              header = T)
usethis::use_data(management_parameters)
