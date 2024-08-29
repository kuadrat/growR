library(usethis)
cCO2_coefficients = read.csv("inst/extdata/IPCC_CO2_concentration.csv")
usethis::use_data(cCO2_coefficients)
