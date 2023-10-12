library(usethis)
library(rmodvege)

# Create data for the results of an exemplary parameter scan.
env = create_example_environment()
# Reduce years to reduce runtime
env$years = env$years[[1]]

param_values = list(w_FGA = seq(0.25, 1, 0.25),
                    w_FGB = seq(0.25, 1, 0.25),
                    w_FGC = seq(0, 0.25, 0.25),
                    w_FGD = c(0),
                    NI = seq(0.75, 1.0, 0.25)
)

parameter_scan_example = run_parameter_scan(env, param_values, force = FALSE)
usethis::use_data(parameter_scan_example, overwrite = TRUE)
