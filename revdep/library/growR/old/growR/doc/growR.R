## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6,
  fig.width = 8
)

## ----include = FALSE----------------------------------------------------------
# Store the original wd
original_dir = getwd()

## ----tempdir------------------------------------------------------------------
working_dir = file.path(tempdir(), "growR_tutorial")
dir.create(working_dir)
setwd(working_dir)

## ----include = FALSE----------------------------------------------------------
# knitr resets working dir to root.dir after every chunk.
# This makes sure we stay in working_dir
knitr::opts_knit$set(
  root.dir = working_dir
)

## ----setup_directory----------------------------------------------------------
library(growR)
# Check that working directory is correct
print(working_dir)
getwd()
setup_directory(working_dir, force = TRUE)

## ----read_config--------------------------------------------------------------
environments = read_config("example_config.txt")

## ----run_simulation-----------------------------------------------------------
results = growR_run_loop(environments, 
                         output_dir = file.path(working_dir, "output"))

## ----print_results------------------------------------------------------------
# Just print the first years of the first run (i.e. year 2013 at site Sorens)
results[[1]][[1]]

## ----plot_results-------------------------------------------------------------
results[[1]][[1]]$plot()

## ----include = FALSE----------------------------------------------------------
# Simulate the changes made through a config file.
env0 = environments[[1]]$clone()
env0$years = 2013
env1 = env0$clone(deep = TRUE)
env1$parameters$set_parameters(list(NI = 0.5))
env2 = env0$clone(deep = TRUE)
env2$parameters$set_parameters(list(NI = 1.0))
new_envs = c(env0, env1, env2)

## ----ni_screening-------------------------------------------------------------
new_results = growR_run_loop(new_envs)
# Plot all results
for (run in new_results) {
  print(run[[1]]$parameters$NI)
  run[[1]]$plot()
}

## ----include = FALSE----------------------------------------------------------
# Clean up
knitr::opts_knit$set(
  root.dir = original_dir
)
setwd(original_dir)
#unlink(working_dir, recursive = TRUE)

