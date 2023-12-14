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
working_dir = file.path(tempdir(), "growR_calibration_tutorial")
dir.create(working_dir)
setwd(working_dir)
library(growR)
setup_directory(working_dir, force = TRUE)

## ----include = FALSE----------------------------------------------------------
# knitr resets working dir to root.dir after every chunk.
# This makes sure we stay in working_dir
knitr::opts_knit$set(
  root.dir = working_dir
)

## ----prepare_env--------------------------------------------------------------
envs = read_config("example_config.txt")
# We only need one run environment.
env = envs[[1]]
# Save compuation time by considering only one year
env$years = env$years[1]

## ----analyze_pscan------------------------------------------------------------
# Prepare path to reference data
site = env$site_name
print(site)
datafile = file.path("data", sprintf("%s.csv", site))

## ----include = FALSE----------------------------------------------------------
# Clean up
knitr::opts_knit$set(
  root.dir = original_dir
)
setwd(original_dir)
#unlink(working_dir, recursive = TRUE)

