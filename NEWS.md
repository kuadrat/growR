# growR 1.2.0.9000

## Added

* `j_start_of_growing_season` is written to output file headers.
* Output file header contains `cut_DOYs`.

## Changed

* Temperature sum is only increased once growing season has started.

# growR 1.2.0

## Added

* Green biomass has a lower bound defined by a minimum grass height. This is 
  to prevent the population from completely dying out and effectively 
  simulates reserves.

## Changed

* The start of the growing season is now determined by the *multicriterial 
  thermal definition* as described in chapter 2.3.1.3 of the dissertation of 
  Andreas Schaumberger:
  *Räumliche Modelle zur Vegetations- und Ertragsdynamik im 
  Wirtschaftsgrünland*, 2011, ISBN-13: 978-3-902559-67-8

# growR 1.1.0

## Added

* Input integrity checking for WeatherData and ManagementData.

* PScanPlotter (`plot_parameter_scan`) now allows to select which variable to 
  display (from dBM, cBM and cBM_tot).

* Instances where simulation results for variable `dBM` are visualized or 
  compared to experimental data now allow specifying and integration window 
  for `dBM`, which should be set to match the experimental reality. This 
  concerns `ModvegeSite$plot()` and `analyze_parameter_scan()` as well as the 
  `compare.R` script.

* S3 dispatch for plot method of `ModvegeSite` objects -> `plot(mvs)` is now 
  possible if `mvs` is a `ModvegeSite` instance.

* `ParameterData` input checking: throws error on duplicate input parameter 
  name.

* Debug utility conveniences `browse` and `browse_end`.

* `ModvegeSite$plot_XXX` functions for more insights into model behaviour, 
  with XXX in `water`, `limitations`, `growth` and `plot_var` for generic 
  variable plotting.

## Changed

* Input data CSV files are now actual CSV files, instead of 
  "semicolon-separated value" files.

* `ParameterData` now assumes default values for most parameters. 
  `check_parameters` now only throws an error if any of the really  
  *required* parameters are missing.

* Number of days per year is now inferred from weather data instead of 
  hardcoded to 365, allowing simulations to be run for years with incomplete 
  data.

* Crop coefficient (`parameters$crop_coefficient`) and senescence capping are 
  now model parameters instead of being hardcoded.

## Fixed

* autocut: `get_annual_gross_yield` was incorrectly hardcoded to return 1.

* `ParameterData$set_parameters` now updates initial condition values, if 
  applicable.

## Removed

* Removed superfluous weather inputs.

* Removed automatic temperature *correction*.

* Redundant argument *store_results* in `growR_run_loop`.

* `SEA` symmetrization around 1 is not enforced anymore.

# growR 1.0.0

* Initial CRAN submission.

# growR 0.0.1

* Renaming from `rmodvege` to `growR`.

# rmodvege 2.1.3

* Port of `rmodvege-scripts` to R package structure.
