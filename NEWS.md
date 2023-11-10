# growR 1.0.1

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

## Changed

* Input data CSV files are now actual CSV files, instead of 
  "semicolon-separated value" files.

* `ParameterData` now assumes default values for most parameters. 
  `check_parameters` now only throws an error if any of the really  
  *required* parameters are missing.

* Number of days per year is now inferred from weather data instead of 
  hardcoded to 365, allowing simulations to be run for years with incomplete 
  data.

## Fixed

* autocut: `get_annual_gross_yield` was incorrectly hardcoded to return 1.

## Removed

* Removed superfluous weather inputs.

* Removed automatic temperature *correction*.

* Redundant argument *store_results* in `growR_run_loop`.

# growR 1.0.0

* Initial CRAN submission.

# growR 0.0.1

* Renaming from `rmodvege` to `growR`.

# rmodvege 2.1.3

* Port of `rmodvege-scripts` to R package structure.
