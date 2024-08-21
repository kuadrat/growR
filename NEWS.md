# growR 1.3.0.9000

## Added

* site_name and run_name are printed to console in `growR_run_loop`.
* PhenologicalAutocut
* Multithreading for `growR_run_loop` with the `parallel` package.
* New parameter for irrigation.
* `growR_run_loop` allows to suppress returning the simulation objects. This 
  is useful to prevent overkill memory usage.

## Changed

* autocut algorithm is now outsourced to its own R6class.
* autocut accepts 'medium' as alias for 'middle' and applies a single cut 
  according to phenology.
* Performance improvements to ModvegeSite$un() in the order of 25%.
* Small performance improvement of `WeatherData` initialization through 
  vectorization of snow model.

## Fixed

* `ModvegeParameters$set_parameters()` can now handle empty lists.
* Fix in determination of start of growing season for cases with no snow in 
  first half-year.

## Removed

* `ModvegeParameters$senescence_cap`
* `ModvegeSite$cBM`

# growR 1.3.0

## Added

* An additional option for determination of growing season start is available 
  through `ModvegeSite$set_SGS_method. This comes with a different available 
  definition of the temperature sum and is reflected in the new methods 
  `start_of_growing_season`, `start_of_growing_season_mtd`, 
  `weighted_temperature_sum`.

## Changed

* `j_start_of_growing_season` is written to output file headers.
* Output file header contains `cut_DOYs`.
* `growR_run_loop` allows use of previous year's state variables as initial 
  conditions for current year by means of the `independent` argument.
* `stubble_height` is now a parameter (i.e. a field of `ModvegeParameters`) 
  instead of a field of `MovegeSite`.
* Lower biomass limit is only applied after `ST2`.

## Fixed

* Window sizes in multicriterial thermal definition were incorrect.
* Calculation of `fAgeDR` was incorrect.
* Biomass is correctly updated after cuts.
* `dBM` can become negative, as it should.
* `fW` now uses correct threshold value of 3.8 instead of 3.81.
* Correction of AEv calculation.
* NULL value for simulations without cut_DOYs still produces a header in the 
  output file.
* Bug where error message was not printed correctly in case of missing 
  parameters.
* Biomass (BM, BMxx) was unaligned with biomass change (dBM, dBMxx).

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

## Removed

* compare.R script is not fetched by `setup_directory()`.

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
