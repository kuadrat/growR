# growR 1.0.1

## Added

* Instances where simulation results for variable `dBM` are visualized or 
  compared to experimental data now allow specifying and integration window 
  for `dBM`, which should be set to match the experimental reality. This 
  concerns `ModvegeSite$plot()` and `analyze_parameter_scan()` as well as the 
  `compare.R` script.

## Changed

* Input data CSV files are now actual CSV files, instead of 
  "semicolon-separated value" files.

* `ParameterData` now only throws a warning on missing parameters instead of 
  an error.

## Removed

* Removed superfluous weather inputs.

# growR 1.0.0

* Initial CRAN submission.

# grower 0.0.1

* Renaming from `rmodvege` to `growR`.

# rmodvege 2.1.3

* Port of `rmodvege-scripts` to R package structure.
