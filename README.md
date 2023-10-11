# rmodvege - R implementation of the grassland model ModVege[^1]

<!-- badges: start -->
  [![R-CMD-check](https://github.com/kuadrat/rmodvege/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kuadrat/rmodvege/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

![](man/figures/logo.png)

This `R` implementation of the grassland model `ModVege` by [Jouven et 
al.](https://doi.org/10.1111/j.1365-2494.2006.00515.x)[^1] is based off an 
`R` implementation created by Pierluigi Calanca[^2].

## Installation

### From CRAN

This is the preferred installation route for most users.

This `R` package can be installed as usual from 
[CRAN](https://cran.r-project.org/) by issuing the following at the prompt of 
an `R` session:
```
install.packages("rmodvege")
```

### From source

Installing from source might make sense if...
    - you intend on making changes to the model,
    - you want to contribute to package development and maintenance,
    - for some reason installation from CRAN is not an option for you.

In this case,

1. Start by cloning this repository"
```
$ git clone git@github.com:kuadrat/rmodvege.git
```
or via https:
```
$ git clone https://github.com/kuadrat/rmodvege.git
```
This will create a directory `rmodvege` in your file system.

If you don't have or don't want to use *git*, you could alternatively copy 
the source code as a .zip file from 
[github](https://github.com/kuadrat/rmodvege/archive/refs/heads/master.zip).
Unzip the contents into a directory `rmodvege`.
2. You can now install *rmodvege* by issuing the following at the prompt of 
an `R` session:
```
install.packages("/full/path/to/rmodvege", repos = NULL)
```
You should replace `"/full/path/to/"` with the actual path to the `rmodvege` 
directory on your computer. Also, replace slashes (`/`) with backslashe (`\`) 
if you're on Windows.

`rmodvege` should now be installed and available in `R` through 
`library(rmodvege)`.

If you make changes to the source files in the `rmodvege` directory, just 
uninstall the current version (issue `remove.packages("rmodvege")` in `R`) 
and repeat step 2 above.

### Non-package version

If you just want to focus on using and adjusting the modvege model and feel 
that the structure of an `R` package is more of a hindrance than a help to 
your cause, there is a third option.
Simply use the pre-`R`-package version of `rmodvege`, which is essentially a 
collection of `R` scripts.
Some users might be more familiar or comfortable working in this manner 
instead of working with package code.

Go to https://github.com/kuadrat/rmodvege-scripts to access the script-based 
implementation of modvege. Note, however, that the script based version is 
not maintained and might therefore lack some functionality which is provided 
by the `rmodvege` package.

## References

[^1]: Jouven, M., P. Carrère, und R. Baumont. „Model Predicting Dynamics of 
Biomass, Structure and Digestibility of Herbage in Managed Permanent 
Pastures. 1. Model Description“. Grass and Forage Science 61, Nr. 2 (2006): 
112–24. [https://doi.org/10.1111/j.1365-2494.2006.00515.x](https://doi.org/10.1111/j.1365-2494.2006.00515.x).

[^2]: Calanca, Pierluigi, Claire Deléglise, Raphaël Martin, Pascal Carrère, 
und Eric Mosimann. „Testing the Ability of a Simple Grassland Model to 
Simulate the Seasonal Effects of Drought on Herbage Growth“. Field Crops 
Research 187 (Februar 2016): 12–23. 
[https://doi.org/10.1016/j.fcr.2015.12.008](12–23. https://doi.org/10.1016/j.fcr.2015.12.008).

