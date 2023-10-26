# growR - R implementation of the grassland model ModVege[^1]

<!-- badges: start -->
  [![Documentation](https://badgen.net/badge/Documentation/github.io/cyan)](https://kuadrat.github.io/growR/)
  [![R-CMD-check](https://github.com/kuadrat/growR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kuadrat/growR/actions/workflows/R-CMD-check.yaml)
  [![codecov](https://codecov.io/gh/kuadrat/growR/graph/badge.svg?token=65OACJW5FV)](https://codecov.io/gh/kuadrat/growR)
<!-- badges: end -->

![](man/figures/logo.png)

This `R` implementation of the grassland model `ModVege` by [Jouven et 
al.](doi:10.1111/j.1365-2494.2006.00515.x)[^1] is based off an 
`R` implementation created by Pierluigi Calanca[^2].

The implementation in this package contains a few additions to the above 
cited version of ModVege, such as simulations of management decisions, and 
influences of snow cover. As such, the model is fit to simulate grass growth 
in mountainous regions, such as the Swiss Alps.

The package also contains routines for calibrating the model and helpful 
tools for analysing model outputs and performance.

## Contents

1. [Installation](#installation)
2. [Getting Started](#getting-started)
4. [Contributing](#contributing)
5. [Contact](#contact)
3. [Glossary](#glossary)
7. [Footnotes and References](#footnotes-and-references)

## Installation

### From CRAN

This is the preferred installation route for most users.

This `R` package can be installed as usual from 
[CRAN](https://cran.r-project.org/) by issuing the following at the prompt of 
an `R` session:
```
install.packages("growR")
```

### From source

Installing from source might make sense if...

- you intend on making changes to the model[^3],
- you want to contribute to package development and maintenance,
- you want to get access to the cutting edge version, which may have changes
  not yet available on the CRAN version but is also likely less stable,
- for some reason installation from CRAN is not an option for you.

In this case,

1. Start by cloning this repository"
    ```
    $ git clone git@github.com:kuadrat/growR.git
    ```
    or via https:
    ```
    $ git clone https://github.com/kuadrat/growR.git
    ```
    This will create a directory `growR` in your file system.
    
    If you don't have or don't want to use *git*, you could alternatively copy 
    the source code as a .zip file from 
    [github](https://github.com/kuadrat/growR/archive/refs/heads/master.zip).
    Unzip the contents into a directory `growR`.

2. You can now install *growR* by issuing the following at the prompt of 
    an `R` session:
    ```
    install.packages("/full/path/to/growR", repos = NULL)
    ```
    You should replace `"/full/path/to/"` with the actual path to the `growR` 
    directory on your computer. Also, replace slashes (`/`) with backslashe (`\`) 
    if you're on Windows.

`growR` should now be installed and available in `R` through 
`library(growR)`.

If you make changes to the source files in the `growR` directory, just 
uninstall the current version (issue `remove.packages("growR")` in `R`) 
and repeat step 2 above.

### Non-package version

If you just want to focus on using and adjusting the ModVege model and feel 
that the structure of an `R` package is more of a hindrance than a help to 
your cause, there is a third option.
Simply use the pre-`R`-package version of `growR`, called `rmodvege`, 
which is essentially a collection of `R` scripts.
Some users might be more familiar or comfortable working in this manner 
instead of working with package code.

Go to https://github.com/kuadrat/rmodvege-scripts to access the script-based 
implementation of ModVege. Note, however, that the script based version is 
not maintained and might therefore lack some functionality which is provided 
by the `growR` package.

## Getting Started

The package documentation is hosted on github pages: 
https://kuadrat.github.io/growR/.
Have a look to find an introductory tutorial, further information as well as 
the complete package reference.

Alternatively (in case github pages are down or you prefer an offline 
solution), you can find the same information under *Reference manual* and 
*Vignettes* on the CRAN package homepage: 
https://cran.r-project.org/web/packages/growR/index.html

Finally, it's also possible to directly access the package documentation and 
vignettes from an `R` interpreter, using the `?` and `vignette()` tools, e.g.
```
> library(growR)
# Get help on a function or object
> ?growR_run_loop
# some output...

# List available vignettes
> vignette(package = "growR")
Vignettes in package ‘growR’:

parameter_descriptions  
                        Parameter Descriptions (source, html)
growR                   Tutorial (source, html)

# Inspect a vignette
> vignette("growR")
```

## Contributing

All forms of contributions to this project are warmly welcome. You are invited to:
- provide direct feedback over e-mail.
- submit bug reports and feature requests via [github issues](https://github.com/kuadrat/growR/issues).
- make changes and additions to the code and submit [pull requests](https://www.howtogeek.com/devops/what-are-git-pull-requests-and-how-do-you-use-them/) to let your contributions become part of future versions.
- suggest improvements for or write documentation and tutorials.
- reference work that made use of `growR` here.

If you intend to collaborate in a regular and ongoing manner, best get in touch with [Kevin Kramer](#contact).

## Contact

[Kevin Kramer](https://www.physik.uzh.ch/~kekram): kevin.pasqual.kramer@protonmail.ch


## Glossary

Terms used in this project

- `growR`
Name of this project and the corresponding R package. The shown 
capitalization is adhered to even when used in function or object names in 
the code base.
- `ModVege`
The basis for the underlying grassland model implemented here. The naming 
convention of objects overrides the capitalization shown here, when the model 
name is referred to in function and object names.
- `rmodvege`
Early name of this project and still the name of a legacy project that was 
not factored as an R package, but rather as a collection of R scripts. Still 
available, though unmaintained at https://github.com/kuadrat/rmodvege-scripts/.

## Footnotes and References

[^1]: Jouven, M., P. Carrère, und R. Baumont. „Model Predicting Dynamics of 
Biomass, Structure and Digestibility of Herbage in Managed Permanent 
Pastures. 1. Model Description“. Grass and Forage Science 61, Nr. 2 (2006): 
112–24. [doi:10.1111/j.1365-2494.2006.00515.x](doi:10.1111/j.1365-2494.2006.00515.x).

[^2]: Calanca, Pierluigi, Claire Deléglise, Raphaël Martin, Pascal Carrère, 
und Eric Mosimann. „Testing the Ability of a Simple Grassland Model to 
Simulate the Seasonal Effects of Drought on Herbage Growth“. Field Crops 
Research 187 (Februar 2016): 12–23. 
[doi:10.1016/j.fcr.2015.12.008](doi:10.1016/j.fcr.2015.12.008).

[^3]: If you make changes that generally improve `growR`, it would be great if you could 
share them to make them available to all future users. See [Contributing](#contributing).

