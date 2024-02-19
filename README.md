# growR - R implementation of the grassland model ModVege[^1]

<!-- badges: start -->
  [![Documentation](https://badgen.net/badge/Documentation/github.io/cyan)](https://kuadrat.github.io/growR/)
  [![R-CMD-check](https://github.com/kuadrat/growR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kuadrat/growR/actions/workflows/R-CMD-check.yaml)
  [![codecov](https://codecov.io/gh/kuadrat/growR/graph/badge.svg?token=65OACJW5FV)](https://app.codecov.io/gh/kuadrat/growR)
  [![status](https://joss.theoj.org/papers/bd4b3a207a8d4de1dc784dba702e38fc/status.svg)](https://joss.theoj.org/papers/bd4b3a207a8d4de1dc784dba702e38fc)
<!-- badges: end -->

![](man/figures/logo.png)

This `R` implementation of the grassland model `ModVege` by [Jouven et 
al.](https://doi.org/10.1111/j.1365-2494.2006.00515.x)[^1] is based off an 
`R` implementation created by Pierluigi Calanca[^2].

The implementation in this package contains a few additions to the above 
cited version of ModVege, such as simulations of management decisions, and 
influences of snow cover. As such, the model is fit to simulate grass growth 
in mountainous regions, such as the Swiss Alps.

The package also contains routines for calibrating the model and helpful 
tools for analysing model outputs and performance.

## Contents

1. [What is this for?](#what-is-this-for?)
2. [Installation](#installation)
3. [Getting Started](#getting-started)
4. [Contributing](#contributing)
5. [Contact](#contact)
6. [Glossary](#glossary)
7. [Footnotes and References](#footnotes-and-references)

## What is this for?

This `R` package allows simulation of grass growth.

### Why simulate grass growth?

Grasslands constitute one of Earth's most widespread terrestrial 
ecosystems[^3] and a core element in global agriculture, providing roughly 
half the feed inputs for global livestock systems 
[^4].
Beside their contribution to global food production, they provide a catalogue 
of other ecosystem services, such as water flow and erosion regulation, 
pollination service, carbon sequestration and climate regulation 
[^3].
The latter have become particularly important in light of anthropogenic 
climate change [^5].

Understanding the functioning of grassland ecosystems and their responses to 
external changes is therefore of significant interest.
Vegetation models provide a powerful platform for such studies.

### How does this compare to other grass and vegetation models?

The number of grassland models is large and ever-growing.
We can therefore not give a comprehensive list, but will try to make a couple 
of representative comparisons to illustrate where `growR` has its niche.
For the most part, an advantage of `growR` over other, similar models and 
their implementations is its distribution as `R` package via CRAN.

- The [Hurley Pasture Model](https://sites.massey.ac.nz/hurleypasturemodel/hurley-pasture-model/) [^6]
  is a detailed mechanistic model for managed pastures. It is implemented in 
  the *Advanced continuous simulation language (ACSL)* and the source code is 
  available on request.
- [BASGRA](https://github.com/davcam/BASGRA/) [^7] 
  and its descendant [BASGRA_N](https://github.com/MarcelVanOijen/BASGRA_N) 
  [^8] are multi-year grassland models which include tiller dynamics.
  They are also implemented in `R` with the source code freely available. 
  However, they do not come packaged, as `growR` does.
- PROGRASS [^9] was developed to capture the interactions in grass/clover 
  mixtures. As of this writing, no accessible implementation was found.
- The focus of PaSim [^10] is the investigation of livestock production, 
  which is not directly covered in `growR`, under climate change conditions.

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

In this case, start by cloning this repository
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

#### Alternative A

You can now install your local version of *growR* by issuing
the following at the prompt of an `R` session:
```
install.packages("/full/path/to/growR", repos = NULL)
```
You should replace `"/full/path/to/"` with the actual path to the `growR` 
directory on your computer. Also, replace slashes (`/`) with backslashe (`\`) 
if you're on Windows.

`growR` should now be installed and available in `R` through `library(growR)`.   
If you make changes to the source files in the `growR` directory, just 
uninstall the current version (issue `remove.packages("growR")` in `R`) 
and repeat this step.

#### Alternative B

If you make frequent changes to the code, it might be
unpractical to uninstall and reinstall the changed version each time. In that
case, `devtools` comes in very handy (if needed, install it with
`install.packages("devtools")`). It allows you to load a package into an active
`R` session without the need of it being properly installed. The following has
practically the equivalent result as the method described in 
[alternative A](#alternative-a):
```
library(devtools)
devtools::load_all("/full/path/to/growR")
```
The notes about `"/full/path/to"` as in [Alternative A](#alternative-a) apply here as well.
   
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

[Kevin Kramer](https://www.physik.uzh.ch/~kekram/): kevin.pasqual.kramer@protonmail.ch


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
112–24. [doi:10.1111/j.1365-2494.2006.00515.x](https://doi.org/10.1111/j.1365-2494.2006.00515.x).

[^2]: Calanca, Pierluigi, Claire Deléglise, Raphaël Martin, Pascal Carrère, 
und Eric Mosimann. „Testing the Ability of a Simple Grassland Model to 
Simulate the Seasonal Effects of Drought on Herbage Growth“. Field Crops 
Research 187 (Februar 2016): 12–23. 
[doi:10.1016/j.fcr.2015.12.008](https://doi.org/10.1016/j.fcr.2015.12.008).

[^3]: Zhao, Yuanyuan, Zhifeng Liu, and Jianguo Wu. “Grassland Ecosystem 
Services: A Systematic Review of Research Advances and Future Directions.” 
Landscape Ecology 35, no. 4 (April 1, 2020): 793–814. 
[doi:10.1007/s10980-020-00980-3](https://doi.org/10.1007/s10980-020-00980-3).

[^4]: Herrero, Mario, Petr Havlík, Hugo Valin, An Notenbaert, Mariana C. 
Rufino, Philip K. Thornton, Michael Blümmel, Franz Weiss, Delia Grace, and 
Michael Obersteiner. “Biomass Use, Production, Feed Efficiencies, and 
Greenhouse Gas Emissions from Global Livestock Systems.” Proceedings of the 
National Academy of Sciences 110, no. 52 (December 24, 2013): 20888–93. 
[doi:10.1073/pnas.1308149110](https://doi.org/10.1073/pnas.1308149110).

[^5]: IPCC Report 2022, Chapter 5.

[^6]: Thornley, J. H. M. Grassland Dynamics: An Ecosystem Simulation Model. 
CAB International, 1998.

[^7]: Van Oijen, M., M. Höglind, D.R. Cameron, and S.M. Thorsen. 
“BASGRA_2014.” Zenodo, August 13, 2015. https://doi.org/10.5281/zenodo.27867.

[^8]: Höglind, Mats, David Cameron, Tomas Persson, Xiao Huang, and Marcel van 
Oijen. “BASGRA_N: A Model for Grassland Productivity, Quality and Greenhouse 
Gas Balance.” Ecological Modelling 417 (February 1, 2020): 108925. 
[doi:10.1016/j.ecolmodel.2019.108925](https://doi.org/10.1016/j.ecolmodel.2019.108925).

[^9]: Lazzarotto, P., P. Calanca, and J. Fuhrer. “Dynamics of Grass–Clover 
Mixtures—An Analysis of the Response to Management with the PROductive 
GRASsland Simulator (PROGRASS).” Ecological Modelling 220, no. 5 (March 10, 
2009): 703–24. 
[doi:10.1016/j.ecolmodel.2008.11.023](https://doi.org/10.1016/j.ecolmodel.2008.11.023).

[^10]: Graux, A. -I., M. Gaurut, J. Agabriel, R. Baumont, R. Delagarde, L. 
Delaby, and J. -F. Soussana. “Development of the Pasture Simulation Model for 
Assessing Livestock Production under Climate Change.” Agriculture, Ecosystems 
& Environment 144, no. 1 (November 1, 2011): 69–91. 
[doi:10.1016/j.agee.2011.07.001](https://doi.org/10.1016/j.agee.2011.07.001).



[^3]: If you make changes that generally improve `growR`, it would be great if you could 
share them to make them available to all future users. See [Contributing](#contributing).

