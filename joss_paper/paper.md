---
title: 'growR: R Implementation of the Vegetation Model ModVege'
tags:
  - R
  - agronomy
  - grasslands
  - agroecology
  - modelling
authors:
  - name: Kevin P. Kramer
    orcid: 0000-0001-5523-6924
    equal-contrib: true
    affiliation: 1
    corresponding: true # (This is how to denote the corresponding author)
  - name: Pierluigi Calanca
    orcid: 0000-0003-3113-2885
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Agroscope, Climate and Agriculture Group, Reckenholzstrasse 191, CH-8046 Zürich, Switzerland
   index: 1
date: 13 December 2023
bibliography: paper.bib

---

# Summary and Statement of Need

Grasslands constitute one of Earth's most widespread terrestrial ecosystems 
[@zhao2020GrasslandEcosystemServices] and managed grasslands are a core 
element in global agriculture, providing roughly half the feed inputs for 
global livestock systems [@herrero2013BiomassUseProduction].
Beside their contribution to global food production, they provide a catalogue 
of other ecosystem services, such as water flow and erosion regulation, 
pollination service, carbon sequestration and climate regulation 
[@zhao2020GrasslandEcosystemServices].
The latter have become particularly important in light of anthropogenic 
climate change [@IPCC2022Chapter05].

There is thus ample motivation to study the properties and dynamics of 
grasslands.
Mathematical models are widely used to assess climate change impacts on 
grassland functioning.
Additionally, such models can be employed in agricultural and political 
decision support, see e.g. [GrazPlan](https://grazplan.csiro.au/) 
[@moore1997GRAZPLANDecisionSupport].
Dozens of models have been formulated and tested in recent decades.
Each of these models has been created with different applications in mind 
and thus comes with its own focal points and a set of advantages and 
disadvantages.
To give just a few examples:

- The [Hurley Pasture Model](https://sites.massey.ac.nz/hurleypasturemodel/hurley-pasture-model/)[@thornley1998GrasslandDynamicsEcosystem]
  is a detailed mechanistic model for managed pastures.
- [BASGRA](https://github.com/davcam/BASGRA/) [@vanoijen2015BASGRA_2014] 
  and its descendant [BASGRA_N](https://github.com/MarcelVanOijen/BASGRA_N) 
  [@hoglind2020BASGRAModelGrassland] are multi-year grassland models which 
  include tiller dynamics.
- PROGRASS [@lazzarotto2009DynamicsGrassClover] was developed to capture the 
  interactions in grass/clover mixtures.
- The focus of PaSim [@graux2011DevelopmentPastureSimulation] is the 
  investigation of livestock production under climate change conditions.
- ModVege [@jouven2006ModelPredictingDynamics] is a mechanistic model 
  that is designed to capture the dominant processes with a minimum of 
  required input parameters.
- The Moorepark St Gilles [@ruelle2018DevelopmentMooreparkSt] and 
  Gras-sim [@kokah2023ModelingDailyDynamics] models both extend ModVege in 
  terms of soil water and nitrogen dynamics and management.

The existing grassland models vary not only in their formulation and 
structural complexity, but also in the manner in which they are implemented 
and distributed, ranging from sets of zipped script files being shared 
bilaterally among researchers to professionally developed and maintained 
(open or closed) software suites.
With this large variability in implemented models, version control, 
transparency and clear traceability of employed model implementations becomes 
challenging, which is detrimental for the reproduction of scientific results.

This paper describes the software package `growR`.
`growR` is an implementation of the vegetation model 
ModVege [@jouven2006ModelPredictingDynamics] in the `R` language 
[@rcoreteam2021LanguageEnvironmentStatistical].
It is packaged and distributed via the 
[comprehensive R archive network (CRAN)](https://cran.r-project.org/) with 
the [source code freely and openly 
available](https://github.com/kuadrat/growr) and thus presents a contribution 
to the above formulated need for reproducible practices in ecosystems modelling.

# Package Description

The origin of `growR` lies in an existing, unpublished `R` implementation of 
the same vegetation model.
This original code base has been used to simulate grass growth dynamics and 
the effects of drought in Switzerland [@calanca2016TestingAbilitySimple].
It has since been refactored into an `R` package which is currently being 
used to investigate the impacts of climate change on Swiss agriculture in the 
framework of the [National Center for Climate Services](https://www.nccs.admin.ch/nccs/de/home/klimawandel-und-auswirkungen/nccs-impacts.html)' 
*Impacts* program.

The `growR` package contains classes which define data structures and 
functionalities for parsing the model inputs, carrying out the grass growth 
simulations and providing different forms of output.
These classes and their functionalities are wrapped in high level functions 
which streamline the most common use cases.
In addition to this core functionality, the package contains utilities for 
some common tasks that arise in ecosystem modelling (and beyond), like setting 
up a clean directory structure, assessing model performance when compared to 
a set of validation data and carrying out sweeps over parameter space in 
order to aid model calibration.

## Model Extensions

The core model implementation follows the description by 
@jouven2006ModelPredictingDynamics but it contains a number of extensions 
that have proven valuable.
Use of any of these additions is optional, so the user is free to work with 
the model in its original formulation or with any combination of the provided 
extensions.
These additions include:

- Simulation of snow cover by use of a model by 
  @kokkonen2006ConstructionDegreedaySnow and 
  @rango1995RevisitingDegreeDayMethod, important when modelling grassland in 
  mountainous regions.
- A cut decision algorithm, which allows the model to simulate management 
  decisions in the absence of such input data. The decision process is based 
  on work by @hugueninElie2017DuengungGrasland and @petersen2021DynamicSimulationManagement.
- Plant responses to elevated CO~2~ conditions: The evapotranspiration 
  [@kruijt2008EffectsRisingAtmospheric] and photosynthetic rates 
  [@soltani2012ModelingPhysiologyCrop; @kellner2017CoupledHydrologicalplantGrowth] 
  of plants can be modified by the atmospheric CO~2~ concentration.
- Use of the multicriterial thermal definition of the growing season, as 
  proposed by @schaumberger2011RaeumlicheModelleZur.
- All model parameters default to the values provided by 
  @jouven2006ModelPredictingDynamics, but are accessible to adjustments by 
  the user.

Publications discussing and validating these extensions are in preparation.

# Conclusion

The `growR` package enhances the grassland modelling landscape with a model 
implementation complete with analysis tools and utilities.
The distribution as an `R` package on [CRAN](https://cran.r-project.org/) 
ensures an easy installation procedure and a relatively high standard of code 
quality and documentation through CRAN's submission policies.

# Acknowledgements

The work of K.P.K. has been supported by 
[Agroscope](https://www.agroscope.admin.ch/), the [National Center for 
Climate Sevices](https://www.nccs.admin.ch/nccs/de/home.html) and the 
[Federal Office for Agriculture](https://www.blw.admin.ch/blw/en/home.html).

# References

