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
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Agroscope, Federal Department of Economic Affairs, Education and Research, Switzerland
   index: 1
date: 13 December 2023
bibliography: paper.bib

---

# Summary and Statement of Need

Grasslands constitute one of Earth's most widespread terrestrial ecosystems 
[@zhao2020GrasslandEcosystemServices] and managed grasslands are a core 
element in global agriculture, providing roughly half the feed inputs for 
global livestock systems [@herrero2013BiomassUseProduction].
Beside their importance for global food production, they provide a catalogue 
of other ecosystem services, such as water flow and erosion regulation, 
pollination service, carbon sequestration and climate regulation 
[@zhao2020GrasslandEcosystemServices].
The latter two examples have radically grown in urgency in light of 
anthropogenic climate change [@IPCC2002Chapter05].

There is thus ample motivation to study the properties and dynamics of 
grasslands.
Experimental approaches, while of fundamental importance, suffer from high 
costs in time resources required, as a study site typically has to be maintained 
over several year in order to make a scientific observation.
For this reason, the method of investigating grassland dynamics by means of 
mathematical models and simulationist approaches has found widespread 
application, with dozens of models being formulated, employed and further 
developed.
Each of these models has been developed with different applications in mind 
and thus comes with its own focal points and a set of advantages and 
disadvantages.
To give just a few examples:

- [The Hurley Pasture Model](thornley1997TemperateGrasslandResponses) is a 
  rather complete and detailed mechanistic model for managed pastures.
- [RothC](https://www.rothamsted.ac.uk/rothamsted-carbon-model-rothc) was 
  developed for the long-term 
  [Rothamsted Parkgrass Experiment](jenkinson1994TrendsHerbageYields) and 
  thus uses comparatively long time scales with a focus on the carbon balance.
- [ModVege](jouven2006ModelPredictingDynamics) is another mechanistic model 
  that is designed to capture the dominant processes with a minimum of 
  required input parameters.
- [The Moorepark St Gilles](ruelle2018DevelopmentMooreparkSt) and 
  [Gras-sim](kokah2023ModelingDailyDynamics) models both extend 
  [ModVege](jouven2006ModelPredictingDynamics) by adding more processes and 
  therefore complexity.

The different existing grassland models vary in their formulation, but also 
in the way they are implemented.
With this large variability in models, model implementations and 
implementation versions, transparency and becomes critical for the 
reproduction of scientific results.

This paper describes the software package `growR`.
`growR` is an implementation of the vegetation model 
[ModVege](@jouven2006ModelPredictingDynamics) in the `R` language 
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
functionalities for parsing the model inputs, carrying out the simulation 
grass growth simulations and providing different forms of output.
These classes and their functionalities are wrapped in high level functions 
which streamline the most common use cases.
In addition to this core functionality, the package contains utilities for 
some common tasks that arise in ecosystem modeling, like setting up a clean 
directory structure, assessing model performance when compared to a set of 
validation data and carrying out sweeps over parameter space in order to aid 
model calibration.

## Model Extensions

The core model implementation follows the description by 
@jouven2006ModelPredictingDynamics but it contains a number of extensions 
that have proven to be useful.
Use of all of these extensions is completely optional, so the user is free to 
work with the model in its original formulation or with any combination of 
the provided extensions.
These additions include:

- A cut decision algorithm, which allows the model to simulate management 
  decisions in the absence of such input data. The decision process is based 
  on work by @petersen2021DynamicSimulationManagement and 
  @huguenin_elie2017DuengungGrasland.
- Plant responses to elevated CO~2~ conditions. The evapotranspiration 
  [@kruijt2008EffectsRisingAtmospheric] and photosynthetic rates 
  [@soltani2012ModelingPhysiologyCrop, @kellner2017CoupledHydrologicalplantGrowth] 
  of plants can be modified by the atmospheric CO~2~ concentration.
- The multicriterial thermal definition as described in Chapter 2.3.1.3 of 
  @schaumberger2011RaeumlicheModelleZur is used in order to determine the 
  start of the growing season.
- All model parameters default to the values provided by 
  @jouven2006ModelPredictingDynamics, but are accessible to adjustments by 
  the user.

# Acknowledgements


# References

