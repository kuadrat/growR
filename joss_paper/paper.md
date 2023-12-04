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
anthropogenic climate change [@IPCC-2002-Chapter05].

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

- [The Hurley Pasture Model](@thornley1997TemperateGrasslandResponses) is a 
  rather complete and detailed mechanistic model for managed pastures.
- [RothC](https://www.rothamsted.ac.uk/rothamsted-carbon-model-rothc) was 
  developed for the long-term 
  [Rothamsted Parkgrass Experiment](@jenkinson1994TrendsHerbageYields) and 
  thus uses comparatively long time scales with a focus on the carbon balance.
- [ModVege](@jouven2006ModelPredictingDynamics) is another mechanistic model 
  that is designed to capture the dominant processes with a minimum of 
  required input parameters.
- [The Moorepark St Gilles](@ruelle2018DevelopmentMooreparkSt) and 
  [Gras-sim](@kokah2023ModelingDailyDynamics) models both extend 
  [ModVege](@jouven2006ModelPredictingDynamics) by adding more processes and 
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


# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements


# References

