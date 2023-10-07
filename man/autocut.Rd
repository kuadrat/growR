\name{autocut}
\alias{autocut}
\title{autocut}
\description{
Simulation routine to realistically predict grass cutting events.
This follows an implementation described in:

Petersen, Krischan, David Kraus, Pierluigi Calanca, Mikhail A. 
Semenov, Klaus Butterbach-Bahl, and Ralf Kiese. “Dynamic Simulation 
of Management Events for Assessing Impacts of Climate Change on 
Pre-Alpine Grassland Productivity.” European Journal of Agronomy 
128 (August 1, 2021): 126306. 
https://doi.org/10.1016/j.eja.2021.126306.

The decision to cut is made based on two criteria.
First, it is checked whether a *target biomass* is reached on given 
DOY. The defined target depends on the DOY and is given through 
:func:`get_target_biomass`. If said biomass is present, return `TRUE`.

Otherwise, it is checked whether a given amount of time has passed 
since the last cut. Depending on whether this is the first cut of 
the season or not, the relevant parameters are 
:int:`last_DOY_for_initial_cut` and :int:`max_cut_period`.
If that amount of time has passed, return `TRUE`, otherwise return 
`FALSE`.
 
The target biomass for a given day is determined following the principles 
described in Petersen et al.

The exact regression for the target biomass is based on Fig. S2 in the 
supplementary material of Petersen et al.

A refinement to expected yield as function of altitude has been 
implemented according to Table 1a in

Huguenen-Elie et al. "Düngung von Grasland", Agrarforschung Schweiz, 
8, (6), 2017, 
https://www.agrarforschungschweiz.ch/2017/06/9-duengung-von-grasland-grud-2017/
}
\references{
Petersen, Krischan, David Kraus, Pierluigi Calanca, Mikhail A. 
Semenov, Klaus Butterbach-Bahl, and Ralf Kiese. “Dynamic Simulation 
of Management Events for Assessing Impacts of Climate Change on 
Pre-Alpine Grassland Productivity.” European Journal of Agronomy 
128 (August 1, 2021): 126306. 
https://doi.org/10.1016/j.eja.2021.126306.

Huguenen-Elie et al. "Düngung von Grasland", Agrarforschung Schweiz, 
8, (6), 2017, 
https://www.agrarforschungschweiz.ch/2017/06/9-duengung-von-grasland-grud-2017/
 

}
