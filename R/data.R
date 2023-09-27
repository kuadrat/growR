#' Parameters for expected yields in Switzerland
#'
#' @description
#' The dataset contains the parameters *a* and *b* used to model the expected 
#' gross dry matter yield (in t / ha) as a function of altitude (in m.a.s.l.) as
#' yield = a + b * altitude.
#'
#' @details
#' Lookup Table of expected yield as functions of height and management 
#' intensity after table 1a in Olivier Huguenin et al. 
#' Grundlagen für die Düngung landwirtschaftlicher Kulturen in der Schweiz 
#' (GRUD), Kapitel 9: Düngung von Grasland
#' ISBN 1663-7852
#' https://www.agrarforschungschweiz.ch/2017/06/9-duengung-von-grasland-grud-2017/
#'
#' @format A data.frame with 4 rows and 3 variables:
#' \describe{
#'    \item{intensity}{Management intensity}
#'    \item{a}{Offset *a* in t / ha}
#'    \item{b}{Slope *b* in t / ha / m}
#' }
#'
"yield_parameters"

#' Management practices for Swiss grasslands
#'
#' @description
#' Expected yields, uncertainties and average number of cuts as function of 
#' altitude and management intensity.
#'
#' @details
#' Data after table 1b in
#'
#' Olivier Huguenin et al. 
#' Grundlagen für die Düngung landwirtschaftlicher Kulturen in der Schweiz 
#' (GRUD), Kapitel 9: Düngung von Grasland
#' ISBN 1663-7852
#' https://www.agrarforschungschweiz.ch/2017/06/9-duengung-von-grasland-grud-2017/
#'
#' @format A data.frame with 15 rows and 5 variables
#' \describe{
#' \item{intensity}{Management intensity}
#' \item{altitude}{Altitude in m.a.s.l.}
#' \item{n_cuts}{Average number of cuts}
#' \item{yield}{Expected gross dry matter yield in t / ha}
#' \item{sigma_yield}{Uncertainty on yield in t / ha}
#' }
#'
"management_parameters"
