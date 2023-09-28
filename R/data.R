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
#' @format A data.frame with 15 rows and 5 variables:
#' \describe{
#' \item{intensity}{Management intensity}
#' \item{altitude}{Altitude in m.a.s.l.}
#' \item{n_cuts}{Average number of cuts}
#' \item{yield}{Expected gross dry matter yield in t / ha}
#' \item{sigma_yield}{Uncertainty on yield in t / ha}
#' }
#'
"management_parameters"

#' Example Weather Data
#'
#' @description
#' Datasets containing the weather input parameters as used by rmodvege. The 
#' same data is made available as plain text files by the package and 
#' automatically found in the `input` directory created by [setup_directory()] 
#' if the `include_examples` option is set to `TRUE` (default).
#'
#' @details
#' For use in rmodvege, a WeatherData object has to be created from a plain 
#' text file. Therefore, this dataset is only provided for convenient 
#' inspection. In order to run rmodvege, use the plain text files provided by 
#' the package. Use `system.file("extdata", package = "rmodvege")` to locate them.
#'
#' The *snow* column is not actually used by rmodvege but rather calculated 
#' through precipitation and temperatures in [WeatherData$read_weather()].
#'
#' Likewise, the *rSSD* column is deprecated, currently unused and only kept 
#' for backwards compatibility.
#'
#' @family {weather_datasets}
#'
#' @seealso [setup_directory()]
#'
#' @format A data.frame with 3652 rows and 10 variables:
#'   \describe{
#'     \item{year}{Year as an integer}
#'     \item{DOY}{Day of year as an integer}
#'     \item{Ta}{Average temperature of the day in degree Celsius}
#'     \item{Tmin}{Minimum temperature of the day in degree Celsius}
#'     \item{Tmax}{Maximum temperature of the day in degree Celsius}
#'     \item{precip}{Daily precipitation in mm}
#'     \item{rSSD}{Relative sunshine duration in percent}
#'     \item{SRad}{Sun irradiance in J / s / m^2. This can be converted into 
#'           photosynthetically active radiation (PAR) in MJ / m^2 as:
#'           PAR = SRad * 0.47 * 24 * 60 * 60 / 1e6}
#'     \item{ET0}{Evapotranspiration in mm.}
#'     \item{snow}{Precipitation in the form of snow in mm}
#'   }
#'
"posieux_weather"

