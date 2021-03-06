% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary.R
\docType{data}
\name{dictionary}
\alias{dictionary}
\title{CSLS dictionary of sites}
\format{
A data frame with the following columns:
\describe{
\item{lake}{lake name, "Pleasant", "Long", or "Plainfield", factor}
\item{obs_type}{observation type, "LK" (lake), "GW" (groundwater), or
"P" (precipitation), factor}
\item{site_id}{unique site id for measurement site, e.g., LL-01, factor}
\item{SWIMS_station_name}{SWIMS station name, if exists for this site,
factor}
\item{SWIMS_station_id}{SWIMS station id, if exists for this site, integer}
\item{USGS_id}{USGS site number, corresponds to site_no in
"water_levels" data frame, numeric}
\item{WBIC}{water body identification code, for lake sites only, integer}
\item{site_type}{upgradient, downgradient, precipitation, lake,
deep, invalid, or NA, factor}
\item{site_type_detailed}{upgradient, downgradient,
precipitation, lake, deep, invalid,
inconsistent, typically_downradient, or
NA, factor}
\item{lat_deg}{latitude of the lake, decimal degrees, numeric}
\item{long_deg}{longitude of the lake, decimal degrees, numeric}
\item{elev_m}{approximate elevation of the lake, meters above mean sea
level, integer}
\item{bouy_bottom_elev_m}{elevation of the bottom of the lake at the
bouy location, meters above mean sea level,
numeric.}
}
}
\usage{
data(dictionary)
}
\description{
Dictionary for looking up alternate names and identification numbers for
sample sites in the CSLS.
}
\details{
Raw csv data is processed in the \code{data-raw/} subdirectory of this
project with the functions \code{import_dictionary.R} which is run by
\code{runall_cslsdata.R}. No cleaning or transformations are performed on raw
csv data beyond converting to .Rda.
}
\examples{
# Load dictionary
dictionary <- CSLSdata::dictionary

}
\keyword{datasets}
