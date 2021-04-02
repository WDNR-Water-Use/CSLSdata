#' CSLS MODFLOW model results
#'
#' Monthly lake water budget and water level data for each study lake from
#' USGS-led CSLS MODFLOW simulations.
#'
#' Raw data is processed in with the function \code{import_model_results.R} in the
#' \code{data-raw} subdirectory of this project. This function loads model outputs
#' from csv files and saves as a combined data frame with new columns for the
#' lake and simulation type.
#'
#' @examples
#' MODFLOW <- CSLSdata::MODFLOW
#'
#' @docType data
#'
#' @usage data(MODFLOW)
#'
#' @format A data frame with the following columns:
#' \describe{
#'  \item{scenario}{type of model simulation, e.g. "no_irr", or "cur_irr"}
#'  \item{sim}{simulation number from Monte Carlo runs}
#'  \item{lake}{name of lake, i.e. "Pleasant", "Long", "Plainfield"}
#'  \item{date}{date of output, POSIXct}
#'  \item{level_m}{mean lake stage for the month, mamsl}
#'  \item{vol_m3}{mean lake volume for the month, (m^3)}
#'  \item{P_m3}{precipitation volume this month (m^3)}
#'  \item{E_m3}{evaporation volume this month (m^3)}
#'  \item{GWin_m3}{groundwater inflow volume this month (m^3)}
#'  \item{GWout_m3}{groundwater outflow volume this month (m^3)}
#'  \item{dV_m3}{change in lake volume this month (m^3)}
#'  \item{P_m3_d}{precipitation flow rate (m^3/d)}
#'  \item{E_m3_d}{evaporation flow rate (m^3/d)}
#'  \item{GWin_m3_d}{groundwater inflow rate into lake (m^3/d)}
#'  \item{GWout_m3_d}{groundwater outflow rate into lake (m^3/d)}
#'  \item{dV_m3_d}{change in lake volume as a flow rate (m^3/d)}
#'  }
#'
"MODFLOW"
