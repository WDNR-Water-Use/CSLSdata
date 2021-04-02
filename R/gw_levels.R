#' CSLS daily groundwater levels
#'
#' Daily groundwater level observations for groundwater monitoring wells at the
#' CSLS lakes. Groundwater monitoring wells and sensors are maintained by the
#' Wisconsin State Geologic and Natural History Survey. Raw data are retrieved
#' from DNR Water Use section ArcGIS feature services for the Central Sands.
#' Water quantity information is summarized at a daily time step for these
#' measurements. The feature layer of interest is
#' \url{https://uadnrmaps.wi.gov/arcgis/rest/services/DG_HiCap/DG_CSLS_QUANT_MON_WTM_EXT/FeatureServer/2}.
#'
#' Raw csv data is processed in the \code{data-raw/} subdirectory of this
#' project with the functions \code{import_water_levels.R} and
#' \code{subset_gw_levels.R} which are run by \code{runall_cslsdata.R}.
#'
#' \code{import_water_levels.R} downloads the current feature layer from DNR
#' feature services with water quantity measurements in the Central Sands
#' (summarized at a daily time step). Feature layer of interest is DG_HiCap >
#' DG_CSLS_QUANT_MON_WTM_EXT > Layer 2 (W13101.WU_CSLS_QUANT_DATA). The only
#' cleaning performed is subsetting to desired parameters and adjusting datetime
#' formats.
#'
#' \code{subset_gw_levels.R} subsets the larger water level data frame by
#' USGS_id to retrieve only groundwater records for a given lake, then attaches
#' the CSLS site_id to the record. Also calculates the median 30-day differences
#' and uses this to define gradient of well on each day.
#'
#' @examples
#' gw_levels <- CSLSdata::gw_levels
#'
#' @seealso \code{\link{lake_levels}}
#'
#' @docType data
#'
#' @usage data(gw_levels)
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{site_id}{unique CSLS site id}
#'   \item{lake}{associated lake (Pleasant, Long, Plainfield)}
#'   \item{date}{date of measurement}
#'   \item{level_m}{gw elevation, meters above mean sea level}
#'   \item{lake_level_m}{lake elevation, meters above mean sea level}
#'   \item{diff_m}{difference between lake and gw elevation on this day (m)}
#'   \item{window_diff_m}{median difference over past 30 days}
#'   \item{site_type}{gradient classification based on median 30-day difference
#'                    (upgradient, downgradient, or no gradient)}
#' }
#'
#' @source \url{https://uadnrmaps.wi.gov/arcgis/rest/services/DG_HiCap/DG_CSLS_QUANT_MON_WTM_EXT/FeatureServer/2}
"gw_levels"
