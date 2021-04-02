#' CSLS Water Chemistry data
#'
#' All water chemistry data associated with the Central Sands Lakes Study from:
#'   * The Wisconsin Department of Natural Resources (DNR) Surface Water
#'     Integrated Management System (SWIMS), project "Central Sands Lake Study"
#'     or "csls".
#'   * The National Atmospheric Deposition Program (NADP) precipitation
#'     chemistry data from Devil's Lake.
#'   * Isotope measurements
#'   * HOBO continuous logger measurements
#'
#' Raw csv data is processed in the \code{data-raw/} subdirectory of this
#' project with the functions \code{import_isotopes}, \code{import_NADP},
#' \code{import_SWIMS.R}, \code{import_HOBO}, and \code{combine_chem} which are
#' run by \code{runall_cslsdata.R}.
#'
#' \code{import_isotopes} loads isotope measurements for the CSLS lakes and
#' extracts isotopic measurements for 18O and 2H (duterium) flagged as VALID =
#' TRUE.
#'
#' \code{import_NADP} loads in NADP data and cleans with the following steps:
#' 1. Defines SWIMS equivalent of NADP parameters of interest
#' 2. Limits only valid data to the desired site (default: Devil's Lake).
#' 3. Rearranges data frame for parameters of interest
#' 4. Retains any LOD flags, eliminates clearly invalid results (< 0)
#' 5. Converts NADP param names to SWIMS equivalents
#' 6. Add units, date, and site_id
#'
#' \code{import_SWIMS.R} loads in SWIMS data associated with the Central Sands Lakes
#' Study (project: "csls") and cleans with the following steps:
#' 1. Transforms column names to characters that are easier to work with in R.
#' 2. Imports and appends LDES data, if using.
#' 3. Uses station_name and WBIC to match samples to CSLS site ids. Makes sure
#'    all samples have a CSLS site_id match.
#' 4. Extracts and parses sample depth or sample depth range, where exists.
#' 5. Updates column classes (to datetime, numeric, etc.)
#' 6. Performs QC on results and makes note of any flagged samples
#' 7. Subsets to useful columns and returns SWIMS data frame.
#'
#' \code{import_HOBO.R} loads in HOBO data for a lake, cleans up formatting and
#' handling of missing data, and rearranges so that information about the depth
#' of measurements and the parameter measured is treated as data, rather than
#' embedded in column names. It also merges depth information (either depth from
#' bottom or depth from top) with lake level information to get the elevation
#' above mean sea level of each sensor measurement. There is currently no
#' cleaning done to remove measurements when the sensor was out of the water for
#' downloads.
#'
#' \code{combine_chem.R} combines all water chemistry data frames, using the
#' dictionary and gw_levels to define site types.
#'
#' @examples
#' water_chem <- CSLSdata::water_chem
#'
#' @docType data
#'
#' @usage data(water_chem)
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{lake}{name of "lake", either "Pleasant", "Long", "Plainfield", or
#'               "Precip}
#'   \item{site_type}{type of site, e.g. "lake", "precipitation", "upgradient",
#'                    "nogradient", "downgradient", "deep"}
#'   \item{site_id}{unique id for CSLS site}
#'   \item{date}{date of measurement}
#'   \item{dnr_parameter}{DNR parameter code for analyte measured}
#'   \item{description}{Description of analyte measured}
#'   \item{result}{value of sample result}
#'   \item{units}{units of sample result}
#'   \item{depth1_m}{sample depth (m)}
#'   \item{depth2_m}{if sample depth is a range, deepest end of that range (m)}
#'   \item{lod}{limit of detection for lab method}
#'   \item{loq}{limit of quality for lab method}
#'   \item{flag}{notes if data is flagged for being under the LOD ("LOD"), under
#'              the LOQ ("LOQ"), analyzed past the holding date ("AGE"),
#'              duplicate sample ("DUPLICATE"), blank sample ("BLANK"), other
#'              reason ("COMMENT"), bad well ("BAD_WELL") or not flagged ("NONE")}
#'   \item{flag_reason}{longer comment about reason for flag}
#' }
#'
#' @source \url{https://dnr.wi.gov/topic/surfacewater/swims/}
#' @source \url{http://nadp.slh.wisc.edu/data/sites/list/?net=NTN}
"water_chem"
