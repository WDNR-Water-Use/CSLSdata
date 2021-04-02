#' CSLS plant water depth limits
#'
#' Water depth limits for all plant communities present in the CSLS lakes.
#'
#' Raw csv data is processed in the \code{data-raw/} subdirectory of this
#' project with the function \code{import_plant_limits.R} which is run by
#' \code{runall_cslsdata.R}.
#'
#' \code{import_plant_limits} loads the rules for plant community water depth
#' limits for the CSLS lakes and transforms depths from feet to meters.
#'
#' @examples
#' plant_limits <- CSLSdata::plant_limits
#'
#' @seealso \code{\link{bathymetry}}
#'
#' @docType data
#'
#' @usage data(plant_limits)
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{lake}{lake, e.g., "Pleasant", "Long", or "Plainfield"}
#'   \item{variable}{plant community, e.g. "upland" or "inland_beach"}
#'   \item{shallow}{the shallowest water depth this plant community can tolerate
#'                  (m)}
#'   \item{deep}{the deepest water depth this plant community can tolerate (m)}
#' }
#'
"plant_limits"
