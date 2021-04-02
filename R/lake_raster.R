#' CSLS lake rasters
#'
#' Lake elevation rasters for each lake as exported from ArcGIS. Elevation
#' information is a combination of Biobase sonar data (Pleasant) or
#' point-intercept surveys (Long and Plainfield) plus LiDAR, for elevations
#' above the lake level elevation on the date of each survey.
#'
#' Raw raster data is processed in the \code{data-raw/} subdirectory of this
#' project with the function \code{import_lake_rasters.R} which is run by
#' \code{runall_cslsdata.R}.
#'
#' \code{import_lake_rasters.R} loads the lake rasters for CSLS lakes (as
#' exported from ArcGIS) into a list, with no transformations of the data. These
#' rasters are clipped to approximately the highest observed lake level (in
#' 2019) plus a spatial buffer of 20m.
#'
#' @examples
#' lake_raster <- CSLSdata::lake_raster
#'
#' @docType data
#'
#' @usage data(lake_raster)
#'
#' @format A list with the following columns:
#' \describe{
#'  \item{Pleasant}{elevation raster for Pleasant Lake (m)}
#'  \item{Long}{elevation raster for Long Lake (m)}
#'  \item{Plainfield}{elevation raster for Plainfield Lake (m)}
#'  }
#'
"lake_raster"
