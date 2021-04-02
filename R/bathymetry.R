#' CSLS bathymetry relationships
#'
#' Info from bathymetry data in ArcGIS for the lake including the elevation (m),
#' surface area (m^2), and volume (m^3) relationship. Calculated using the
#' Storage Capacity tool in Spatial Analyst Supplemental Tools. Also includes:
#'   * The average lake profile (horizontal distance from highest shoreline for
#'     each contour level, m), calculated from the lake elevation rasters.
#'   * The area of each plant community at each elevation.
#'   * The area of suitably hard substrate for centrarchid fish species at
#'     Pleasant Lake
#'
#' Raw csv data is processed in the \code{data-raw/} subdirectory of this
#' project with the function \code{import_bathymetry.R} which is run by
#' \code{runall_cslsdata.R}.
#'
#' \code{import_bathymetry.R} loads bathymetry data from ArcGIS for all lakes
#' including the elevation (m), surface area (m^2), and volume (m^3)
#' relationship. No cleaning is needed. It then calculates the average lake
#' profile from the lake rasters. Note that due to limitation in the raster
#' extent, Long Lake calculations begin ~0.5 ft below maximum elevation
#' available from ArcGIS elev/area/vol calculations. Next, it calculates the
#' estimated area of each plant community at each elevation given rules on plant
#' water depth limits (\code{import_plant_limits.R}). Lastly, it combines all of
#' this with additional information on the mean hardness of substrate accessible
#' to centrarchid fish species at this lake level
#' (\code{import_fish_substrate.R}).
#'
#' @examples
#' bathymetry <- CSLSdata::bathymetry
#'
#' @references \url{https://www.arcgis.com/home/item.html?id=3528bd72847c439f88190a137a1d0e67}
#'
#' @docType data
#'
#' @usage data(bathymetry)
#'
#' @format A data frame with the following columns:
#' \describe{
#'  \item{lake}{name of lake, i.e. "Pleasant", "Long", "Plainfield"}
#'  \item{elev_m}{elevation of lake surface above mean sea level (m)}
#'  \item{area_m2}{surface area of lake when it is at this elevation (m^2)}
#'  \item{vol_m3}{total volume of the lake when it is at this elevation (m^3)}
#'  \item{horiz_dist_m}{horizontal distance of this lake shoreline from highest
#'                      lake shoreline (that can be calculated in the raster
#'                      file) (m)}
#'  \item{plant_area_m2}{total area of lake used to calculate plant areal
#'                       extents (m^2)}
#'  \item{upland_m2}{area of upland plants at this lake at this elevation (m^2)}
#'  \item{inland_beach_m2}{area of inland_beach plants at this lake at this
#'                         elevation (m^2)}
#'  \item{emergent_m2}{area of emergent plants at this lake at this elevation
#'                     (m^2)}
#'  \item{floating_m2}{area of floating plants at this lake at this elevation
#'                     (m^2)}
#'  \item{submergent_weed_m2}{area of submergent_weed (Potamogeton) plants at
#'                            this lake at this elevation (m^2)}
#'  \item{submergent_algae_m2}{area of submergent_algae (Nitella) plants at this
#'                             lake at this elevation (m^2)}
#'  \item{submergent_m2}{area of submergent plants at this lake at this
#'                       elevation (m^2)}
#'  \item{upland_pcnt}{area of upland plants at this lake at this elevation (%
#'                     of lake outline)}
#'  \item{inland_beach_pcnt}{area of inland_beach plants at this lake at this
#'                         elevation (% of lake outline)}
#'  \item{emergent_pcnt}{area of emergent plants at this lake at this elevation
#'                     (% of lake outline)}
#'  \item{floating_pcnt}{area of floating plants at this lake at this elevation
#'                     (% of lake outline)}
#'  \item{submergent_weed_pcnt}{area of submergent_weed (Potamogeton) plants at
#'                            this lake at this elevation (% of lake outline)}
#'  \item{submergent_algae_pcnt}{area of submergent_algae (Nitella) plants at this
#'                             lake at this elevation (% of lake outline)}
#'  \item{submergent_pcnt}{area of submergent plants at this lake at this
#'                        elevation (% of lake outline)}
#'   \item{meanHrdAtNest}{approximate area of suitably hard substrate for
#'                            centrarchid fish at this elevation (% of lake
#'                            outline)}
#'  }
#'
"bathymetry"
