#' Import lake rasters
#'
#' This function loads the lake rasters for CSLS lakes (as exported from ArcGIS)
#' into a list, with no transformations of the data. These rasters are clipped
#' to approximately the highest observed lake level (in 2019) plus a spatial
#' buffer of 20m.
#'
#' @return lake_raster, a list with the raster for each lake.

import_lake_rasters <- function() {

  library(raster)

  psnt_raster <- raster("data-raw/psnt_raster.tif")
  long_raster <- raster("data-raw/long_raster.tif")
  pfl_raster  <- raster("data-raw/pfl_raster.tif")

  lake_raster <- list(Pleasant = psnt_raster,
                      Long = long_raster,
                      Plainfield = pfl_raster)

  detach("package:raster", TRUE)

  return(lake_raster)
}
