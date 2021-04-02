#' Import CSLS Daily Water Levels
#'
#' This function downloads the current feature layer from DNR feature services
#' with water quantity measurements in the Central Sands (summarized at a daily
#' time step). Feature layer of interest is DG_HiCap > DG_CSLS_QUANT_MON_WTM_EXT
#' > Layer 2 (W13101.WU_CSLS_QUANT_DATA). The only
#' cleaning performed is subsetting to desired parameters and adjusting datetime
#' formats.
#'
#' @return water_levels, a data frame with the following columns
#'   \item{date}{date of water level measurement}
#'   \item{site_no}{UGSG site number}
#'   \item{obs_type}{type of observation (LK = lake level, GW = groundwater
#'                 level)}
#'   \item{level_m}{water level in meters above mean sea level}

import_water_levels <- function() {
  # Load libraries
  library(jsonlite)
  library(lubridate)
  library(dplyr)

  # Download water level data
  levels_url  <- "https://uadnrmaps.wi.gov/arcgis/rest/services/DG_HiCap/DG_CSLS_QUANT_MON_WTM_EXT/FeatureServer/2/query?f=pjson&where=1=1&outfields=*"
  water_levels <- fromJSON(levels_url)
  water_levels <- as.data.frame(water_levels$features$attributes)
  water_levels <- water_levels %>%
                  mutate(date = as_datetime(OBS_DATE/1000))
  water_levels <- water_levels %>%
                  filter(OBS_TYPE %in% c("LK","GW")) %>%
                  select(date, SITE_NO, SITE_NAME, OBS_TYPE, OBS)

  colnames(water_levels) <- c("date", "site_no", "site_name", "obs_type","level_m")

  return(water_levels)
}
