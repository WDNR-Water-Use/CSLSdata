#' Subset Lake Levels
#'
#' Subsets larger water level data frame by USGS_id to retrieve only lake level
#' records.
#'
#' @param water_levels a data frame with the date, site_no, obs_type, and
#'                     level_m of daily water level observations, as in the
#'                     water_levels dataset.
#' @param dictionary a data frame linking lake and well measurement sites to
#'                   other identifying characteristics. Used here: lake,
#'                   obs_type, and USGS_id.
#' @param bathymetry elevation (m), area (m^2), and volume (m^3) relationship
#'                   for each lake
#'
#' @return lake_levels, a data frame with the following columns:
#'   \item{lake}{lake, e.g., "Pleasant", "Long", or "Plainfield"}
#'   \item{site_id}{site id, e.g., "PLEAS", "LONG", or "PLAIN"}
#'   \item{date}{date of water level measurement}
#'   \item{level_m}{water level in meters above mean sea level}
#'   \item{area_m2}{area of lake at this elevation}
#'   \item{vol_m3}{volume of lake at this elevation}

subset_lake_levels <- function(water_levels, dictionary, bathymetry,
                               lakes = c("Pleasant", "Long", "Plainfield")){
  # Load libraries
  library(dplyr)
  library(bit64)
  library(stringr)

  # Filter out lake levels
  USGS_id         <- dictionary %>%
                     filter(obs_type == "LK") %>%
                     select(.data$lake, .data$USGS_id, .data$site_id)
  USGS_id$USGS_id <- as.integer64(as.numeric(USGS_id$USGS_id))
  lake_levels     <- water_levels %>%
                     filter(as.character(as.numeric(site_no)) %in%
                              as.character(USGS_id$USGS_id),
                            obs_type == "LK")
  lake_levels$lake    <- as.character(as.numeric(lake_levels$site_no))
  lake_levels$site_id <- as.character(as.numeric(lake_levels$site_no))
  for (i in 1:nrow(USGS_id)) {
    lake_levels$lake    <- str_replace_all(lake_levels$lake,
                                           as.character(USGS_id$USGS_id[i]),
                                           as.character(USGS_id$lake[i]))
    lake_levels$site_id <- str_replace_all(lake_levels$site_id,
                                           as.character(USGS_id$USGS_id[i]),
                                           as.character(USGS_id$site_id[i]))
  }
  lake_levels <- lake_levels %>%
                 select(.data$lake, .data$site_id, .data$date, .data$level_m)

  new_levels <- NULL
  for (lake in unique(bathymetry$lake)) {
    this_bathymetry <- bathymetry %>% filter(.data$lake == !!lake)
    f_elev_area     <- approxfun(x = this_bathymetry$elev_m,
                                 y = this_bathymetry$area_m2)
    f_elev_vol      <- approxfun(x = this_bathymetry$elev_m,
                                 y = this_bathymetry$vol_m3)
    these_levels    <- lake_levels %>%
                       filter(.data$lake == !!lake) %>%
                       mutate(area_m2 = f_elev_area(.data$level_m),
                              vol_m3 = f_elev_vol(.data$level_m))
    new_levels <- rbind(new_levels, these_levels)
  }

  lake_levels <- new_levels
  lake_levels$lake    <- factor(lake_levels$lake,
                                levels = lakes)
  lake_levels$site_id <- factor(lake_levels$site_id)

  return(lake_levels)
}
