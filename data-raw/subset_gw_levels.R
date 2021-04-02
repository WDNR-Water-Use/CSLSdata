#' Subset Groundwater Levels
#'
#' This function subsets the larger water level data frame by USGS_id to
#' retrieve only groundwater records, then attaches the CSLS site_id to the
#' record, the lake level for the same day, and notes whether site is upgradient
#' or downgradient.
#'
#' @param water_levels a data frame with the date, site_no, obs_type, and
#'                     level_m of daily water level observations, as in the
#'                     water_levels dataset.
#' @param lake_levels a data frame with the lake, date, site_id, and level_m of
#'                    daily water level observations.
#' @param dictionary a data frame linking lake and well measurement sites to
#'                   other identifying characteristics. Used here: lake,
#'                   obs_type, USGS_id, site_id.
#'
#' @return gw_levels, a data frame with the following columns:
#'   \item{lake}{lake, e.g., "Pleasant", "Long", or "Plainfield"}
#'   \item{site_id}{unique site id for measurement site, e.g., LL-01}
#'   \item{date}{date of water level measurement}
#'   \item{level_m}{water level in meters above mean sea level}
#'   \item{lake_level_m}{water level of lake in meters above mean sea level}
#'   \item{site_type}{}

subset_gw_levels <- function(water_levels, lake_levels, dictionary,
                             min_diff = 0.01, window_days = 30,
                             lakes = c("Pleasant", "Long", "Plainfield")){
  # Load library
  library(dplyr)

  # Filter water levels to just lake groundwater sites
  gw_sites  <- dictionary %>%
               filter(obs_type == "GW",
                      !is.na(USGS_id)) %>%
               mutate(site_id = as.character(.data$site_id))
  gw_levels <- water_levels %>%
               filter(site_no %in% c(gw_sites$USGS_id),
                      obs_type == "GW")

  # Attach site id
  for (i in 1:nrow(gw_levels)) {
    site_no              <- gw_levels$site_no[i]
    gw_levels$site_id[i] <- as.character(gw_sites$site_id[gw_sites$USGS_id == site_no])
    gw_levels$lake[i]    <- as.character(gw_sites$lake[gw_sites$USGS_id == site_no])
  }
  gw_levels <- gw_levels %>%
               select(.data$lake, .data$site_id, .data$date, .data$level_m)

  # Merge with lake levels
  lake_levels <- lake_levels %>%
                 mutate_at("lake", as.character()) %>%
                 select(.data$lake, .data$date, .data$level_m)
  gw_levels <- merge(gw_levels,
                     lake_levels,
                     by = c("lake", "date"),
                     all.x = TRUE)
  colnames(gw_levels) <- c("lake", "date", "site_id", "level_m", "lake_level_m")

  # Calculate difference with lake levels
  gw_levels$diff_m <- gw_levels$level_m - gw_levels$lake_level_m
  this_date  <- min(gw_levels$date)
  end_date   <- max(gw_levels$date)
  new_levels <- NULL
  while (this_date <= end_date){
    this_window  <- interval(this_date - days(window_days), this_date)
    these_levels <- gw_levels %>%
                    filter(.data$date %within% this_window)
    median_diffs <- these_levels %>%
                    group_by(.data$site_id) %>%
                    summarise(window_diff_m = median(.data$diff_m,
                                       na.rm = TRUE)) %>%
                    ungroup()
    these_levels <- these_levels %>% filter(.data$date == this_date)
    these_levels <- merge(these_levels,
                          median_diffs,
                          by = "site_id", all.x = TRUE)
    new_levels   <- rbind(new_levels, these_levels)
    this_date    <- this_date + days(1)
  }
  gw_levels <- new_levels

  # Classify gradient based on min_diff
  gw_levels$site_type <- "nogradient"
  gw_levels$site_type[gw_levels$window_diff_m > min_diff]  <- "upgradient"
  gw_levels$site_type[gw_levels$window_diff_m < -min_diff] <- "downgradient"

  gw_levels$lake      <- factor(gw_levels$lake,
                                levels = lakes)
  gw_levels$site_id   <- factor(gw_levels$site_id)
  gw_levels$site_type <- factor(gw_levels$site_type)

  return(gw_levels)
}
