#' Import HOBO data
#'
#' This function loads in HOBO data for a lake, cleans up formatting and
#' handling of missing data, and rearranges so that information about the depth
#' of measurements and the parameter measured is treated as data, rather than
#' embedded in column names. It also merges depth information (either depth from
#' bottom or depth from top) with lake level information to get the elevation
#' above mean sea level of each sensor measurement. There is currently no
#' cleaning done to remove measurements when the sensor was out of the water for
#' downloads.
#'
#' @param dictionary a data frame linking lake and well measurement sites to
#'                   other identifying characteristics. Used here: lake,
#'                   obs_type, bouy_bottom_elev_m.
#' @param lake_levels a data frame with mean daily lake levels. Used here: date,
#'                    level_m
#' @param filename name of csv file with raw HOBO data
#' @param filedir name of subdirectory with csv file
#'
#' @return HOBO, a data frame with columns for date, param (ltmp_degC or
#'         DO_ppm), value, depth_m (depth from bottom or top of lake), depth_ref
#'         (whether depth is referenced from the bottom or the top of the lake)
#'         and elev_m. (elevation above mean sea level).

import_HOBO <- function(dictionary, lake_levels,
                        psnt = "HOBO_psnt_2019_11_15.csv",
                        long = "HOBO_long_2019_11_21.csv",
                        pfl = "HOBO_pfl_2019_12_13.csv",
                        filedir = "data-raw",
                        max_tmp_diff = 2){
  # Load libraries
  library(dplyr)
  library(reshape2)
  library(lubridate)
  library(stringr)

  filenames <- c(psnt, long, pfl)
  lakes     <- c("Pleasant", "Long", "Plainfield")

  HOBO_all <- NULL
  for (i in 1:length(lakes)) {
    filename <- filenames[i]
    lake     <- lakes[i]
    # Read in raw HOBO data
    HOBO      <- read.csv(sprintf("%s/%s", filedir, filename))
    HOBO      <- HOBO[,-1]  # nix the "#" column
    HOBO$FLAG <- NULL  # nix the FLAG column
    HOBO      <- HOBO[-c(1:6),] # nix the first few readings

    # Rename date & temperature columns
    date_col                 <-  which(!is.na(str_match(colnames(HOBO), "Date")))
    colnames(HOBO)[date_col] <- "date"
    colnames(HOBO)           <- colnames(HOBO) %>%
                                str_replace(".m.dfb",".B") %>%
                                str_replace(".m.dft",".T") %>%
                                str_replace("X","ltmp.")

    # Fix format of columns, update NA values
    HOBO$date           <- mdy_hm(HOBO$date)
    HOBO                <- HOBO %>%
                           mutate_if(is.factor, as.character) %>%
                           mutate_if(is.character, as.numeric) # force "#NA" to NA
    HOBO[HOBO < (-100)] <- NA # DO sometimes reads in as -888 for no value

    # Move data re: parameter and depth from colnames into data frame
    HOBO             <- melt(HOBO, id.vars = "date")
    HOBO$description <- HOBO$variable %>%
                        str_extract(regex("[:alpha:]")) %>%
                        tolower() %>%
                        str_replace("d","DISSOLVED OXYGEN HOBO") %>%
                        str_replace("l","TEMPERATURE HOBO")
    HOBO$depth_m     <- HOBO$variable %>%
                        str_replace("ltmp.","") %>%
                        str_replace(".B","") %>%
                        str_replace(".T","") %>%
                        str_replace("DO.","") %>%
                        as.numeric()
    HOBO$depth_ref    <- HOBO$variable %>%
                         str_replace("ltmp.","") %>%
                         str_replace("DO.","") %>%
                         str_extract_all(regex("[:alpha:]")) %>%
                         str_replace("T","top") %>%
                         str_replace("B","bottom")
    HOBO$variable      <- NULL
    HOBO$dnr_parameter <- NA
    HOBO$units         <- NA
    HOBO$dnr_parameter[HOBO$description == "DISSOLVED OXYGEN HOBO"] <- 300
    HOBO$units[HOBO$dnr_parameter == 300] <- "MG/L"
    HOBO$dnr_parameter[HOBO$description == "TEMPERATURE HOBO"] <- 10
    HOBO$units[HOBO$dnr_parameter == 10] <- "DEGREES C"

    HOBO$lake     <- lake
    HOBO_all      <- rbind(HOBO_all, HOBO)
  }

  HOBO <- HOBO_all %>%
          select(lake = .data$lake,
                 date = .data$date,
                 dnr_parameter = .data$dnr_parameter,
                 description = .data$description,
                 result = .data$value,
                 units = .data$units,
                 depth_m = .data$depth_m,
                 depth_ref = .data$depth_ref)

  # Merge with lake levels
  HOBO$floor_date <- floor_date(HOBO$date, unit = "day")
  HOBO            <- merge(HOBO,
                     select(lake_levels, c("lake", "site_id", "date", "level_m")),
                     by.x = c("lake", "floor_date"),
                     by.y = c("lake", "date"),
                     all.x = TRUE)
  bottom          <- dictionary %>%
                     filter(.data$obs_type == "LK") %>%
                     select(lake = .data$lake,
                            bottom_m = .data$bouy_bottom_elev_m,
                            site_type = .data$site_type)
  HOBO            <- merge(HOBO,
                           bottom,
                           by = "lake",
                           all.x = TRUE)
  HOBO$elev_m <- NA
  HOBO$elev_m[HOBO$depth_ref == "bottom"]  <- HOBO$depth_m[HOBO$depth_ref == "bottom"] +
                                              HOBO$bottom_m[HOBO$depth_ref == "bottom"]
  HOBO$depth_m[HOBO$depth_ref == "bottom"] <- HOBO$level_m[HOBO$depth_ref == "bottom"] -
                                              HOBO$elev_m[HOBO$depth_ref == "bottom"]
  HOBO$elev_m[HOBO$depth_ref == "top"]     <- HOBO$level_m[HOBO$depth_ref == "top"] -
                                              HOBO$depth_m[HOBO$depth_ref == "top"]

  HOBO <- HOBO %>%
          select(date = .data$date,
                 lake = .data$lake,
                 site_id = .data$site_id,
                 dnr_parameter = .data$dnr_parameter,
                 description = .data$description,
                 result = .data$result,
                 units = .data$units,
                 depth1_m = .data$depth_m,
                 site_type = .data$site_type)
  HOBO$flag <- "NONE"

  # # Quality control - remove records where change in temp from previous time > 2oC
  # nix_dates <- HOBO %>%
  #              filter(.data$param == "ltmp_degC") %>%
  #              arrange(.data$depth_m, .data$date) %>%
  #              mutate(ltmp_prev = lag(.data$value)) %>%
  #              mutate(ltmp_diff = abs(.data$ltmp_prev - .data$value)) %>%
  #              filter(.data$ltmp_diff >= max_tmp_diff &
  #                       .data$date > min(.data$date)) %>%
  #              select(.data$date) %>%
  #              unique() %>%
  #              unlist()
  # if (length(nix_dates) > 0){
  #   HOBO <- HOBO[-which(HOBO$date %in% nix_dates), ]
  # }

  return(HOBO)
}
