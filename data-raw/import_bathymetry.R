#' Import Bathymetry Information
#'
#' This funcition loads bathymetry data from ArcGIS for the lake including the
#' elevation (m), surface area (m^2), and volume (m^3) relationship. No cleaning
#' is needed.
#'
#' It then calculates the average lake profile from the lake rasters. Note that
#' due to limitation in the raster extent, Long Lake calculations begin ~0.5 ft
#' below maximum elevation available from ArcGIS elev/area/vol calculations.
#'
#' Lastly, it combines all of this with additional information on the mean
#' hardness of substrate accessible to centrarchid fish species at this lake
#' level.
#'
#' @param filename name of csv file with bathymetry info exported from ArcGIS
#' @param filedir name of subdirectory with csv file
#'
#' @return bathymetry, a data frame with the following columns:
#'  \item{lake}{name of lake, i.e. "Pleasant", "Long", "Plainfield"}
#'  \item{elev_m}{elevation of lake surface above mean sea level (m)}
#'  \item{area_m2}{surface area of lake when it is at this elevation (m^2)}
#'  \item{vol_m3}{total volume of the lake when it is at this elevation (m^3)}
#'  \item{horiz_dist_m}{horizontal distance of this lake shoreline from highest
#'                      lake shoreline (that can be calculated in the raster
#'                      file) (m)}
#'  \item{plant_area_m2}{total area of lake used to calculate plant areal extents
#'                       (m^2)}
#'  \item{upland}{area of upland plants at this lake at this elevation (m^2)}
#'  \item{inland_beach}{area of inland_beach plants at this lake at this
#'                      elevation (m^2)}
#'  \item{emergent}{area of emergent plants at this lake at this elevation
#'                  (m^2)}
#'  \item{floating}{area of floating plants at this lake at this elevation
#'                  (m^2)}
#'  \item{submergent_weed}{area of submergent_weed (Potamogeton) plants at this
#'                         lake at this elevation (m^2)}
#'  \item{submergent_algae}{area of submergent_algae (Nitella) plants at this
#'                          lake at this elevation (m^2)}
#'  \item{submergent}{area of submergent plants at this lake at this elevation
#'                    (m^2)}
#'   \item{meanSubHrd}{mean substrate hardness at this elevation}

import_bathymetry <- function(filename,
                              lake_raster,
                              plant_limits,
                              fish_substrate,
                              filedir = "data-raw",
                              lakes = c("Pleasant", "Long", "Plainfield")) {
  library(raster)
  library(sf)
  library(dplyr)

  # Elevation, area, volume from ArcGIS ----------------------------------------
  bathymetry <- read.csv(sprintf("%s/%s", filedir, filename))

  # AVERAGE LAKE PROFILES ------------------------------------------------------
  # Think of the contour rings like rectangles (or more closely, trapezoids)
  # that are bent to form a ring. The average width of these bendy
  # trapezoids/rings is the area of the ring divided by the average length of
  # the two countour lines that form the bendy trapezoid/ring.
  depth_profile <- NULL
  for (lake in lakes) {
    this_raster     <- lake_raster[[lake]]
    this_levels     <- bathymetry$elev_m[bathymetry$lake == lake]
    contours        <- rasterToContour(this_raster, levels = this_levels)
    contour_lines   <- st_as_sf(contours)
    contour_areas   <- st_polygonize(contour_lines)
    this_area_m2    <- st_area(contour_areas)
    this_length_m   <- st_length(contour_lines)
    this_profile    <- data.frame(elev_m = as.numeric(contour_lines$level),
                                  area_m2 = as.numeric(this_area_m2),
                                  length_m = as.numeric(this_length_m)) %>%
                       filter(.data$area_m2 > 0) %>%
                       arrange(desc(.data$elev_m)) %>%
                       mutate(ring_area_m2 = lag(.data$area_m2) - .data$area_m2,
                              avg_length_m = 0.5*(lag(.data$length_m) +
                                                    .data$length_m),
                              avg_dist_m = .data$ring_area_m2/
                                           .data$avg_length_m)
    this_profile    <- this_profile %>%
                       mutate(avg_dist_m = ifelse(is.na(.data$avg_dist_m),
                                                    0, .data$avg_dist_m),
                              horiz_dist_m = cumsum(.data$avg_dist_m),
                              lake = !!lake) %>%
                       dplyr::select(.data$lake, .data$elev_m,
                                     .data$horiz_dist_m)
    depth_profile   <- bind_rows(depth_profile, this_profile)
  }

  # PLANT AREAS ----------------------------------------------------------------
  # Given rules about water depth tolerance for plant communities, define area
  plant_areas <- NULL
  for (lake in lakes) {
    these_limits <- plant_limits %>% filter(.data$lake == !!lake)
    # Maximum levels (to the nearest cm) that result in nice contours that don't
    # mess up the calculate_veg_area function
    max_elev     <- switch(lake,
                           "Plainfield" = 337.0,
                           "Long" = 336.70,
                           "Pleasant" = 299.70)
    elev_area    <- bathymetry %>%
                    filter(.data$lake == !!lake,
                           .data$elev_m <= max_elev) %>%
                    arrange(desc(.data$elev_m)) %>%
                    mutate(inc_area = .data$area_m2 - lead(.data$area_m2)) %>%
                    dplyr::select(lake = .data$lake,
                                  elev_m = .data$elev_m,
                                  area = .data$area_m2,
                                  inc_area = .data$inc_area)
    min_elev     <- min(elev_area$elev_m)
    max_elev     <- max(elev_area$elev_m)

    # Vegetation elevations: lower and upper extent of class
    upper <- data.frame(lake = elev_area$elev_m)
    lower <- data.frame(lake = elev_area$elev_m)

    for (plant in these_limits$variable) {
      this_limits   <- these_limits %>% filter(.data$variable == plant)
      upper[,plant] <- upper$lake - this_limits$shallow
      lower[,plant] <- lower$lake - this_limits$deep
    }
    upper[upper > max_elev] <- max_elev
    upper[upper < min_elev] <- NA
    lower[lower > max_elev] <- max_elev
    lower[lower < min_elev] <- min_elev
    lower[is.na(upper)]     <- NA

    # Vegetation areas: sum incremental areas within elevation limits
    areas <- data.frame(lake = elev_area$lake,
                        elev_m = elev_area$elev_m,
                        plant_area_m2 = max(elev_area$area))
    for (plant in these_limits$variable) {
      for (i in 1:nrow(areas)) {
        area_subset <- elev_area %>%
                       filter(.data$elev_m >= lower[i,plant],
                              .data$elev_m <= upper[i,plant])
        area_name <- sprintf("%s_m2", plant)
        pcnt_name <- sprintf("%s_pcnt", plant)
        areas[i, area_name] <- sum(area_subset$inc_area, na.rm = TRUE)
        areas[i, pcnt_name] <- 100*areas[i, area_name]/areas[i, "plant_area_m2"]
      }
    }

    # Combine vegetation areas for all lakes
    plant_areas <- bind_rows(plant_areas, areas)
  }


  # COMBINE --------------------------------------------------------------------
  bathymetry <- left_join(bathymetry,
                          depth_profile,
                          by = c("lake", "elev_m")) %>%
                left_join(plant_areas,
                          by = c("lake", "elev_m")) %>%
                left_join(fish_substrate,
                          by = c("lake", "elev_m"))
  bathymetry$lake <- factor(bathymetry$lake,
                            levels = lakes)
  bathymetry <- bathymetry %>%
                arrange(.data$lake, .data$elev_m)

  detach("package:raster", TRUE)

  return(bathymetry)
}
