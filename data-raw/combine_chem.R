#' Combine water chemistry data
#'
#' This function combines all water chemistry data frames, using the dictionary
#' and gw_levels to define site types.
#'
#' @param SWIMS
#' @param NADP
#' @param isotopes
#' @param HOBO
#' @param gw_levels
#' @param dictionary
#' @param lakes
#'
#' @return water_chem, a data frame with the following columns:
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
#'
#' @export
combine_chem <- function(SWIMS, NADP, isotopes, HOBO, gw_levels, dictionary,
                         lakes = c("Pleasant", "Long", "Plainfield", "Precip")) {

  # Load Libraries -------------------------------------------------------------
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(NISTunits)

  # Make data combinable and combine -------------------------------------------
  SWIMS      <- SWIMS %>%
                mutate_at(c("dnr_parameter", "description", "units", "site_id", "flag"),
                          as.character)
  NADP       <- NADP %>%
                mutate_at(c("result"), as.character)
  isotopes   <- isotopes %>%
                mutate_at(c("description", "result"), as.character)
  HOBO       <- HOBO %>%
                mutate_at(c("dnr_parameter", "result", "site_type"), as.character)
  water_chem <- bind_rows(SWIMS, NADP, isotopes)

  # Define lakes & site types --------------------------------------------------
  water_chem$lake      <- water_chem$site_id
  water_chem$site_type <- water_chem$site_id
  for (i in 1:nrow(dictionary)) {
  water_chem$lake      <- str_replace_all(water_chem$lake,
                                          paste0(as.character(dictionary$site_id[i]),"$"),
                                          as.character(dictionary$lake[i]))
  water_chem$site_type <- str_replace_all(water_chem$site_type,
                                          paste0(as.character(dictionary$site_id[i]),"$"),
                                          as.character(dictionary$site_type[i]))
  }

  # Get specific about groundwater (up/down/no gradient)
  gw_levels              <- gw_levels %>%
                            mutate_at(c("site_id", "site_type"), as.character) %>%
                            select(.data$site_id, .data$date, .data$site_type)
  water_chem$floor_date  <- floor_date(water_chem$date, unit = "day")
  water_chem             <- merge(water_chem,
                                  gw_levels,
                                  by.x = c("floor_date", "site_id"),
                                  by.y = c("date", "site_id"),
                                  all.x = TRUE)
  use_gw_type            <- which(!is.na(water_chem$site_type.y))
  water_chem$site_type.x[use_gw_type] <- water_chem$site_type.y[use_gw_type]
  water_chem$site_type   <- water_chem$site_type.x
  water_chem$site_type.x <- NULL
  water_chem$site_type.y <- NULL
  water_chem$floor_date  <- NULL

  # Add in HOBO data -----------------------------------------------------------
  water_chem <- bind_rows(water_chem, HOBO)

  # QUALITY CONTROL ------------------------------------------------------------
  # Flag bad well(s)
  invalid_ids <- dictionary %>% filter(.data$site_type == "invalid")
  invalid_ids <- as.character(invalid_ids$site_id)
  water_chem$flag[water_chem$site_id %in% invalid_ids] <- "BAD_WELL"
  water_chem$flag[water_chem$site_id == "PFL-09"]      <- "BAD_WELL"

  # Fix FT vs. FEET mismatch
  water_chem$units[water_chem$units == "FT"] <- "FEET"

  # Fix water temperature units
  wrong_tmp_units <- which(water_chem$units == "DEGREES F" &
                             water_chem$dnr_parameter == "10")
  if (length(wrong_tmp_units) != 2) {
    stop("There are more field temp results than expected with units of DEGREES F")
  } else {
    water_chem$units[water_chem$dnr_parameter == "10"] <- "DEGREES C"
  }

  # Check for unexpected unit mismatch
  parameter_units <- unique(water_chem[,c("dnr_parameter", "description","units")]) %>%
                     filter(!.data$dnr_parameter %in% c("10", "20", "NA"))
  parameters      <- unique(parameter_units[,c("dnr_parameter", "description")])
  if (nrow(parameter_units) != nrow(parameters)) {
    stop("At least one parameter is reported with more than one type of units")
  }

  # FACTOR ---------------------------------------------------------------------
  water_chem <- water_chem %>%
                mutate_at(c("site_id", "dnr_parameter", "description", "units",
                            "flag", "lake", "site_type"), as.factor) %>%
                select(.data$lake,
                       .data$site_type,
                       .data$site_id,
                       .data$date,
                       .data$dnr_parameter,
                       .data$description,
                       .data$result,
                       .data$units,
                       .data$depth1_m,
                       .data$depth2_m,
                       .data$lod,
                       .data$loq,
                       .data$flag,
                       .data$flag_reason)
  water_chem$lake <- factor(water_chem$lake,
                            levels = lakes)

  return(water_chem)
}
