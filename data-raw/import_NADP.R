# Load NADP Precipitation Data

#' This function loads in NADP data and cleans with the following steps:
#' 1. Defines SWIMS equivalent of NADP parameters of interest
#' 2. Limits only valid data to the desired site (default: Devil's Lake).
#' 3. Rearranges data frame for parameters of interest
#' 4. Retains any LOD flags, eliminates clearly invalid results (< 0)
#' 5. Converts NADP param names to SWIMS equivalents
#' 6. Add units, date, and site_id
#'
#' @param filename name of csv with raw NADP data
#' @param filedir name of subdirectory with csv files
#' @param dictionary_file name of csv with NADP site information. Defaults to
#'                        "NADP_WI_dictionary.csv"
#' @param keep_site name of NADP site to use for analysis, defaults to
#'                  "Devils Lake"
#'
#' @return NADP, a data frame with all NADP info for Devil's Lake, including;
#'  \item{dateint}{date interval (lubridate)}
#'  \item{date}{end date of measurement interval}
#'  \item{description}{parameter sampled}
#'  \item{result}{value of sample result}
#'  \item{units}{units of sample result}
#'  \item{site_id}{id of sample site (i.e., "NADP")}
#'  \item{flag}{data flag, "LOD" or "NONE"}

import_NADP <- function(filename,
                        filedir = "data-raw",
                        dictionary_file = "NADP_WI_dictionary.csv",
                        keep_site = "Devils Lake") {
  # Setup environment ------------------------------------------------------------
  library(lubridate)
  library(reshape2)
  library(usethis)
  library(dplyr)
  library(stringr)

  # Not retained: Br, NH4, pcpn depth
  NADP_to_SWIMS <- data.frame(NADP_name = c("ph", "Conduc", "Ca", "Mg", "K",
                                            "Na", "NH4", "NO3", "Cl", "SO4"),
                              SWIMS_name = c("PH LAB",
                                             "CONDUCTIVITY, UMHOS/CM @ 25C",
                                             "CALCIUM TOTAL RECOVERABLE",
                                             "MAGNESIUM TOTAL RECOVERABLE",
                                             "POTASSIUM TOTAL RECOVERABLE",
                                             "SODIUM TOTAL RECOVERABLE",
                                             "NITROGEN NH3-N DISS",
                                             "NITROGEN NO3+NO2 DISS (AS N)",
                                             "CHLORIDE",
                                             "SULFATE TOTAL"),
                              DNR_parameter = c(403, 95, 918, 921, 50245,
                                                923, 608, 631, 940, 945))
  NADP_to_SWIMS <- NADP_to_SWIMS %>% mutate_all(as.character)

  # Process data ---------------------------------------------------------------
  # Site dictionary (names + ids)
  NADP_dict <- read.csv(sprintf("%s/%s", filedir, dictionary_file))
  keep_id   <- NADP_dict %>%
               filter(.data$SITE.NAME == keep_site) %>%
               select(.data$SITE.ID) %>%
               unlist() %>%
               as.character()

  # Load raw data
  NADP_pcpn <- read.csv(sprintf("%s/%s", filedir, filename))
  NADP_pcpn <- NADP_pcpn %>%
               filter(.data$siteID == keep_id,
                      as.character(.data$valcode) %in% c("w ", "wa", "wi")) %>%
               mutate(start_date = ymd_hm(.data$dateon),
                      end_date = ymd_hm(.data$dateoff))


  # Reorganize data for parameters of interest
  NADP           <- NADP_pcpn %>%
                    select(.data$start_date, .data$end_date, .data$ph, .data$Conduc)
  NADP           <- melt(NADP, id.vars = c("start_date", "end_date"))
  colnames(NADP) <- c("start_date", "end_date", "description", "result")
  NADP$flag      <- "NONE"
  for (param in NADP_to_SWIMS$NADP_name[3:nrow(NADP_to_SWIMS)]) {
    this_param           <- NADP_pcpn[,c("start_date",
                                         "end_date",
                                         param,
                                         sprintf("flag%s", param))]
    colnames(this_param) <- c("start_date",
                              "end_date",
                              "result",
                              "flag")
    this_param$description <- param
    NADP                 <- suppressWarnings(bind_rows(NADP, this_param))
  }

  # Retain LOD flag
  NADP$flag[NADP$flag == "<"]   <- "LOD"
  NADP$flag[NADP$flag != "LOD"] <- "NONE"
  NADP$flag[is.na(NADP$flag)]   <- "NONE"

  # Remove invalid results
  NADP$result[NADP$result < 0] <- NA

  # Convert NADP parameter name to SWIMS name
  NADP$description   <- as.character(NADP$description)
  NADP$dnr_parameter <- NADP$description
  for (i in 1:nrow(NADP_to_SWIMS)) {
    NADP$description <- str_replace_all(NADP$description,
                                        NADP_to_SWIMS$NADP_name[i],
                                        NADP_to_SWIMS$SWIMS_name[i])
    NADP$dnr_parameter <- str_replace_all(NADP$dnr_parameter,
                                          NADP_to_SWIMS$NADP_name[i],
                                          as.character(NADP_to_SWIMS$DNR_parameter[i]))
  }

  # Add units
  NADP$units <- "MG/L"
  NADP$units[NADP$description == "PH LAB"] <- "SU"
  NADP$units[NADP$description == "CONDUCTIVITY, UMHOS/CM @ 25C"] <- "uS/cm"

  # Convert NH4 (mg/L) to NH3-N (mg/L) & NO3 (mg/L) to NO3-N (mg/L)
  NADP$result[NADP$dnr_parameter == 608] <- (18.03846/14.0067)*
                                          NADP$result[NADP$dnr_parameter == 608]
  NADP$result[NADP$dnr_parameter == 631] <- (62.0049/14.0067)*
                                          NADP$result[NADP$dnr_parameter == 631]

  # Add date interval, define single "date" as end_date.
  # NADP$dateint <- interval(NADP$start_date, NADP$end_date)
  NADP$date    <- NADP$end_date

  # Add site_id
  NADP$site_id <- "NADP"

  # Rearrange final data frame
  NADP <- NADP %>%
          select(.data$date,
                 .data$dnr_parameter,
                 .data$description,
                 .data$result,
                 .data$units,
                 .data$site_id,
                 .data$flag)
  return(NADP)
}
