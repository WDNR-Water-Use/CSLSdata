#' Import Isotope Measurements
#'
#' This function loads isotope measurements for the CSLS lakes and extracts
#' isotopic measurements for 18O and 2H (duterium) flagged as VALID = TRUE. Raw
#' data is obtained by saving the file
#' "//central/water/WQWT_PROJECTS/WY_MS_Monitoring/2018 Summer Field
#' Crew/Central Sands Lake Study/Stable Isotopes/_Well_Isotope_Tracking.xsls" as
#' a csv in the "raw-data" subdirectory of this project.
#'
#' @param filename name of isotope csv file with stable isotope measurement
#'                 info, including (but not limited to):
#' \itemize{
#'   \item Lake - lake associated with measurement
#'   \item Site.ID - unique site id for where location was taken (e.g. LL-01 for
#'                   Long Lake well 1). Assumed to match site ids in sites_file.
#'   \item Valid - field indicating whether or not sample measurement is valid
#'                 for analysis (TRUE) or not (FALSE).
#'   \item d18O..VSMOW. - stable isotope measurement for d18O at this site
#'   \item dD..VSMOW. - stable isotope measurement for d18O at this site
#'   \item Collection.Date.Time - date and time of sample collection
#' }
#'
#' @param filedir name of subdirectory with csv file
#' @param use_kniffin logical defaults to TRUE to add in kniffin precipitation
#'                    data
#'
#' @return isotopes, a data frame with the following columns:
#'  \item{date}{date of measurement [POSIXct]}
#'  \item{lake}{lake, either "Pleasant", "Long", "Plainfield", "Hancock",
#'            "Chaffee", or "Tagatz" [factor]}
#'  \item{site_id}{unique ID for site of measurement, e.g. "PRECIP", "LONG",
#'               "LL-01" [factor]}
#'  \item{d18O}{isotopic composition for 18O (per mil) [numeric]}
#'  \item{d2H}{isotopic composition for 2H (per mil) [numeric]}

import_isotopes <- function(filename, filedir = "data-raw", use_kniffin = TRUE) {
  # Load libraries
  library(dplyr)
  library(lubridate)
  library(reshape2)

  # Read csv
  isotopes <- read.csv(sprintf("%s/%s", filedir, filename))
  isotopes <- isotopes %>%
              filter(Valid == TRUE,
                     is.na(d18O..VSMOW.) == FALSE) %>%
              mutate_at(c("Collection.Date.Time", "Site.ID"), as.character()) %>%
              mutate(date = mdy_hm(.data$Collection.Date.Time)) %>%
              select(date = .data$date,
                     site_id = .data$Site.ID,
                     d18O = .data$d18O..VSMOW.,
                     d2H = .data$dD..VSMOW.)

  if (use_kniffin) {
    kniffin <- read.csv("data-raw/isotopes_kniffin.csv")
    kniffin <- kniffin %>%
               mutate_at(c("date", "site_id"), as.character) %>%
               filter(.data$site_id == "PRECIP") %>%
               mutate(date = as_datetime(mdy(.data$date))) %>%
               select(.data$date, .data$site_id, .data$d18O, .data$d2H)
    kniffin$date <- kniffin$date + months(1) - days(1)
    isotopes     <- rbind(isotopes, kniffin)
  }

  # QC: make sure all sites have a match with the dictionary sites
  iso_ids     <- isotopes %>%
                 mutate(site_id = as.character(site_id)) %>%
                 select(site_id) %>%
                 unique() %>%
                 unlist()
  dict_ids    <- dictionary %>%
                 mutate(site_id = as.character(site_id)) %>%
                 select(site_id) %>%
                 unique() %>%
                 unlist()
  unknown_ids <- iso_ids[!iso_ids %in% c(dict_ids, "PRECIP", "TAGATZ", "CHAFFEE")]
  if (length(unknown_ids > 0)) {
    warning(sprintf("Unknown site ids in isotope data: %s", unknown_ids))
  }

  # Rearrange data frame to match SWIMS
  isotopes <- melt(isotopes, id.vars = c("date", "site_id"))
  colnames(isotopes) <- c("date", "site_id", "description", "result")

  # Fix precip site_id
  isotopes$site_id <- as.character(isotopes$site_id)
  isotopes$site_id[isotopes$site_id == "PRECIP"] <- "HCK"
  isotopes <- isotopes[which(!isotopes$site_id %in% c("TAGATZ", "CHAFFEE")),]

  isotopes$units <- "PER MIL"
  isotopes$flag  <- "NONE"

  isotopes <- isotopes %>%
              select(.data$date, .data$description, .data$result,
                     .data$units, .data$site_id, .data$flag)

  return(isotopes)
}
