#' Import SWIMS data
#'
#' This function loads in SWIMS data associated with the Central Sands Lakes
#' Study (project: "csls") and cleans with the following steps:
#' 1. Transforms column names to characters that are easier to work with in R.
#' 2. Imports and appends LDES data, if using.
#' 3. Uses station_name and WBIC to match samples to CSLS site ids. Makes sure
#'    all samples have a CSLS site id match.
#' 4. Extracts and parses sample depth or sample depth range, where exists.
#' 5. Updates column classes (to datetime, numeric, etc.)
#' 6. Performs QC on results and makes note of any flagged samples
#' 7. Subsets to useful columns and returns SWIMS data frame.
#'
#' @param filename name of csv with raw SWIMS data
#' @param dictionary data frame linking lake and well measurement sites to
#'                   other identifying characteristics
#' @param filedir name of subdirectory with csv file
#' @param use_LDES logical indicates whether to load LDES data as well
#' @param LDESfile filename of LDES csv, if using
#'
#' @return SWIMS, a data frame with all SWIMS info for the CSLS, including;
#'  \item{date}{date of measurement}
#'  \item{dnr_parameter}{DNR parameter code for analyte measured}
#'  \item{description}{Description of analyte measured}
#'  \item{result}{value of sample result}
#'  \item{units}{units of sample result}
#'  \item{depth1_m}{sample depth (m)}
#'  \item{depth2_m}{if sample depth is a range, other end of that range (m)}
#'  \item{site_id}{unique id for CSLS site}
#'  \item{lod}{limit of detection for lab method}
#'  \item{loq}{limit of quality for lab method}
#'  \item{flag}{notes if data is flagged for being under the LOD ("LOD"), under
#'              the LOQ ("LOQ"), analyzed past the holding date ("AGE"),
#'              duplicate sample ("DUPLICATE"), blank sample ("BLANK"), other
#'              reason ("COMMENT"), or not flagged ("NONE")}
#'  \item{flag_reason}{longer comment about reason for flag}

import_SWIMS <- function(filename, dictionary, filedir = "data-raw",
                         use_LDES = FALSE, LDESfile = NULL) {
  # Load libraries -------------------------------------------------------------
  library(stringr)
  library(lubridate)
  library(testthat)
  library(dplyr)
  library(NISTunits)

  # Load SWIMS (and apend LDES, if necessary) ----------------------------------
  SWIMS           <- read.csv(sprintf("%s/%s", filedir, filename),
                              colClasses = "character")
  colnames(SWIMS) <- colnames(SWIMS) %>%
                     str_replace_all("\\.\\.","\\.") %>%
                     str_replace_all("\\.", "_") %>%
                     str_replace("_$", "") %>%
                     tolower()
  if (use_LDES){ SWIMS <- import_LDES(SWIMS, LDESfile) }

  # Add CSLS site ids for wells and lakes --------------------------------------
  ids   <- dictionary %>%
           mutate_all(as.character) %>%
           replace(is.na(.), "") %>%
           mutate(SWIMS_station_id = str_replace(SWIMS_station_id, "NA", "")) %>%
           select(site_id = site_id,
                  station_name = SWIMS_station_name) %>%
           unique()
  SWIMS <- SWIMS %>%
           mutate(station_name = toupper(station_name))
  SWIMS <- merge(SWIMS, ids, by = "station_name", all.x = TRUE)

  wbics <- dictionary %>%
           mutate_all(as.character) %>%
           filter(!is.na(WBIC)) %>%
           mutate(SWIMS_station_id = str_replace(SWIMS_station_id, "NA", "")) %>%
           select(site_id = site_id,
                  wbic = WBIC) %>%
           unique()
  for (i in 1:nrow(SWIMS)) {
    if (!is.na(SWIMS$wbic[i])) {
      site_id <- wbics %>%
                 filter(wbic == SWIMS$wbic[i]) %>%
                 select(site_id) %>%
                 unlist()
      if (length(site_id) == 1) {SWIMS$site_id[i] <- site_id}
    }
  }

  missing_sites <- unique(SWIMS$station_name[is.na(SWIMS$site_id)])
  if (length(missing_sites) > 0) {
    warning(sprintf("No CSLS site id associated with: %s", missing_sites))
  }

  # Extract sample depth, where exists -----------------------------------------
  SWIMS$result_depth[is.na(SWIMS$result_depth)] <- ""
  for (i in 1:nrow(SWIMS)) {
    # Extract depth string, if exists
    if (SWIMS$result_depth[i] != "") {
      depths <- str_split(SWIMS$result_depth[i], " ") %>% unlist
    } else if (SWIMS$header_labslip_depth[i] != "") {
      depths <- str_split(SWIMS$header_labslip_depth[i], " ") %>% unlist
    } else {
      depths <- ""
    }

    # Parse depth string
    if (length(depths) == 2) {
      depth1 <- as.numeric(depths[1])
      depth2 <- NA
      units  <- depths[2]
    } else if (length(depths) == 4) {
      depth1 <- as.numeric(depths[1])
      depth2 <- as.numeric(depths[3])
      units  <- depths[4]
    } else if (length(depths) == 1) {
      depth1 <- NA
      depth2 <- NA
      units  <- ""
    } else {
      warning("Unrecognized format for sample depth in row #%d: %s", i, depths)
    }

    # Convert measurements in feet to meters
    if (tolower(units) %in% c("feet", "foot", "ft")){
      depth1 <- NISTftTOmeter(depth1)
      depth2 <- NISTftTOmeter(depth2)
    } else if (!tolower(units) %in% c("meters", "meter", "m", "")){
      warning(sprintf("Units '%s' not understood for row #%d", units, i))
    }
    # Save to dataset
    SWIMS$depth1_m[i] <- suppressWarnings(min(c(depth1, depth2), na.rm = TRUE))
    if (is.infinite(SWIMS$depth1_m[i])) {
      SWIMS$depth1_m[i] <- NA
    }
    SWIMS$depth2_m[i] <- max(c(depth1, depth2))
  }
  # QC check on sample depths
  double_depth <- SWIMS %>%
                  filter(result_depth != "",
                         header_labslip_depth != "") %>%
                  select(start_date_time, site_id, description,
                         result_depth, header_labslip_depth)
  if (nrow(double_depth) > 0) {
    warning(sprintf("Two recorded depths for these samples: %s",
                    str_c(double_depth, sep = "", collapse = ", ")))
  }

  # Update format of data ------------------------------------------------------
  # Strings to datetimes
  SWIMS$start_date_time <- mdy_hm(SWIMS$start_date_time)
  # Strings to numeric
  SWIMS$lod             <- suppressWarnings(as.numeric(SWIMS$lod))
  SWIMS$loq             <- suppressWarnings(as.numeric(SWIMS$loq))
  # NAs to empty strings
  SWIMS$result_comments[is.na(SWIMS$result_comments)]                 <- ""
  SWIMS$lab_comments[is.na(SWIMS$lab_comments)]                       <- ""
  SWIMS$sample_labslip_field_no[is.na(SWIMS$sample_labslip_field_no)] <- ""
  SWIMS$result_depth[is.na(SWIMS$result_depth)]                       <- ""
  SWIMS$header_labslip_depth[is.na(SWIMS$header_labslip_depth)]       <- ""

  # QC on data -----------------------------------------------------------------
  SWIMS$flag        <- "NONE"
  SWIMS$flag_reason <- ""

  for (i in 1:nrow(SWIMS)) {
    numeric_result    <- suppressWarnings(as.numeric(SWIMS$result[i]))
    is_numeric_result <- !is.na(numeric_result)
    is_numeric_lod    <- !is.na(SWIMS$lod[i])
    is_numeric_loq    <- !is.na(SWIMS$loq[i])

    if (str_detect(SWIMS$sample_labslip_field_no[i], "BLANK") |
        str_detect(SWIMS$sample_labslip_field_no[i], "BLNK")) {
      SWIMS$flag[i]        <- "BLANK"
      SWIMS$flag_reason[i] <- "Sample label indicates this is a blank"
    } else if (str_detect(SWIMS$sample_labslip_field_no[i], "DUP")) {
      SWIMS$flag[i]        <- "DUPLICATE"
      SWIMS$flag_reason[i] <- "Sample label indicates this is a duplicate"
    } else if (SWIMS$result[i] == "ND"){
      SWIMS$result[i]      <- "0"
      SWIMS$flag[i]        <- "LOD"
      SWIMS$flag_reason[i] <- sprintf("Non-detect, LOD is %s", SWIMS$lod[i])
    } else if (str_detect(tolower(SWIMS$result_comments[i]), "exceed")){
      SWIMS$flag[i]        <- "COMMENT"
      SWIMS$flag_reason[i] <- SWIMS$result_comments[i]
    } else if (str_detect(tolower(SWIMS$lab_comments[i]), "analyzed past")){
      SWIMS$flag[i]        <- "AGE"
      SWIMS$flag_reason[i] <- SWIMS$lab_comments[i]
    } else if (is_numeric_result & is_numeric_lod) {
      if (SWIMS$lod[i] - numeric_result > 0){
        SWIMS$flag[i]        <- "LOD"
        SWIMS$flag_reason[i] <- sprintf("Result is %s, LOD is %s",
                                        SWIMS$result[i],
                                        SWIMS$lod[i])
      }
    } else if (is_numeric_result & is_numeric_loq){
      if (SWIMS$loq[i] - numeric_result > 0){
        SWIMS$flag[i]        <- "LOQ"
        SWIMS$flag_reason[i] <- sprintf("Result is %s, LOQ is %s (LOD is %s)",
                                        SWIMS$result[i],
                                        SWIMS$loq[i],
                                        SWIMS$lod[i])
      }
    }
  }

  # Weird (one-off) label mix up
  pfl_sample_id  <- 457791002
  long_sample_id <- 457791003
  SWIMS$site_id[which(SWIMS$sample_labslip_id == pfl_sample_id)]  <- "LONG"
  SWIMS$site_id[which(SWIMS$sample_labslip_id == long_sample_id)] <- "PLAIN"

  # Dates SECCHI depth entered as meters instead of feet
  meter_dates <- as_datetime(mdy("06-04-2019", "06-18-2019"))
  meter_SWIMS <- SWIMS %>%
                 filter(floor_date(.data$start_date_time, unit = "day") %in% meter_dates,
                        .data$description == "SECCHI DEPTH - FEET")
  if (min(as.numeric(meter_SWIMS$result)) < 2) {
    SWIMS <- SWIMS %>%
             mutate(result = ifelse(floor_date(.data$start_date_time,
                                               unit = "day") %in% meter_dates &
                                    .data$description == "SECCHI DEPTH - FEET",
                    as.character(NISTmeterTOft(as.numeric(.data$result))),
                    .data$result))
  }

  #  Unbelievably high Chl-A sample at Plainfield
  bad_chla <- SWIMS %>%
              filter(.data$site_id == "PLAIN",
                     .data$dnr_parameter == "99717",
                     floor_date(.data$start_date_time, unit = "day") ==
                       as_datetime("2019-09-10")) %>%
              select(.data$sample_labslip_id) %>%
              as.character()
  SWIMS$flag[SWIMS$sample_labslip_id == bad_chla &
               SWIMS$dnr_parameter == "99717"] <- "BAD_SAMPLE"
  SWIMS$flag_reason[SWIMS$sample_labslip_id == bad_chla &
               SWIMS$dnr_parameter == "99717"] <- "unbelievably high given TP and typical range of chl-a values at PFL"

  #  Reversed top & bottom total phosphorus samples at PSNT on 9/10/19
  flipped <- SWIMS %>%
             filter(.data$site_id == "PLEAS",
                    .data$dnr_parameter == "665",
                    .data$sample_labslip_id %in% c('469413001', '469412003'))
  result01 <- flipped$result[flipped$sample_labslip_id == '469413001']
  result02 <- flipped$result[flipped$sample_labslip_id == '469412003']
  SWIMS$result[SWIMS$sample_labslip_id == '469413001' &
                 SWIMS$dnr_parameter == "665"] <- result02
  SWIMS$result[SWIMS$sample_labslip_id == '469412003' &
                 SWIMS$dnr_parameter == "665"] <- result01

  # Subset SWIMS to only needed (and QC-ed) columns ----------------------------
  SWIMS <- SWIMS %>%
           mutate_at(vars(dnr_parameter, description, units, site_id, flag),
                     as.factor) %>%
           select(date = .data$start_date_time,
                  dnr_parameter = .data$dnr_parameter,
                  description = .data$description,
                  result = .data$result,
                  units = .data$units,
                  depth1_m = .data$depth1_m,
                  depth2_m = .data$depth2_m,
                  site_id = .data$site_id,
                  lod = .data$lod,
                  loq = .data$loq,
                  flag = .data$flag,
                  flag_reason = .data$flag_reason)
  SWIMS <- unique(SWIMS)

  return(SWIMS)
}
