#' Import LDES data
#'
#' This function loads in LDES data associated with the Central Sands Lakes
#' Study. Column names are transformed so that periods (R's replacement for
#' white spaces, parentheses, etc.) are replaced by an underscore and all are
#' converted to lowercase. Changes key colnames and formatting of some text
#' fields to match SWIMS conventions, adds DNR parameter descriptions, then
#' appends any LDES results not yet in SWIMS to the SWIMS data frame.
#'
#' @param SWIMS a data frame with all SWIMS info for the CSLS
#' @param filename name of csv with raw SWIMS data
#' @param filedir name of subdirectory with csv file
#'
#' @return SWIMS, a data frame with all SWIMS info plus LDES for the CSLS.

import_LDES <- function(SWIMS, filename, filedir = "data-raw") {
  # Load LDES ------------------------------------------------------------------
  LDES           <- read.csv(sprintf("%s/%s", filedir, filename),
                             colClasses = "character")
  colnames(LDES) <- colnames(LDES) %>%
                    str_replace_all("\\.\\.","\\.") %>%
                    str_replace_all("\\.", "_") %>%
                    str_replace("_$", "") %>%
                    tolower()

  # Clean text, rename columns -------------------------------------------------
  LDES <- LDES %>%
          mutate(station_name = str_replace_all(sample_location,"-", " - ")) %>%
          mutate(sample_depth = str_replace_all(sample_depth, " ", "")) %>%
          mutate(sample_depth = str_replace(sample_depth, "-", " to ")) %>%
          mutate(sample_depth = str_replace(sample_depth, "F", " Feet")) %>%
          mutate(sample_depth = str_replace(sample_depth, "M", " Meters"))%>%
          mutate(sample_depth = str_replace(sample_depth, "D", " Feet")) %>%
          select(start_date_time = .data$start_date_time,
                 dnr_parameter = .data$dnr_parameter_code,
                 result = .data$result_value,
                 units = .data$units,
                 lod = .data$lod,
                 loq = .data$loq,
                 wbic = .data$waterbody_id,
                 sample_labslip_field_no = .data$field,
                 station_name = .data$station_name,
                 lab_comments = .data$lab_comment,
                 header_labslip_depth = .data$sample_depth)

  # Add DNR parameter description ----------------------------------------------
  params <- SWIMS %>%
            select(dnr_parameter, description) %>%
            unique()
  LDES   <- merge(LDES, params, all.x = TRUE)

  # QC check -------------------------------------------------------------------
  unknown_cols <- colnames(LDES)[!colnames(LDES) %in% colnames(SWIMS)]
  if (length(unknown_cols > 0)) {
    warning(sprintf("Columns in LDES not in SWIMS: %s", unknown_cols))
  }

  # Append and return unique results -------------------------------------------
  LDES  <- LDES %>%
           filter(!.data$sample_labslip_field_no %in%
                    SWIMS$sample_labslip_field_no)
  SWIMS <- bind_rows(SWIMS, LDES)

  return(SWIMS)
}
