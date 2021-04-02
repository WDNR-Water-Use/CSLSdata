#' Import Dictionary
#'
#' This function loads the dictionary for CSLS lakes into a data frame, with no
#' transformations of the data.
#'
#' @param filename name of csv file linking lake and well measurement sites to
#'                   other identifying characteristics, e.g., lake, obs_type,
#'                   site_id, SWIMS_station_id, USGS_id, WBIC, elev_m, etc.
#' @param filedir name of subdirectory with csv file
#'
#' @return site_dictionary, the same inforamtion as a data frame.

import_dictionary <- function(filename, filedir = "data-raw") {
  dictionary <- read.csv(sprintf("%s/%s", filedir, filename))
  return(dictionary)
}
