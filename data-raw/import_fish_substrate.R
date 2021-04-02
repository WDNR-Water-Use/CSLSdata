#' Import centrarchid fish substrate info
#'
#' This function loads the data frame with information on area of suitably hard
#' substrate for centrarchid fish at Pleasant Lake based on Biobase survey.
#' Converts approximate area in acres to approximate area in m^2 to match units
#' on other measures of area.
#'
#' @return fish_substrate, a data frame with the following columns:
#'   \item{lake}{lake, only have data on "Pleasant"}
#'   \item{elev_m}{lake elevation (m)}
#'   \item{substrate_area_m2}{approximate area of suitably hard substrate for
#'                            centrarchid fish at this elevation (m^2)}

import_fish_substrate <- function(filename,
                                  filedir = "data-raw") {

  library(dplyr)
  library(NISTunits)

  load(sprintf("%s/%s", filedir, filename))
  fish_substrate <- fishSubstrateMean

  return(fish_substrate)
}
