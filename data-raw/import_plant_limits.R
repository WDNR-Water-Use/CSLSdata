#' Import plant depth limits
#'
#' This function loads the rules for plant depth limits for the CSLS lakes and
#' transforms depths from feet to meters.
#'
#' @return plant_limits, a data frame with the following columns:
#'   \item{lake}{lake, e.g., "Pleasant", "Long", or "Plainfield"}
#'   \item{variable}{plant community, e.g. "upland" or "inland_beach"}
#'   \item{shallow}{the shallowest water depth this plant community can tolerate
#'                  (m)}
#'   \item{deep}{the deepest water depth this plant community can tolerate (m)}
#'

import_plant_limits <- function(filename,
                                filedir = "data-raw") {

  library(dplyr)
  library(NISTunits)

  plant_limits <- read.csv(sprintf("%s/%s", filedir, filename))  %>%
                  mutate_at(c("shallow", "deep"), NISTftTOmeter)

  return(plant_limits)
}
