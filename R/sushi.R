#' CSLS raw fish data
#'
#' Species, dates, techniques, some age/length/weight data
#'
#' Downloaded from the fisheries database, searching by WBIC for each lake.
#'
#' Raw data is processed in with the script "format_sushi" in the data-raw folder
#'
#' @examples
#' # Load fish data for Long Lake directly
#' lake  <- "pleas"
#' sushi <- CSLSdata::sushi[[lake]]
#'
#' # Load fish data, then subset for specific lake
#' data(sushi)
#' sushi <- sushi$pleas
#'
#' @docType data
#'
#' @usage data(sushi)
#'
#' @format A list of four data frames:
#' \describe{
#'   \item{pleas}{data for Pleasant Lake}
#'   \item{pleasALW}{age/length/weight data for Pleasant Lake}
#'   \item{long}{data for Long Lake}
#'   \item{longALW}{age/length/weight for Long Lake}
#' }
#'
"sushi"
