#' Import CSLS Hourly Weather
#'
#' This function reads in hourly weather from the Hancock, WI weather station.
#' Required weather fields include air temperature (deg C), relative humidity
#' (%), precipitation (mm), solar radiation (MJ/m^2/hr), and wind speed (m/s).
#' This funcition cuts lines known to contain uneeded metadata, updates the
#' format of values, and fills in NA values via linear interpolation.
#'
#' @param filename name of Hancock weather csv
#' @param filedir name of subdirectory with csv file
#' @param skip_lines [numeric] number of rows in csv to skip before reading in
#'                   data, defaults to 7.
#'
#' @return weather, a data frame with the following columns for hourly weather:
#' \item{date}{date and time of weather observation}
#' \item{atmp}{air temperature (deg C)}
#' \item{P}{precipitation (mm)}
#' \item{RH}{relative humidity (percent)}
#' \item{Rs}{incoming solar or shortwave radiation (MJ/m^2/hr)}
#' \item{wind}{wind speed (m/s)}

import_weather <- function(filename, filedir = "data-raw", skip_lines = 7){
  # Load libraries
  library(lubridate)
  library(dplyr)
  library(zoo)

  # Load weather data
  weather      <- read.csv(sprintf("%s/%s", filedir, filename),
                           skip = skip_lines)

  # Fix formatting
  end_data     <- which(weather$date == "Variable Ids:") - 2
  weather      <- weather[1:end_data, ]
  weather$date <- as.character(weather$date)
  weather$time <- as.character(weather$time)
  weather$atmp <- as.numeric(as.character(weather$atmp))
  weather$srad <- weather$srad/1000 #kJ to MJ

  # Fix times
  weather$time[which(weather$time == "24:00:00")] <- "24:00"
  weather$date <- mdy_hm(sprintf("%s %s", weather$date, weather$time))

  # Select columns of interest
  weather           <- weather %>%
                       select(date, atmp, pcpn, relh, srad, wspd)
  colnames(weather) <- c("date","atmp","P","RH","Rs","wind")

  # Interpolate NAs
  zoo.weather   <- read.zoo(weather)
  zoo.weather   <- as.data.frame(na.approx(zoo.weather))
  weather[,2:6] <- zoo.weather

  return(weather)
}
