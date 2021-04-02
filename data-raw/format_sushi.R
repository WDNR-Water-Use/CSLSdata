# Formatting and other pre-analysis operations on raw fish data downloaded from the fisheries database
# https://infotrek.er.usgs.gov/wdnr_biology/static/wdnr_home.shtml
# Found by searching by WBICs for Pleasant and Long lakes (no recent surveying on Plainfield Lake)
# ALW stands for Age, Length, Weight

sushi <- list(
  long = read.csv("data-raw/Long raw fish.csv", header = TRUE, na.strings = "-"),
  longALW = read.csv(file = "data-raw/Long length_weight_age.csv", header = TRUE, na.strings = "-"),
  pleas = read.csv(file = "data-raw/pleasant fish raw.csv", header = TRUE, na.strings = "-"),
  pleasALW = read.csv(file = "data-raw/Pleasant length_weight_age_raw_data.csv", na.strings = "-")
)

library(devtools)
usethis::use_data(sushi, overwrite = TRUE, compress = "xz")
