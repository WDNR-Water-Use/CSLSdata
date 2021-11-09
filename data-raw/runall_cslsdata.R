# Obtain all CSLS data
#
# Runs all import and subsetting scripts to load and filter raw CSLS water
# quantity and water quality data.

# Setup environment ------------------------------------------------------------
library(usethis) # for use_data

# All scripts live within the data-raw subdirectory
function_files <- c(list.files("data-raw", pattern = "import*"),
                    list.files("data-raw", pattern = "subset*"),
                    list.files("data-raw", pattern = "combine*"))
for (file in function_files) {
  source(sprintf("data-raw/%s", file))
}

# All Lakes --------------------------------------------------------------------
dictionary     <- import_dictionary("dictionary_csls.csv")
weather        <- import_weather("weather_hancock_2020_01_07.csv")
lake_raster    <- import_lake_rasters()
plant_limits   <- import_plant_limits("plant_community_depths_2020_11.csv")
fish_substrate <- import_fish_substrate("fishSubstrateMean.Rda")
bathymetry     <- import_bathymetry("elev_area_vol_2020_09_04.csv",
                                    lake_raster,
                                    plant_limits,
                                    fish_substrate)
# water_levels  <- import_water_levels()
# lake_levels   <- subset_lake_levels(water_levels, dictionary, bathymetry)
# gw_levels     <- subset_gw_levels(water_levels, lake_levels, dictionary,
#                                   min_diff = 0.01, window_days = 30)
lake_levels    <- CSLSdata::lake_levels
gw_levels      <- CSLSdata::gw_levels
isotopes       <- import_isotopes("isotopes_csls_2019_12_05.csv",
                                  use_kniffin = TRUE)
NADP           <- import_NADP("NADP_WI_2020_06_01.csv")
SWIMS          <- import_SWIMS("SWIMS_csls_2020_06_01.csv", dictionary,
                               use_LDES = TRUE,
                               LDESfile = "LDES_csls_2020_01_28.csv")
HOBO           <- import_HOBO(dictionary, lake_levels,
                              psnt = "HOBO_psnt_2019_11_15.csv",
                              long = "HOBO_long_2019_11_21.csv",
                              pfl = "HOBO_pfl_2019_12_13.csv")
water_chem     <- combine_chem(SWIMS, NADP, isotopes, HOBO, gw_levels, dictionary)
MODFLOW        <- import_model_results()

# Save to data directory -------------------------------------------------------
usethis::use_data(weather, dictionary, lake_raster, plant_limits, bathymetry,
                  lake_levels, gw_levels, water_chem, MODFLOW, overwrite = TRUE,
                  compress = "xz")

# Save csv versions to inst/csv directory --------------------------------------
csv_dir <- "inst/csv"

write.csv(weather,
          sprintf("%s/weather.csv", csv_dir),
          row.names = FALSE)
write.csv(dictionary,
          sprintf("%s/dictionary.csv", csv_dir),
          row.names = FALSE)
write.csv(plant_limits,
          sprintf("%s/plant_limits.csv", csv_dir),
          row.names = FALSE)
write.csv(bathymetry,
          sprintf("%s/bathymetry.csv", csv_dir),
          row.names = FALSE)
write.csv(lake_levels,
          sprintf("%s/lake_levels.csv", csv_dir),
          row.names = FALSE)
write.csv(gw_levels,
          sprintf("%s/gw_levels.csv", csv_dir),
          row.names = FALSE)
write.csv(water_chem,
          sprintf("%s/water_chem.csv", csv_dir),
          row.names = FALSE)
write.csv(MODFLOW,
          sprintf("%s/MODFLOW.csv", csv_dir),
          row.names = FALSE)
