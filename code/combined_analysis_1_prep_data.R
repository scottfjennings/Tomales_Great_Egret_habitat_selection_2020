



# packages, source ----
library(tidyverse)
library(lubridate)
library(amt)
#options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")
source("code/utility_functions.r")

# read habitat data ----
tomales_habitat <- raster("derived_data/habitat/Marigear_Eelgrass_CARI_mo.tif")
hab_names_df <- read.csv("derived_data/habitat/Tomales_habitat_raster_key_2.csv") %>% 
  mutate(coarse.name = as.character(coarse.name))

tomales_dem_bathy <- raster("derived_data/habitat/tomales_dem_bathy_max.tif")

hetp_for_habitat_sel <- readRDS("derived_data/birds/wild_greg_tomales")


# function to turn GPS data into steps/bursts, calculate step length and turn angles, and combine with habitat data
# 10 minute sampling interval yields mean and med step lengths that are sufficiently greater than 10m (GPS error), see code chuck in misc_tasks.R
make_combined_data <- function(zbird) {
greg_track <- hetp_for_habitat_sel %>% 
  filter(bird == zbird) %>%
  amt::make_track(utm.easting, utm.northing, timestamp, crs = sp::CRS("+init=epsg:32710"), water.level = water.level, inlight = inlight) %>%  
  amt::track_resample(rate = minutes(10), tolerance = minutes(1)) %>%
  amt::filter_min_n_burst() %>%
  steps_by_burst(keep_cols = "both") %>% 
  amt::random_steps() %>% 
  amt::extract_covariates(tomales_habitat, where = "both") %>% 
  amt::extract_covariates(tomales_dem_bathy, where = "both") %>% 
  rename(habitat.type.start = Marigear_Eelgrass_CARI_mo_start,
         habitat.type.end = Marigear_Eelgrass_CARI_mo_end,
         elevation.start = tomales_dem_bathy_max_start,
         elevation.end = tomales_dem_bathy_max_end) %>% 
  mutate(bird = zbird)
}



# call function and fix habitat names

greg_steps_habitat <- map_df(wild_gregs$bird, make_combined_data) %>% 
  full_join(., dplyr::select(hab_names_df, habitat.start = coarse.name, habitat.type.start = Value)) %>% 
  full_join(., dplyr::select(hab_names_df, habitat.end = coarse.name, habitat.type.end = Value))


# fix 
# Point Reyes lowest observed tide = -2.69; highest observed = 8.54
# lowest elevation in LiDAR DEM = -1.38



saveRDS(greg_steps_habitat, "derived_data/amt_bursts/greg_steps_habitat")
