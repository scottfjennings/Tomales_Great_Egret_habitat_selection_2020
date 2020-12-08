
# add habitat classification to ODBA values
# can run all this as of 12/8/2020


# packages, source ----
library(tidyverse)
library(lubridate)
library(amt)
#options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")
source("code/utility_functions.r")

# read habitat data
tomales_habitat <- raster("derived_data/habitat/Marigear_Eelgrass_CARI_mo.tif")
hab_names_df <- read.csv("derived_data/habitat/Tomales_habitat_raster_key_2.csv") %>% 
  mutate(coarse.name = as.character(coarse.name))

tomales_dem_bathy <- raster("derived_data/habitat/tomales_dem_bathy_max.tif")

# no odba for GREG 6
wild_gregs <- filter(wild_gregs, bird != "GREG_6")

# read data with calculated ODBA, has UTM for closest timestamped GPS location.
# this created in hetp_data_work/code_HETP/data_management/add_covariates.R
hetp_odba <- readRDS("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/ready_for_analysis/hetp_for_odba")

# function using amt::extract_covariates to assign 
make_odba_habitat <- function(zbird) {
odba_track <- hetp_odba %>% 
  filter(!is.na(odba.timestamp), !is.na(timestamp.x), bird == zbird) %>% 
  arrange(timestamp.x) %>% 
  mk_track(.x = utm.easting, .y = utm.northing, .t = timestamp.x, crs = sp::CRS("+init=epsg:32710"), all_cols = T)


odba_habitat <- odba_track %>% 
  amt::extract_covariates(tomales_habitat) %>% 
  amt::extract_covariates(tomales_dem_bathy) %>% 
  filter(!is.na(tomales_dem_bathy_max)) %>% 
  rename(habitat = Marigear_Eelgrass_CARI_mo, bathymetry = tomales_dem_bathy_max)

}

all_greg_odba_habitat <- map_df(wild_gregs$bird, make_odba_habitat) %>% 
  full_join(., dplyr::select(hab_names_df, coarse.name, habitat = Value))

saveRDS(all_greg_odba_habitat, "derived_data/birds/odba_habitat")
