
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


# define functions ----
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
  rename(habitat.type = Marigear_Eelgrass_CARI_mo,
         elevation = tomales_dem_bathy_max)

}

# calculate step lengths
hetp_for_habitat_sel <- readRDS("derived_data/birds/wild_greg_tomales")


make_steps_habitat <- function(zbird) {
greg_track <- hetp_for_habitat_sel %>% 
  filter(bird == zbird) %>% 
  amt::make_track(utm.easting, utm.northing, timestamp, crs = sp::CRS("+init=epsg:32710"), water.level = water.level, inlight = inlight) %>% 
  amt::track_resample(rate = minutes(30), tolerance = minutes(1)) %>%
  amt::filter_min_n_burst() %>%
  steps() %>% 
  mutate(ta.deg = as_degree(ta_),
         ta.deg.abs = abs(ta.deg)) %>% 
  mutate(speed = sl_ / as.numeric(dt_)) %>% 
  amt::extract_covariates(tomales_habitat, where = "both") %>% 
  amt::extract_covariates(tomales_dem_bathy, where = "both") %>% 
  rename(habitat.type.start = Marigear_Eelgrass_CARI_mo_start,
         habitat.type.end = Marigear_Eelgrass_CARI_mo_end,
         elevation.start = tomales_dem_bathy_max_start,
         elevation.end = tomales_dem_bathy_max_end) %>% 
  mutate(bird = zbird)
}



# call functions and save output


all_greg_odba_habitat <- map_df(wild_gregs$bird, make_odba_habitat) %>% 
  full_join(., dplyr::select(hab_names_df, coarse.name, habitat.type = Value))

saveRDS(all_greg_odba_habitat, "derived_data/birds/odba_habitat")


steps_habitat <- map_df(wild_gregs$bird, make_steps_habitat) %>% 
  full_join(., dplyr::select(hab_names_df, coarse.name.start = coarse.name, habitat.type.start = Value)) %>% 
  full_join(., dplyr::select(hab_names_df, coarse.name.end = coarse.name, habitat.type.end = Value))

saveRDS(steps_habitat, "derived_data/birds/steps_habitat_30min")


# checking created data
steps_habitat <- readRDS("derived_data/birds/steps_habitat_30min")



steps_habitat %>% 
  filter(!is.na(coarse.name.start)) %>% 
  filter(bird != "GREG_4") %>% 
  ungroup() %>% 
  group_by(bird, coarse.name.start) %>% 
  summarise(num.zero = n()) %>% 
  spread(key = coarse.name.start, value = num.zero)


steps_habitat %>% 
  filter(bird != "GREG_4") %>% 
  filter(coarse.name.start %in% c("eelgrass", "shellfish")) %>% 
  filter(!is.na(coarse.name.start)) %>% 
ggplot() +
  geom_density(aes(x = sl_, fill = coarse.name.start, alpha = 0.2)) +
  facet_wrap(~bird, scales = "free")

steps_habitat %>% 
  ungroup() %>% 
  filter(sl_ <= 1) %>% 
  group_by(bird) %>% 
  summarise(num.zero = n())


steps_habitat %>% 
  filter(coarse.name.start %in% c("eelgrass", "shellfish")) %>% 
  #filter(!is.na(coarse.name.start)) %>% 
ggplot() +
  geom_density(aes(x = ta_, fill = coarse.name.start, alpha = 0.2)) +
  facet_wrap(~bird, scales = "free")

