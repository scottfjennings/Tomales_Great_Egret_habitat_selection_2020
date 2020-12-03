

# pull together all data for analysis into single object, create any new variables that are derivatives of info from different objects


# specific tasks:
# items to combine
# GREG GPS points (with tide, etc info from hetp_data_work\code_HETP\data_management\interpolate_bird_tides.r and add_covariates.r??)
# ODBA from hetp_data_work\code_HETP\data_management\calculate_ODBA.r
# habitat and DEM rasters from spatial_data_read_clean.r

# convert to amt
# make amt track from GPS, 
# generate random points 
# peel off real locations with ODBA and habitat classification; these will be analyzed separately from the iSSF analysis

# additional covariates
# add water depth to real and random points based on DEM and tide level




# packages, source ----
# some packages loaded in spatial_data_read_clean.r
library(lubridate)
library(amt)
library(lunar)
library(raster)
#options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")
source("code/utility_functions.r")
# !!! set seed important so we get the same results each time random steps are generated
set.seed(1)

# new functions ----
filter_by_speed <- function(ztrack, upper.speed = 5) { 
greg_speed <- amt::speed(ztrack)

greg_track_speed <- cbind(ztrack, greg_speed)

greg_no_fly <- greg_track_speed %>% 
  dplyr::select(t_, greg_speed) %>% 
  filter(greg_speed < upper.speed)

greg_dat <- ztrack %>%
  right_join(., greg_no_fly, by = c("t_"))
return(greg_dat)
}


greg_dat_to_burst <- function(zbird) { # wrapper function for multiple data processing steps
greg_track <- hetp_for_habitat_sel %>% 
  filter(bird == zbird) %>% 
  amt::make_track(utm.easting, utm.northing, timestamp, crs = sp::CRS("+init=epsg:32710"), water.level = water.level, inlight = inlight) %>% 
  filter_by_speed() %>% 
  amt::track_resample(rate = minutes(5), tolerance = minutes(1)) %>%
  amt::filter_min_n_burst() %>%
  amt::steps_by_burst(keep_cols = "end") %>%
  amt::random_steps() %>% 
  amt::extract_covariates(tomales_habitat, where = "end") %>% 
  amt::extract_covariates(tomales_dem_bathy, where = "end") %>% 
  rename(habitat.type = Marigear_Eelgrass_CARI_mo,
         elevation = tomales_dem_bathy_max)

# notes on changes to nrow:
# steps_by_burst() drops last row of each burst_, and first row of first burst_ nrow() after steps_by_burst = nrow(greg_track) - (distinct(greg_track, burst_) %>% nrow()) + 1
# then, steps_by_burst() makes direction_p and ta_ == NA for first step
# first step gets dropped by random_steps() due to NAs, so nrow() after random_steps() = (nrow() after steps_by_burst() - 1) * (1 + n_control)
# extract_covariates() does not change nrow()

}

  

# Point Reyes lowest observed tide = -2.69; highest observed = 8.54
# lowest elevation in LiDAR DEM = -1.38

reclass_sub_intertidal <- function(greg_dat, sub_inter_bound = -2.69) {
greg_dat <- greg_dat %>% 
  mutate(old.hab = habitat,
         habitat = case_when(habitat %in% c("intertidal", "subtidal") & elevation < sub_inter_bound ~ "subtidal",
                             habitat %in% c("intertidal", "subtidal") & elevation >= sub_inter_bound ~ "intertidal",
                             TRUE ~ as.character(habitat)))
}

simplify_moon_phase <- function(greg_dat) {
moon_phases <- data.frame(moon = c("New", "Waxing crescent", "First quarter", "Waxing gibbous", "Full", "Waning gibbous", "Last quarter", "Waning crescent"),
                          moon.simple = rep(c("new.full", "quarter", "half", "quarter"), 2))
greg_dat <- greg_dat %>% 
  mutate(moon = lunar.phase(date, name = 8)) %>% 
  full_join(., moon_phases)
}


burst_wrapper <- function(zbird, save.burst = T) {
greg_dat <- greg_dat_to_burst(zbird) %>% 
  left_join(., dplyr::select(hab_names_df, habitat.type = Value, habitat.num = coarse.num, habitat = coarse.name) %>% distinct(), by = c("habitat.type")) 

greg_dat_complete <- greg_dat %>%
  group_by(step_id_) %>% 
  filter_at(vars(habitat, elevation),all_vars(!is.na(.))) %>% 
  mutate(habitat = as.factor(habitat),
         habitat = relevel(habitat, "intertidal"),
         date = as.Date(t1_)) %>% 
  reclass_sub_intertidal() %>% 
  simplify_moon_phase() %>% 
  mutate(water.depth = water.level - elevation) 

greg_dat_complete2 <- greg_dat %>%
  filter(case_ == TRUE) %>% 
  drop_na(habitat, elevation) %>% 
  droplevels() %>% 
  select(step_id_) %>% 
  left_join(., greg_dat) 

if(save.burst == T) {
  saveRDS(greg_dat_complete, paste("derived_data/amt_bursts/", zbird, "_burst", sep = ""))

} else {
return(greg_dat_complete)
}
}

# data ----

# uses habitat data from code/spatial_data_read_clean.r; 
wild_gregs <- data.frame(bird = paste("GREG_", seq(1:11), sep = ""))

tomales_habitat <- raster("derived_data/habitat/Marigear_Eelgrass_CARI_mo.tif")
hab_names_df <- read.csv("derived_data/habitat/Tomales_habitat_raster_key_2.csv") %>% 
  mutate(coarse.name = as.character(coarse.name))

tomales_dem_bathy <- raster("derived_data/habitat/tomales_dem_bathy_max.tif")

# n_tomales_center lat =  38.191736, long = -122.915068 


# study_area_clipped_gps <- read.dbf("C:/Users/scott.jennings/Documents/Projects/hetp/analyses/tomales_habitat_selection_2020/derived_data/hetp_201706_202007_tomales_10m.dbf")

# readRDS("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/ready_for_analysis/hetp_for_habitat_sel") %>% mutate_if(is.factor, as.character) %>% filter(bird %in% wild_gregs$bird, event.id %in% study_area_clipped_gps$event_id) %>% saveRDS("derived_data/birds/wild_greg_tomales")

hetp_for_habitat_sel <- readRDS("derived_data/birds/wild_greg_tomales")

# 1 bird 
# zbird = "GREG_2"



# calling functions ----
# 

greg_dat <- burst_wrapper("GREG_1", save.burst = F)




greg_dat %>% 
  filter(habitat != "freshwater.wetland") %>% 
  filter(elevation <= 10) %>% 
  #filter(water.depth < 2, water.depth > -5) %>% 
  ggplot() +
  geom_histogram(aes(x = water.depth, fill = case_), binwidth = 1) +
  facet_wrap(~habitat, scales = "free")


system.time(map(wild_gregs$bird, burst_wrapper))

# 2020-09-10 all 11 wild birds
#    user  system elapsed 
#  380.34   22.07  408.66 

# saveRDS(greg_burst_all_dat, paste("derived_data/amt_bursts/", zbird, "_burst", sep = ""))

# test plot to look at water depth for use points
greg_burst_all_dat %>% 
  filter(y1_ > 4229000, x1_ < 505500) %>% 
  ggplot(.) +
  geom_point(aes(x = x1_, y = y1_, color = habitat.type))


# check to make sure there are real data points for all levels of the varbs
greg_burst_all_dat %>% 
  filter(case_ == T) %>% 
  group_by(habitat.type) %>% 
  summarise(num.hab = n())



# if not, filter out those levels

greg_dat <- greg_dat %>% 
  filter(cari != "11") %>%
  #filter(cari != "1") %>% 
  #filter(cari != "2") %>% 
  droplevels()
# greg3 no points in cari1 and only 3 real points in cari11, removed those levels
# very few data points for greg4, not using
# greg5 no points in cari1 and only 3 real points in cari11, removed those levels
# greg6 no points in cari11, removed that level
# greg7 very few points in shellfish, but kept in model
# greg8 no points in cari1 and only 3 real points in cari11, removed those levels
# greg9 no points in cari2, removed that level. no points in shellfish, removed from model
# greg10 no points in cari11, removed that level, 28 shellfish points but keeping in model
# greg11 only 2 points in cari11, removed that level
