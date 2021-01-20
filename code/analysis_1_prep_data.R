



# packages, source ----
library(tidyverse)
library(lubridate)
library(amt)
#options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")
source("code/utility_functions.r")

set.seed(1)
# data prep ----
# read habitat data 
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


#  need to fix the classification of some raster cells based on their elevation and tide heights. 
# Point Reyes lowest observed tide = -2.69; highest observed = 8.54
# lowest elevation in LiDAR DEM = -1.38
#sub_inter_bound = -2.69
# 1/20/2021 I decided the differentiation between intertidal and subtidal was too dependent on elevation values that were iffy. Elevations right around the boundary between the LiDAR dem and the sonar bathymetry layer seem a little suspect to me



# I don't quite understand the math justification for the movement parms transformations, but this is what the amt peeps do in their papers

# positive values for depth.end are below current tide level
# drop_na() strips amt formatting/classes from object, filter(!is.na()), maintains amt structure
greg_steps_habitat <- greg_steps_habitat %>% 
  filter(!is.na("habitat.start")) %>% 
  filter(!is.na("habitat.end")) %>% 
  filter(!is.na("elevation.start")) %>% 
  filter(!is.na("elevation.end")) %>% 
  filter(habitat.start != "freshwater.wetland", habitat.end != "freshwater.wetland") %>% 
  filter(bird != "GREG_4") %>% 
  filter(elevation.end < 10) %>% 
  #mutate(habitat.start = as.character(habitat.start),
  #       habitat.start = ifelse(habitat.start == "intertidal" & elevation.start < sub_inter_bound, "subtidal", habitat.start),
  #       habitat.start = ifelse(habitat.start == "subtidal" & elevation.start >= sub_inter_bound, "intertidal", habitat.start)) %>% 
  #mutate(habitat.end = as.character(habitat.end),
  #       habitat.end = ifelse(habitat.end == "intertidal" & elevation.end < sub_inter_bound, "subtidal", habitat.end),
  #       habitat.end = ifelse(habitat.end == "subtidal" & elevation.end >= sub_inter_bound, "intertidal", habitat.end)) %>% 
  mutate(habitat.end = ifelse(habitat.end %in% c("subtidal", "intertidal"), "other.tidal", habitat.end),
         habitat.start = ifelse(habitat.start %in% c("subtidal", "intertidal"), "other.tidal", habitat.start)) %>% 
  mutate(depth.end = round(water.level_end, 2) - round(elevation.end, 2)) %>% 
  mutate(cos_ta_ = cos(ta_),
         log_sl_ = log(sl_)) %>% 
  mutate(habitat.start = as.factor(habitat.start),
         habitat.start = relevel(habitat.start, "other.tidal"),
         habitat.end = as.factor(habitat.end),
         habitat.end = relevel(habitat.end, "other.tidal")) 




saveRDS(greg_steps_habitat, "derived_data/amt_bursts/greg_steps_habitat")

#
#### run to here to prep data for model fitting ----
# data checking ----

greg_steps_habitat <- readRDS("derived_data/amt_bursts/greg_steps_habitat")

# area of each wetland type in raster

rast <- raster("derived_data/habitat/Marigear_Eelgrass_CARI_mo.tif")

rast_ll <- projectRaster(rast, crs = "+proj=longlat +datum=WGS84", method = "ngb")

tomales_dem_bathy <- raster("derived_data/habitat/tomales_dem_bathy_max.tif")

tomales_dem_bathy_ll <- projectRaster(tomales_dem_bathy, crs = "+proj=longlat +datum=WGS84")


 
z.extent = extent(-122.9789, -122.8083, 38.06231, 38.23470)

dem.new <- resample(tomales_dem_bathy_ll, rast_ll, method = "bilinear")




mask_rast <- rast_ll %>% crop(., z.extent)



mask_dem <- dem.new %>% 
  crop(., z.extent) 


hab_stack <- stack(mask_rast, mask_dem)

# adapted from https://gis.stackexchange.com/questions/167465/reclassifying-raster-stack-based-on-condition-and-other-layers-using-r
#Write reclassification function

rc <- function(Marigear_Eelgrass_CARI_mo, tomales_dem_bathy_max) {
  ifelse((Marigear_Eelgrass_CARI_mo == 11 | Marigear_Eelgrass_CARI_mo == 39 | Marigear_Eelgrass_CARI_mo == 40) & tomales_dem_bathy_max < sub_inter_bound, 18, Marigear_Eelgrass_CARI_mo)
  ifelse(Marigear_Eelgrass_CARI_mo == 18 & tomales_dem_bathy_max >= sub_inter_bound, 11, Marigear_Eelgrass_CARI_mo)
}

#Apply function to raster stack

hab_stack2 <- overlay(hab_stack, fun=rc)




hab_area_2 <- tapply(area(hab_stack2), hab_stack2[], sum) %>% 
  data.frame() %>%  
  rename(area.m2 = 1) %>% 
  rownames_to_column("Value") %>% 
  mutate(Value = as.numeric(Value)) %>% 
  full_join(., hab_names_df) %>% 
  filter(!is.na(coarse.name)) %>% 
  group_by(coarse.name) %>% 
  summarise(area.m2.alt = sum(area.m2))

hab_area_ll <- tapply(area(rast_ll), rast_ll[], sum)  %>% 
  data.frame() %>%  
  rename(area.m2 = 1) %>% 
  rownames_to_column("Value") %>% 
  mutate(Value = as.numeric(Value)) %>% 
  full_join(., hab_names_df) %>% 
  filter(!is.na(coarse.name)) %>% 
  mutate(coarse.name = ifelse(coarse.name %in% c("subtidal", "intertidal"), "other.tidal", coarse.name)) %>% 
  group_by(coarse.name) %>% 
  summarise(area.m2.ll = sum(area.m2))

compare_area <- full_join(hab_area_ll, hab_area_2)

compare_area %>% 
  filter(!is.na(coarse.name)) %>% 
  summarise(tot.area = sum(area.m2.ll),
                           tot.area.alt = sum(area.m2.alt))





values(hab_stack) %>% 
  data.frame() %>% 
  rename(Value = Marigear_Eelgrass_CARI_mo) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  full_join(., hab_names_df) %>%
  filter(!is.na(coarse.name)) %>% 
  ggplot()+
  geom_histogram(aes(x = tomales_dem_bathy_max), binwidth = 1) +
  ggtitle("hab_stack") +
  facet_wrap(~coarse.name, scales = "free")

readRDS("derived_data/amt_bursts/greg_steps_habitat")%>% 
  filter(!is.na(habitat.start), case_ == TRUE) %>% 
  ggplot()+
  geom_histogram(aes(x = elevation.start), binwidth = 1) +
  ggtitle("steps_habitat") +
  facet_wrap(~habitat.start, scales = "free")


# number of days tracked for each bird
greg_steps_habitat %>% 
  data.frame() %>%  
  mutate(date = as.Date(t1_)) %>% 
  group_by(bird) %>% 
  distinct(date) %>% 
  summarise(n.days = n(),
            min.day = min(date),
            max.day = max(date)) %>% 
  view()

greg_steps_habitat %>% 
  data.frame() %>%  
  mutate(date = as.Date(t1_)) %>% 
  group_by(bird) %>% 
  distinct(date) %>% 
  summarise(n.days = n()) %>% 
  ungroup() %>% 
  summarise(mean.days = mean(n.days)) %>% 
  view() 
  

# total number of used steps for each bird in each habitat
greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  group_by(bird, habitat.end) %>% 
  summarise(num.steps = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "habitat.end", values_from = "num.steps") %>% 
  view()

# mean step length and turn angle; used later for making model predictions
greg_steps_habitat %>% 
  data.frame() %>% 
  filter(!is.na(ta_)) %>% 
  #group_by(bird) %>% 
  summarise(mean_sl_ = mean(sl_), # 127
            mean_ta_ = mean(ta_), # 127
            mean_cos_ta_ = mean(cos_ta_), # 127
            cos_mean_ta_ = cos(mean_ta_)) # close enough to 1

# extract highest and lowest water depths for each habitat
habitat_depths <- greg_steps_habitat %>% 
  data.frame() %>% 
  group_by(habitat.end) %>% 
  summarise(min.depth = min(depth.end),
            max.depth = max(depth.end)) %>% 
  mutate(max.depth2 = floor(max.depth / 0.5) * 0.5,
         max.depth2 = ft2m(max.depth2),
         min.depth2 = floor(min.depth / 0.5) * 0.5,
         min.depth2 = ft2m(min.depth2))



greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE, habitat.end != "subtidal") %>% 
  ggplot() +
  geom_point(aes(x = x2_, y = y2_, color = elevation.end)) +
  facet_wrap(~habitat.end)

filter(greg_steps_habitat, habitat.start == "intertidal" & elevation.start < sub_inter_bound) %>% view()
filter(greg_steps_habitat, habitat.end %in% c("intertidal", "subtidal"), depth.end < -5) %>% dplyr::select(habitat.end, elevation.end, water.level_end, depth.end) %>% view()
# look at number of used vs available for all habitats
# this adapted from appendix A of Fieberg, J., Signer, J., Smith, B.J. and Avgar, T., 2020. A “How-to” Guide for Interpreting Parameters in Resource-and Step-Selection Analyses. bioRxiv.
# https://www.biorxiv.org/content/10.1101/2020.11.12.379834v1.abstract

# zbird = "GREG_1"
greg_steps_habitat %>%  
  data.frame() %>% 
  filter(bird == zbird) %>% 
  dplyr::group_by(case_, habitat.end) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(habitat.end, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "habitat", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light() +
  ggtitle(zbird)


# are there some bursts that seem to be daytime roosting rather than foraging?

day_roost <- greg_steps_habitat %>% 
  data_frame() %>% 
  group_by(bird, burst_) %>% 
  summarise(mean.sl = mean(sl_),
            burst.dist = sum(sl_),
            burst.time = sum(dt_),
            num.steps = n()) %>% 
  ungroup() %>% 
  mutate(burst.speed = burst.dist/as.numeric(burst.time))
# not really - most bursts are composed of steps that are longer than the GPS error we filtered with (10m), and thus mostly represent true movements greater than this distance
# most of the day roosting points are likely excluded by excluding points outside tidal areas
# update. but, negative values after adjusting von Misus concentration parms suggest there actually are a lot of times when birds are stationary  

## map points per habitat

greg_steps_habitat %>% 
  data_frame() %>%
  filter(bird == zbird, case_ == TRUE) %>% 
  ggplot() +
  geom_point(aes(x = x2_, y = y2_, color = habitat.end))



# density curves of step lengths

greg_steps_habitat %>% 
  tibble() %>% 
  filter(bird %in% wild_gregs$bird, case_ == TRUE, sl_ < 50) %>% 
  ggplot() +
  geom_density(aes(x = ta_)) +
  facet_wrap(~habitat.end)
