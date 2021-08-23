



# packages, source ----
library(tidyverse)
library(lubridate)
library(amt)
library(maptools)
#options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")
source("code/utility_functions.r")

set.seed(1)
wetlands_key <- data.frame(Value = seq(1, 4),
                           wetland.type = c("tidal.marsh", "other.tidal", "shellfish", "eelgrass")) %>% 
  mutate(Value = as.numeric(Value))


# data prep ----

# read habitat data 
tomales_wetlands <- raster("derived_data/habitat/tomales_wetlands.tif")
#values(tomales_wetlands) %>% data.frame() %>% rename(values = 1) %>% group_by(values) %>% summarise(num.each = n())


#hab_names_df <- read.csv("derived_data/habitat/Tomales_habitat_raster_key_2.csv") %>%  mutate(coarse.name = as.character(coarse.name))

tomales_dem_bathy <- raster("derived_data/habitat/tomales_dem_bathy_max_0.tif")
tomales_dem_bathy_m <- ft2m(tomales_dem_bathy)

hetp_for_habitat_sel <- readRDS("derived_data/birds/wild_greg_tomales")
hetp_for_habitat_sel <- hetp_for_habitat_sel %>% 
  mutate(water.level = ft2m(water.level)) %>% 
  filter(inlight == TRUE) %>% 
  dplyr::select(bird, utm.easting, utm.northing, timestamp, water.level)

tomales_wetlands <- projectRaster(tomales_wetlands, crs = proj4string(tomales_dem_bathy), method = "ngb")
#values(tomales_wetlands) %>% data.frame() %>% rename(values = 1) %>% group_by(values) %>% summarise(num.each = n())

# added after review: boxplot of depths for each wetland type
crop_extent <- extent(500428, 516689, 4212797, 4232697)

wetlands_dem <- stack(resample(tomales_wetlands, tomales_dem_bathy_m, method = "ngb"), tomales_dem_bathy_m)
wetlands_dem_vals <- wetlands_dem %>% 
  getValues() %>% 
  data.frame() 

wetlands_dem_vals %>% 
  rename(Value = tomales_wetlands, depth)
  
  
# make tracks, assign habitat values to GPS points ----

# function to turn GPS data into steps/bursts, calculate step length and turn angles, and combine with habitat data
# 10 minute sampling interval yields mean and med step lengths that are sufficiently greater than 10m (GPS error), see code chuck in misc_tasks.R
make_combined_data <- function(zbird) {
greg_track <- hetp_for_habitat_sel %>% 
  filter(bird == zbird) %>%
  amt::make_track(utm.easting, utm.northing, timestamp, crs = sp::CRS("+init=epsg:32710"), water.level = water.level) %>%  
  amt::track_resample(rate = minutes(10), tolerance = minutes(1)) %>%
  amt::filter_min_n_burst() %>%
  steps_by_burst(keep_cols = "both") %>% 
  amt::random_steps() %>% 
  amt::extract_covariates(tomales_wetlands, where = "both") %>% 
  amt::extract_covariates(tomales_dem_bathy_m, where = "both") %>% 
  mutate(bird = zbird)
}



# call function

greg_tracks <- map_df(wild_gregs$bird, make_combined_data) 

greg_tracks <- greg_tracks %>% 
  rename(wetland.num.start = tomales_wetlands_start,
         wetland.num.end = tomales_wetlands_end,
         elevation.start = tomales_dem_bathy_max_0_start,
         elevation.end = tomales_dem_bathy_max_0_end)

saveRDS(sl_distr(greg_tracks), "mod_objects/tentative_movement_parms/tentative_sl")
saveRDS(ta_distr(greg_tracks), "mod_objects/tentative_movement_parms/tentative_ta")
# add human readable habitat names and clean
full_join(greg_tracks %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.num.start) %>%
  summarise(num.used.start = n()) %>% 
    rename(wetland = contains("wetland")),
  greg_tracks %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.num.end) %>%
  summarise(num.used.end = n()) %>% 
    rename(wetland = contains("wetland"))) %>% 
  full_join(., wetlands_key %>% rename(wetland = Value))

greg_steps_habitat <- greg_tracks %>% 
  full_join(., dplyr::select(wetlands_key, wetland.start = wetland.type, wetland.num.start = Value)) %>% 
  full_join(., dplyr::select(wetlands_key, wetland.end = wetland.type, wetland.num.end = Value))


full_join(greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.start) %>%
  summarise(num.used.start = n()) %>% 
    rename(wetland = contains("wetland")),
  greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.end) %>%
  summarise(num.used.end = n()) %>% 
    rename(wetland = contains("wetland")))

# Point Reyes lowest observed tide = -2.69; highest observed = 8.54





# I don't quite understand the math justification for the movement parms transformations, but this is what the amt peeps do in their papers

# positive values for depth.end are below current tide level
# drop_na() strips amt formatting/classes from object, filter(!is.na()), maintains amt structure
greg_steps_habitat <- greg_steps_habitat %>% 
  filter(!is.na(wetland.num.start)) %>% 
  filter(!is.na(wetland.num.end)) %>% 
  filter(!is.na(elevation.start)) %>% 
  filter(!is.na(elevation.end)) %>% 
  filter(elevation.end < 10) %>% 
  mutate(depth.end = water.level_end - elevation.end) %>% 
  mutate(cos_ta_ = cos(ta_),
         log_sl_ = log(sl_)) %>% 
  mutate(wetland.start = as.factor(wetland.start),
         wetland.start = relevel(wetland.start, "other.tidal"),
         wetland.end = as.factor(wetland.end),
         wetland.end = relevel(wetland.end, "other.tidal")) 



full_join(greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.start) %>%
  summarise(num.used.start = n()) %>% 
    rename(wetland = contains("wetland")),
  greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.end) %>%
  summarise(num.used.end = n()) %>% 
    rename(wetland = contains("wetland")))


greg_steps_habitat <- greg_steps_habitat %>% 
  mutate(dt.sec = (as.numeric(dt_)*60),
         speed.m.s = sl_/dt.sec) %>% 
  group_by(bird) %>% 
         mutate(prev.sl = lag(sl_))



saveRDS(greg_steps_habitat, "derived_data/amt_bursts/greg_steps_habitat")

#
#### run to here to prep data for model fitting ----
# data checking ----

greg_steps_habitat <- readRDS("derived_data/amt_bursts/greg_steps_habitat")

greg_steps_habitat <- greg_steps_habitat %>% 
  mutate(case_ = as.character(case_),
         data.label = case_when(case_ == "FALSE" ~ "Available (model)",
                                case_ == "TRUE" ~ "Used"),
         data.label = factor(data.label, levels = c("Available (model)", "Used")),
         wetland.label = case_when(wetland.end == "eelgrass" ~ "Eelgrass",
                                   wetland.end == "shellfish" ~ "Shellfish aquaculture",
                                   wetland.end == "tidal.marsh" ~ "Tidal marsh",
                                   wetland.end == "other.tidal" ~ "Other tidal"),
         wetland.label = factor(wetland.label, levels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal")))


greg_steps_habitat %>% 
  data.frame() %>% 
  dplyr::group_by(case_) %>%
  summarise(num.case = n())

greg_steps_habitat %>% 
  data.frame() %>% 
  dplyr::group_by(case_, wetland.end) %>%
  summarise(num.case = n()) %>% 
  pivot_wider(id_cols = wetland.end, names_from = case_, values_from = num.case) %>% 
  rename(available = 2, used = 3) %>% 
  mutate(used.avail = used/available)

greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.end) %>%
  summarise(num.used = n()) %>% 
  arrange(-num.used)

# range of depths and elevations of used and available points for each wetland type
# negative depth = height above water level
greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.end) %>% 
  dplyr::summarise(min.elev = min(elevation.end),
            mean.elev = mean(elevation.end),
            max.elev = max(elevation.end),
            min.depth = min(depth.end),
            mean.depth = mean(depth.end),
            max.depth = max(depth.end))

greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.end) %>% 
  dplyr::summarise(quants = quantile(depth.end, probs = c(0.025, 0.975))) %>% 
  mutate(depth.quant = rep(c("lwr", "upr"))) %>% 
  pivot_wider(id_cols = wetland.end, names_from = depth.quant, values_from = quants)

deep_greg <- greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE, depth.end > 1) %>% 
  dplyr::select(x2_, y2_, bird, t2_, elevation.end, water.level_end, depth.end)

deep_greg_spdf <- SpatialPointsDataFrame(coords = cbind(deep_greg$x2_, deep_greg$y2_), data = deep_greg, proj4string = CRS("+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

rgdal::writeOGR(obj = deep_greg_spdf, dsn = "derived_data/birds/deep_greg_spdf", layer = "deep_greg_spdf", driver="ESRI Shapefile")

greg_steps_habitat %>% 
  data.frame() %>%  
  filter(between(depth.end, -1.7, 1)) %>% 
  ggplot()+
  geom_density(aes(x = depth.end, ..count.., color = data.label)) +
  facet_wrap(~wetland.label) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab("Depth (m)\nnegative values indicate height above water level") +
  ylab("Total steps") +
  #scale_x_continuous(breaks = seq(-3, 3)) +
  scale_color_brewer(palette = "Dark2")
ggsave("figures/habitat_depth_boxplot.png", width = 6, height = 6, dpi = 300)

greg_steps_habitat %>% 
  data.frame() %>%  
  filter(between(depth.end, -1.7, 1)) %>% 
  group_by(wetland.end) %>% 
  mutate(outlier = depth.end > median(depth.end) + 
               IQR(depth.end)*1.5 | depth.end < median(depth.end) -
               IQR(depth.end)*1.5)%>%
  ungroup() %>% 
  mutate(case_ = as.character(case_),
         data.label = case_when(case_ == "FALSE" ~ "Available (model)",
                                case_ == "TRUE" ~ "Used"),
         data.label = factor(data.label, levels = c("Used", "Available (model)")),
         wetland.label = case_when(wetland.end == "eelgrass" ~ "Eelgrass",
                                   wetland.end == "shellfish" ~ "Shellfish\naquaculture",
                                   wetland.end == "tidal.marsh" ~ "Tidal\nmarsh",
                                   wetland.end == "other.tidal" ~ "Other\ntidal"),
         wetland.label = factor(wetland.label, levels = c("Eelgrass", "Shellfish\naquaculture", "Tidal\nmarsh", "Other\ntidal"))) %>% 
  ggplot(aes(x = wetland.label, y = depth.end))+
  geom_boxplot(outlier.shape = NA) + 
  geom_point(data = function(x) dplyr::filter(x, outlier), position = "jitter", width = 0.01) +
  scale_y_continuous(breaks = seq(-3, 3)) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ylab("Depth (m)\nnegative values indicate height above water level") +
  xlab("") +
  scale_color_brewer(palette = "Dark2") +
  geom_hline(yintercept = 1)

ggsave("figures/habitat_depth_boxplot.png", width = 6, height = 6, dpi = 300)


greg_steps_habitat %>% 
  data.frame() %>%  
  mutate(elevation.end.m = ft2m(elevation.end)) %>% 
  ggplot()+
  geom_density(aes(x = elevation.end, color = case_)) +
  facet_wrap(~wetland.end)

greg_steps_habitat %>% 
  data.frame() %>%  
  filter(case_ == TRUE) %>% 
  filter(between(depth.end, -5, 5)) %>% 
  ggplot()+
  geom_point(aes(x = depth.end, y = elevation.end)) +
  facet_wrap(~wetland.end)

# plot gps points
greg_steps_habitat %>% 
  data.frame() %>%  
  filter(case_ == TRUE) %>% 
  ggplot() +
  geom_point(aes(x = x2_, y = y2_, color = wetland.end)) +
  facet_wrap(~bird)



# other data summary for the paper ----
# range of depths in each wetland type ----


tomales_dem_bathy <- raster("derived_data/habitat/tomales_dem_bathy_max_0.tif")
tomales_dem_bathy_m <- ft2m(tomales_dem_bathy)

tomales_wetlands <- raster("derived_data/habitat/tomales_wetlands.tif")
tomales_wetlands <- projectRaster(tomales_wetlands, crs = proj4string(tomales_dem_bathy), method = "ngb")


extent(tomales_dem_bathy_m)

extent(tomales_wetlands)

dem.new <- resample(tomales_dem_bathy_m, tomales_wetlands, method = "ngb")


tomales_wetlands_dem <- stack(tomales_wetlands, dem.new)

dem.new.neg1.4 <- dem.new
dem.new.neg1.4[dem.new.neg1.4 < -1.4] <- NA
tomales_wetlands_available <- overlay(tomales_wetlands, dem.new.neg1.4, fun = function(x, y) {
  x[is.na(y[])] <- NA
  return(x)
})

area_wetlands_available <- tomales_wetlands_dem %>% 
  values() %>% 
  data.frame() %>% 
  rename(Value = 1) %>% 
  full_join(wetlands_key) %>% 
  filter(!is.na(Value)) %>% 
  group_by(wetland.type) %>% 
  summarise(area.m2 = n() * 100,
            area.km2 = area.m2/1000000) %>% 
  ungroup() %>% 
  mutate(percent.total = 100 * (area.km2 / sum(area.km2)),
         percent.total = round(percent.total, 1)) 


sum(area_wetlands_available$area.km2)

tomales_wetlands_dem2 <- stack(tomales_wetlands_available, dem.new)



wetland_depths <- as.data.frame(tomales_wetlands_dem) %>% 
  rename(elevation = tomales_dem_bathy_max_0) %>% 
  full_join(., wetlands_key %>% rename(tomales_wetlands = Value)) %>% 
  filter(!is.na(elevation), !is.na(tomales_wetlands))

wetland_depths %>% 
  group_by(wetland.type) %>% 
    summarise_at("elevation", funs(min, max, mean, median)) 

wetland_depths  %>% 
  ggplot() +
  geom_density(aes(x = elevation)) +
  geom_vline(xintercept = -1.4) +
  facet_wrap(~wetland.type, scales = "free")


habitat_elevations <- wetland_depths %>% 
  dplyr::select(-tomales_wetlands) %>% 
  mutate(wetland.label = case_when(wetland.type == "eelgrass" ~ "Eelgrass",
                                   wetland.type == "shellfish" ~ "Shellfish\naquaculture",
                                   wetland.type == "tidal.marsh" ~ "Tidal\nmarsh",
                                   wetland.type == "other.tidal" ~ "Other\ntidal"),
         wetland.label = factor(wetland.label, levels = c("Eelgrass", "Shellfish\naquaculture", "Tidal\nmarsh", "Other\ntidal")))
    
habitat_elevations %>% 
  filter(between(elevation, -1.5, 3)) %>% 
  ggplot() +
  geom_density(aes(x = elevation, ..count..), bw = 0.25, size = 1) +
  facet_wrap(~wetland.label) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab("Elevation (m)") +
  ylab("Number of raster cells") +
  scale_x_continuous(breaks = seq(-3, 3)) +
  scale_color_brewer(palette = "Dark2")

ggsave("figures/habitat_elevation_kernel_density.tiff", width = 8, height = 5, dpi = 300)

habitat_elevations %>% 
  #filter(between(elevation, -3, 3)) %>% 
  ggplot() +
  geom_boxplot(aes(x = wetland.label, y = elevation)) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ylab("Elevation (m)") +
  xlab("") +
  scale_color_brewer(palette = "Dark2")

ggsave("figures/habitat_elevation_boxplot_all_elevations.png", width = 6, height = 6, dpi = 300)


# depths of used and available points in each wetland type ----

habitat_depths <- readRDS("derived_data/amt_bursts/greg_steps_habitat") %>% 
          dplyr::select(case_, depth = depth.end, wetland.type = wetland.end) 

habitat_depths %>% 
  data.frame() %>% 
  group_by(wetland.type) %>% 
    summarise_at("depth", list(min, max, mean, median)) 

habitat_depths <- habitat_depths %>%
  mutate(data.label = case_when(case_ == FALSE ~ "Available",
                                case_ == TRUE ~ "Used")) %>% 
  mutate(wetland.label = case_when(wetland.type == "eelgrass" ~ "Eelgrass",
                                   wetland.type == "shellfish" ~ "Shellfish\naquaculture",
                                   wetland.type == "tidal.marsh" ~ "Tidal\nmarsh",
                                   wetland.type == "other.tidal" ~ "Other\ntidal"),
         wetland.label = factor(wetland.label, levels = c("Eelgrass", "Shellfish\naquaculture", "Tidal\nmarsh", "Other\ntidal")))





habitat_depths %>% 
  filter(between(depth, -1, 1)) %>% 
  ggplot() +
  geom_density(aes(x = depth, ..count.., color = data.label), bw = 0.25, size = 1) +
  facet_wrap(~wetland.label) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab("Tide-dependent water depth (m)") +
  ylab("Number of points") +
  scale_x_continuous(breaks = seq(-3, 3)) +
  scale_color_brewer(palette = "Dark2")

ggsave("figures/S1_Fig.tiff", width = 8, height = 5, dpi = 300)

# area in each wetland type ----

# read habitat data 
tomales_wetlands <- raster("derived_data/habitat/tomales_wetlands.tif")

tomales_wetlands_vals <- values(tomales_wetlands) %>% 
  data.frame()%>%  
  rename(Value = 1) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(!is.na(Value)) %>% 
  full_join(., wetlands_key) %>% 
  filter(!wetland.type %in% exclude_wetlands) %>% 
  mutate(wetland.type = ifelse(wetland.type %in% other_tidal, "other.tidal", wetland.type)) %>% 
  group_by(wetland.type) %>% 
  summarise(area.m2 = n() * 100,
            area.km2 = area.m2/1000000) %>% 
  ungroup() %>% 
  mutate(percent.total = 100 * (area.km2 / sum(area.km2)),
         percent.total = round(percent.total, 1))


tomales_wetlands_vals_elevs <- wetland_depths %>% 
  filter(!wetland.type %in% exclude_wetlands, ) %>% 
  mutate(wetland.type = ifelse(wetland.type %in% other_tidal, "other.tidal", wetland.type),
         avail.elevation = ifelse(elevation > -1.4, TRUE, FALSE)) %>% 
  group_by(wetland.type, avail.elevation) %>% 
  summarise(area.m2 = n() * 100,
            area.km2 = area.m2/1000000) %>% 
  ungroup() %>% 
  mutate(percent.total = 100 * (area.km2 / sum(area.km2)),
         percent.total = round(percent.total, 1)) 


tomales_wetlands_vals_elevs %>% 
  filter(avail.elevation == TRUE) %>% 
  summarise()



tomales_wetlands_ll <- projectRaster(tomales_wetlands, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), method = "ngb")







readRDS("derived_data/amt_bursts/greg_steps_habitat") %>% 
  filter(!is.na(habitat.start), case_ == TRUE) %>% 
  ggplot()+
  geom_histogram(aes(x = elevation.start), binwidth = 1) +
  ggtitle("steps_habitat") +
  facet_wrap(~habitat.start, scales = "free")



g1_deep <- greg_steps_habitat %>% 
  filter(case_ == TRUE, bird == "GREG_1", depth.end > 0.5) %>% dplyr::select(contains("end")) %>%  view()
x = 504000
y = 4229400
buf = 1000
plot(tomales_dem_bathy_m)
crop(tomales_dem_bathy_m, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot()
points(g1_deep$x1_, g1_deep$y1_, pch = 19, cex = 0.1)


# number of days tracked for each bird
readRDS("derived_data/amt_bursts/greg_steps_habitat") %>% 
  data.frame() %>%  
  mutate(date = as.Date(t1_)) %>% 
  group_by(bird) %>% 
  distinct(date) %>% 
  summarise(n.days = n(),
            min.day = min(date),
            max.day = max(date)) %>% 
  view()

readRDS("derived_data/amt_bursts/greg_steps_habitat") %>% 
  data.frame() %>%  
  mutate(date = as.Date(t1_)) %>% 
  group_by(bird) %>% 
  distinct(date) %>% 
  summarise(n.days = n()) %>% 
  ungroup() %>% 
  summarise(mean.days = mean(n.days),
            se.days = sd(n.days)/sqrt(n())) %>% 
  view() 
  

# total number of used steps for each bird in each habitat
greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  group_by(bird, habitat.end) %>% 
  summarise(num.steps = n()) %>%
  ungroup() %>% 
  pivot_wider(names_from = "habitat.end", values_from = "num.steps") %>% 
 mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(tot.points = other.tidal + eelgrass + shellfish + tidal.marsh) %>% 
  arrange(-tot.points) %>% 
  view()
# proportion of used steps for each bird in each habitat
greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  group_by(bird, habitat.end) %>% 
  summarise(num.steps = n()) %>%
  ungroup() %>% 
  group_by(bird) %>% 
  mutate(tot.steps = sum(num.steps),
         percent.steps = num.steps/tot.steps,
         percent.steps = round(percent.steps*100, 1)) %>%
  dplyr::select(bird, habitat.end, percent.steps) %>% 
  pivot_wider(names_from = "habitat.end", values_from = "percent.steps") %>% 
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
