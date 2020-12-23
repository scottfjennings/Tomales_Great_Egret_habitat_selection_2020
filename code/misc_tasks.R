
library(tidyverse)

source("code/utility_functions.r")

# calculate area of eelgrass coverage in tomales bay ----
eelgrass_table <- read.csv("C:/Users/scott.jennings/Documents/Projects/hetp/analyses/tomales_habitat_selection_2020/derived_data/habitat/eelgrass_2017_attribute_table.csv")

eelgrass_table %>% 
  filter(location == "Tomales Bay") %>% 
  summarise(eel.area = sum(area_m2)) %>% 
  mutate(eel.area.ha = eel.area/10000)


# what are the min and max water depth (elevations relative to current tide) for each habitat ----

read_greg_dat <- function(zbird) {
all_greg_dat <- readRDS(paste("derived_data/amt_bursts/", zbird, "_burst", sep = "")) %>% 
  filter(inlight == T) %>% 
  mutate(bird = zbird)
}

all_greg_dat <- map_df(wild_gregs$bird, read_greg_dat)

all_greg_dat %>% 
  data.frame() %>% 
  group_by(habitat) %>% 
  summarise(min.depth = min(water.depth),
            max.depth = max(water.depth))


# how do step lengths change with different sample rate and how does this relate to GPS accuracy
# trying to determine what sample rate is best
hetp_for_habitat_sel <- readRDS("derived_data/birds/wild_greg_tomales")

test_sample_rate <- function(zbird, zrate) {
greg_track <- hetp_for_habitat_sel %>% 
  filter(bird == zbird) %>% 
  amt::make_track(utm.easting, utm.northing, timestamp, crs = sp::CRS("+init=epsg:32710"), water.level = water.level, inlight = inlight) %>% 
  amt::track_resample(rate = minutes(zrate), tolerance = minutes(1)) %>%
  amt::filter_min_n_burst() %>%
  steps() %>% 
  summarise(min.step = min(sl_),
            med.step = median(sl_),
            mean.step = mean(sl_),
            max.step = max(sl_)) %>% 
  mutate(bird = zbird,
         sample.rate = zrate)
}

rate30 <- map2_df(wild_gregs$bird, 30, test_sample_rate)

rate10 <- map2_df(wild_gregs$bird, 10, test_sample_rate)

rate5 <- map2_df(wild_gregs$bird, 5, test_sample_rate)

rates <- rbind(rate5, rate10, rate30)


rates_long <- rates %>% 
  pivot_longer(cols = contains("step"), names_to = "step.stat")

rates_long %>% 
  filter(sample.rate != 30) %>% 
ggplot() +
  geom_point(aes(x = sample.rate, y = value)) +
  facet_wrap(~step.stat, scales = "free")
  
  
  
  
  
  
  
  
