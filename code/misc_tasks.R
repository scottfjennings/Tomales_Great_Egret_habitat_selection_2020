
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
