
library(tidyverse)
library(amt)
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


# how do step lengths change with different sample rate and how does this relate to GPS accuracy ----
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

rate20 <- map2_df(wild_gregs$bird, 20, test_sample_rate)

rate10 <- map2_df(wild_gregs$bird, 10, test_sample_rate)

rate5 <- map2_df(wild_gregs$bird, 5, test_sample_rate)

rates <- rbind(rate5, rate10, rate20, rate30)


rates_long <- rates %>% 
  pivot_longer(cols = contains("step"), names_to = "step.stat")

rates_long %>% 
  #filter(sample.rate != 30) %>% 
  filter(step.stat %in% c("med.step"), !bird %in% c("GREG_4", "GREG_7", "GREG_9", "GREG_11")) %>% 
ggplot() +
  geom_point(aes(x = sample.rate, y = value)) +
  ylab("Median step length (m)") +
  xlab("GPS sampling interval") +
  scale_y_continuous(breaks = seq(0, 1000, by = 20), labels = seq(0, 1000, by = 20)) +
  scale_x_continuous(breaks = seq(0, 30, by = 5), labels = seq(0, 30, by = 5)) +
  #facet_wrap(~step.stat, scales = "free") +
  ggtitle("Median step length by\nindividual GPS tagged\ngreat egret")
  
  ggsave("figures/gps_interval_med_sl.png", width = 3, height = 3)
#  
## table of wetland classification details ----
baari_tabs <- extract_tables("C:/Users/scott.jennings/Documents/Projects/hetp/analyses/Tomales_Great_Egret_habitat_selection_2020/derived_data/habitat/SFEI_MAPPING_STANDARDS_08092011_v8_0.pdf")


xwalk1 <- baari_tabs[[1]][4:22,] %>% 
  data.frame() %>% 
  dplyr::select(-X2) %>% 
  separate(X1, into = c("code", "classification"), sep = "\\s", extra = "merge", remove = T) %>% 
  rename(wetland.tracker.class = 3)

xwalk2 <- baari_tabs[[2]] %>% 
  data.frame() %>% 
  rename(code = 1, classification = 2, wetland.tracker.class = 3)%>% 
  filter(code != "STREAM NETWORK") %>% 
  mutate(classification = gsub("â€", "-", classification))

xwalk <- rbind(xwalk1, xwalk2)


cari <- read.csv("C:/Users/scott.jennings/Documents/Projects/hetp/analyses/Tomales_Great_Egret_habitat_selection_2020/derived_data/habitat/tomales_cari_attributes.csv") %>% 
  distinct(orig_class, clicklabel, legcode) 

write.csv(cari, "C:/Users/scott.jennings/Documents/Projects/hetp/analyses/Tomales_Great_Egret_habitat_selection_2020/documents/table1.csv")

baari <- read.csv("C:/Users/scott.jennings/Documents/Projects/hetp/analyses/Tomales_Great_Egret_habitat_selection_2020/derived_data/habitat/baari_map_standards.csv") %>% 
  mutate(wetland.tracker.class = gsub("egetated", "egetation", wetland.tracker.class))

baari_xwalk = full_join(baari, xwalk)

cari_baari <- left_join(rename(cari, parent.code = orig_class), baari) %>% 
  distinct() %>% 
  drop_na() %>% 
  filter(!wetland.tracker.class %in% c("Tidal Ditch", "Tidal Panne", "Fluvial Unvegetation Flat", "Fluvial Channel"))


write.csv(cari_baari, "C:/Users/scott.jennings/Documents/Projects/hetp/analyses/Tomales_Great_Egret_habitat_selection_2020/documents/table1.csv", row.names = F)

  
  
  
  
