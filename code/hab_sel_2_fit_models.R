

library(tidyverse)
library(amt)
library(lubridate)
#source("code/utility_functions.r")
wild_gregs <- data.frame(bird = paste("GREG_", seq(1:11), sep = "")) %>% 
  filter(bird != "GREG_4")

#greg_dat_dist <- distinct(greg_dat)

# check data ----

hab_props <- read.csv("derived_data/habitat/habitat_proportions.csv")

zbird = "GREG_1"
greg_dat <- readRDS(paste("derived_data/amt_bursts/", zbird, "_burst", sep = "")) %>% 
  filter(inlight == T)

read_greg_dat <- function(zbird) {
all_greg_dat <- readRDS(paste("derived_data/amt_bursts/", zbird, "_burst", sep = "")) %>% 
  filter(inlight == T) %>% 
  mutate(bird = zbird)
}

all_greg_dat <- map_df(wild_gregs$bird, read_greg_dat)

all_greg_dat %>% 
  filter(elevation <= 10) %>% 
  group_by(habitat) %>% 
  summarise(min.depth = min(water.depth),
            max.depth = max(water.depth))

test_hab <- distinct(all_greg_dat, bird, habitat) %>% 
  data.frame() %>% 
  mutate(fff = 1) %>% 
  pivot_wider(id_cols = bird, names_from = habitat, values_from = fff)


# look at number of used vs available for all habitats
# this adapted from appendix A of Fieberg, J., Signer, J., Smith, B.J. and Avgar, T., 2020. A “How-to” Guide for Interpreting Parameters in Resource-and Step-Selection Analyses. bioRxiv.
# https://www.biorxiv.org/content/10.1101/2020.11.12.379834v1.abstract
all_greg_dat %>%  
  data.frame() %>% 
  dplyr::group_by(case_, habitat) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(habitat, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "habitat", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light() 

# naive used/available --
zzz <- table(all_greg_dat$habitat, all_greg_dat$case_) %>% 
  data.frame() %>% 
  rename(habitat = Var1, case = Var2, num.rec = Freq) %>%
  mutate(case = ifelse(case == "TRUE", "used", "avail")) %>% 
  pivot_wider(id_cols = habitat, names_from = case, values_from = num.rec) %>% 
  mutate(freq.use = used/sum(used),
         freq.avail = avail/sum(avail)) %>% 
  full_join(., hab_props)


zzz %>% 
  dplyr::select(habitat, freq.use, prop.hab) %>% 
  pivot_longer(cols = c(freq.use, prop.hab)) %>% 
  ggplot() +
  geom_col(aes(x = habitat, y = value, fill = name), position = "dodge2")


# how often are generated avail points all the same as the used point? --
fff <- distinct(all_greg_dat, step_id_, bird, case_, habitat) %>% 
  group_by(bird, step_id_, case_) %>% 
  mutate(znum = row_number(),
         zcase = ifelse(case_ == FALSE, paste("avail", znum, sep = "_"), "used")) %>% 
  ungroup()
fff_by_side <- full_join(dplyr::filter(fff, case_ == TRUE),
                         dplyr::filter(fff, case_ == FALSE) %>% drop_na(.),
                         by = c("bird", "step_id_")) %>% 
  mutate(ztest = habitat.x == habitat.y)



hab_test <- fff_by_side %>% 
  distinct(bird, step_id_, ztest) %>% 
  mutate(ztest = ifelse(ztest == TRUE, "yup", "nope"),
         fizz = 1) %>% 
  pivot_wider(id_cols = c("bird", "step_id_"), names_from = ztest, values_from = fizz)

all_greg_dat %>% 
  filter(elevation < 10, case_ == TRUE, habitat == "freshwater.wetland") %>% 
#  filter(correct.hab %in% c("subtidal")) %>% 
ggplot() +
  geom_point(aes(x = x2_, y = y2_, color = elevation))

greg_dat %>%  
  #filter(elevation < 10) %>% 
  #filter(!grepl("freshwater", habitat))  %>% 
  #filter(water.level >= -1, water.level <= 4) %>% 
  ggplot() +
  geom_histogram(aes(x = water.depth, fill = case_)) +
  facet_wrap(~ habitat)


greg_dat %>% 
  filter(water.depth < 2, water.depth > -5, !grepl("freshwater", habitat)) %>% 
  ggplot() +
  geom_histogram(aes(x = water.depth, fill = case_)) +
  facet_wrap(~ habitat)
# do filtering, check again
greg_dat_filt <- greg_dat %>% 
  filter(water.depth < 2, water.depth > -5, habitat != "freshwater.channel") %>%
  droplevels()

greg_dat_filt %>% 
  filter(water.depth < 2, water.depth > -5) %>% 
  ggplot() +
  geom_histogram(aes(x = water.depth, fill = habitat)) +
  facet_wrap(case_ ~ moon.simple)

table(greg_dat_filt$habitat, greg_dat_filt$moon.simple, greg_dat_filt$case_)

#
# model fitting ----

fit_save_hab_depth_mod <- function(zbird) {
  # read data --
  

greg_dat <- readRDS(paste("derived_data/amt_bursts/", zbird, "_burst", sep = "")) %>% 
  filter(inlight == T)

# filter --
greg_dat_filt <- greg_dat %>% 
  #filter(water.depth < 1, water.depth > -5, !grepl("freshwater", habitat)) %>%
  filter(!habitat %in% c("freshwater.wetland")) %>%
  droplevels() %>% 
  mutate(habitat = as.factor(habitat),
         habitat = relevel(habitat, "intertidal"))

# fit models --
# habitat X depth
hab.depth_mod <- greg_dat_filt  %>% 
  fit_ssf(case_ ~ habitat * water.depth + strata(step_id_))

# 
saveRDS(hab.depth_mod, paste("mod_objects/", zbird, "_hab.depth", sep = ""))

}


system.time(map(wild_gregs$bird, fit_save_hab_depth_mod))

# habitat only
fit_save_hab_mod <- function(zbird) {
  # read data --
greg_dat <- readRDS(paste("derived_data/amt_bursts/", zbird, "_burst", sep = "")) %>% 
  filter(inlight == T)
# filter --
greg_dat_filt <- greg_dat %>% 
  filter(water.depth < .5, water.depth > -5, !grepl("freshwater", habitat)) %>%
  #filter(habitat != "freshwater.channel", habitat != "freshwater.wetland") %>%
  droplevels() %>% 
  mutate(habitat = relevel(habitat, "subtidal"))

# habitat only
hab_mod <- greg_dat_filt  %>% 
  fit_ssf(case_ ~ habitat + strata(step_id_))

# 
saveRDS(hab_mod, paste("mod_objects/", zbird, "_hab", sep = ""))

}


fit_save_mods("GREG_2")

system.time(map(wild_gregs$bird, fit_save_hab_mod))


## predicted tide height ----

fit_save_hab_tide_mod <- function(zbird) {
  # read data --
greg_dat <- readRDS(paste("derived_data/amt_bursts/", zbird, "_burst", sep = "")) %>% 
  filter(inlight == T)
# filter --
greg_dat_filt <- greg_dat %>% 
  filter(!grepl("freshwater", habitat)) %>%
  #filter(water.level <= 2.5) %>% 
  droplevels() %>% 
  mutate(habitat = relevel(habitat, "subtidal"))

# fit models --
# habitat X depth
hab.tide_mod <- greg_dat_filt  %>% 
  fit_ssf(case_ ~ habitat * water.level + strata(step_id_))

# 
saveRDS(hab.tide_mod, paste("mod_objects/", zbird, "_hab.tide", sep = ""))
}

system.time(map(wild_gregs$bird, fit_save_hab_tide_mod))

