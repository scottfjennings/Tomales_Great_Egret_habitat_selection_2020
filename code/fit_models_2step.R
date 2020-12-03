
library(tidyverse)
library(amt)
library(lubridate)
library(TwoStepCLogit)
#source("code/utility_functions.r")
wild_gregs <- data.frame(bird = paste("GREG_", seq(1:11), sep = ""))

combine_greg_bursts <- function(zbird) {
greg_dat <- readRDS(paste("derived_data/amt_bursts/", zbird, "_burst", sep = "")) %>% 
  filter(inlight == T) %>% 
  mutate(bird = zbird)
}

all_greg_dat <- map_df(wild_gregs$bird, combine_greg_bursts)




all_mod <- Ts.estim(case_ ~ habitat * water.depth + strata(step_id_) + cluster(bird), data = all_greg_dat, random = ~bird)



tester <- all_greg_dat %>% 
  group_by(bird, step_id_) %>% 
  distinct(habitat) %>% 
  mutate(num.hab.strata = n())
