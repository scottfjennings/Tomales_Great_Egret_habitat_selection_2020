


# after 



library(tidyverse)
library(amt)
library(lubridate)
source("code/utility_functions.r")

#greg_dat_dist <- distinct(greg_dat)

# read data; from combined_analysis_1_prep_data.R ----

greg_steps_habitat <- readRDS("derived_data/amt_bursts/greg_steps_habitat")

# data cleaning ----

wild_gregs <- wild_gregs %>% 
  filter(!bird %in% c("GREG_4", "GREG_7", "GREG_9", "GREG_11")) # GREG_4 doesn't have enough points at all for analysis. 7, 9 and 11 don't have enough points in particular habitats (generally shellfish and subtidal) for coxph models.

#  need to fix the classification of some raster cells based on their elevation and tide heights. 
# Point Reyes lowest observed tide = -2.69; highest observed = 8.54
# lowest elevation in LiDAR DEM = -1.38
sub_inter_bound = -2.69



# I don't quite understand the math justification for the movement parms transformations, but this is what the amt peeps do in their papers

# positive values for depth.end are below current tide level

greg_steps_habitat <- greg_steps_habitat %>% 
  drop_na(c("habitat.start", "habitat.end", "elevation.start", "elevation.end")) %>% 
  filter(habitat.start != "freshwater.wetland", habitat.end != "freshwater.wetland") %>% 
  filter(bird != "GREG_4") %>% 
  filter(elevation.end < 10) %>% 
  mutate(habitat.start = as.character(habitat.start),
         habitat.start = ifelse(habitat.start == "intertidal" & elevation.start < sub_inter_bound, "subtidal", habitat.start),
         habitat.start = ifelse(habitat.start == "subtidal" & elevation.start >= sub_inter_bound, "intertidal", habitat.start)) %>% 
  mutate(habitat.end = as.character(habitat.end),
         habitat.end = ifelse(habitat.end == "intertidal" & elevation.end < sub_inter_bound, "subtidal", habitat.end),
         habitat.end = ifelse(habitat.end == "subtidal" & elevation.end >= sub_inter_bound, "intertidal", habitat.end)) %>% 
  mutate(depth.end = round(water.level_end, 2) - round(elevation.end, 2)) %>% 
  mutate(cos_ta_ = cos(ta_),
         log_sl_ = log(sl_)) %>% 
  mutate(habitat.start = as.factor(habitat.start),
         habitat.start = relevel(habitat.start, "subtidal"),
         habitat.end = as.factor(habitat.end),
         habitat.end = relevel(habitat.end, "subtidal"))

# data checking ----
# extract highest and lowest water depths for each habitat
habitat_depths <- greg_steps_habitat %>% 
  data.frame() %>% 
  group_by(habitat.end) %>% 
  summarise(min.depth = min(depth.end),
            max.depth = max(depth.end)) %>% 
  mutate(max.depth2 = floor(max.depth / 0.5) * 0.5)



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

# zbird = "GREG_11"
greg_steps_habitat %>%  
  data.frame() %>% 
  filter(bird == zbird) %>% 
  dplyr::group_by(case_, habitat.end) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% view()
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

# model fitting ----
# starting out with objective 1, comparing relative habitat selection between habitats.
# testing prediction that egrets experience or perceive eelgrass as more valuable than shellfish areas (higher selection for eelgrass)

# seems reasonable to only use the habXmov model for inference for both objectives - most coefficient estimates are very similar between the 2 models
# but, interpretation of habitat selection is easier without habitat.start in the model, so using mod without habitat:movement interaction for that


# need to fit models for each bird separately, hence functions and purrr::map() call
# this is following the examples in appendix B of Fieberg at al 2020
# use habitat at end of step as predictor of selection 
# see also Signer, J., J. Fieberg, and T. Avgar. 2019. Animal movement tools (amt): R package for managing tracking data and conducting habitat selection analyses. Ecology and Evolution 9:880–890.

# habitat * water depth^2 - does relative selection differ between habitats and does selection of optimal water depth differ between habitats?
fit_full_mod <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end * (depth.end + I(depth.end^2)) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

# 
saveRDS(mod, paste("mod_objects/combined/", zbird, "_full", sep = ""))
}


clog_mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  clogit(case_ ~ habitat.end * (depth.end + I(depth.end^2)) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

map(wild_gregs$bird, fit_full_mod)

# zmod1 <- fit_full_mod("GREG_1")
#

# model won't fit for 7, 9 with error:

# Error in fitter(X, Y, istrat, offset, init, control, weights = weights,  : 
#   NA/NaN/Inf in foreign function call (arg 5)
# In addition: Warning message:
# In fitter(X, Y, istrat, offset, init, control, weights = weights,  :
 
#  Error in fitter(X, Y, istrat, offset, init, control, weights = weights,  : 
#   NA/NaN/Inf in foreign function call (arg 5) 

# this seems to be because there aren't any points for one or more of the habitat categories (subtidal, shellfish)


# get this warning for 11
# Warning message:
# In fitter(X, Y, istrat, offset, init, control, weights = weights,  :
#   Ran out of iterations and did not converge

# this may also be because too few points in shellfish


# now fit the same candidate set of models but with the habitat:movement interactions ----
# these models include the interaction between starting habitat and movement parms to see if movement characteristics differ between habitats


fit_full_mod_hab.mov <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end * (depth.end + I(depth.end^2)) + 
             sl_ + log_sl_ + cos_ta_ + 
             habitat.start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)

# 
saveRDS(mod, paste("mod_objects/combined/", zbird, "_full_habXmov", sep = ""))
}


map(wild_gregs$bird, fit_full_mod_hab.mov)

# fit_full_mod_hab.mov("GREG_4")
#

# compare coefficients between models with and without the habitat:movement interactions ----

compare_coefs <- function(zbird) {

  full <- readRDS(paste("mod_objects/combined/", zbird, "_full", sep = ""))
  full_habXmov <- readRDS(paste("mod_objects/combined/", zbird, "_full_habXmov", sep = ""))


full_coef <- full$model$coefficients %>% 
  data.frame() %>% 
  rownames_to_column("varb") %>% 
  rename(full = 2)

full_habXmov_coef <- full_habXmov$model$coefficients %>% 
  data.frame() %>% 
  rownames_to_column("varb") %>% 
  rename(full_habXmov = 2)

comp_coefs <- full_join(full_coef, full_habXmov_coef, by = "varb") %>% 
  mutate(comp.coefs = exp(full) - exp(full_habXmov),
         bird = zbird)
}

comp_coefs_all <- map_df(wild_gregs$bird, compare_coefs)

comp_coefs_all %>% 
  filter(!is.na(comp.coefs)) %>% 
  ggplot() +
  geom_point(aes(x = bird, y = comp.coefs))



# check coefs and se in full mod
get_full_mod_coefs <- function(zbird) {

  full <- readRDS(paste("mod_objects/combined/", zbird, "_full", sep = ""))


full_coef <- summary(full)$coefficients %>% 
  data.frame() %>% 
  rownames_to_column("varb") %>% 
  mutate(bird = zbird)
}

full_mod_coefs <- map_df(wild_gregs$bird, get_full_mod_coefs)


