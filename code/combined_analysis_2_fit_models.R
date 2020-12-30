


# after 



library(tidyverse)
library(amt)
library(lubridate)
source("code/utility_functions.r")

#greg_dat_dist <- distinct(greg_dat)

# read data; from combined_analysis_1_prep_data.R ----

greg_steps_habitat <- readRDS("derived_data/amt_bursts/greg_steps_habitat")

# data cleaning

wild_gregs <- wild_gregs %>% 
  filter(!bird %in% c("GREG_4", "GREG_7", "GREG_9")) # see notes below for excluding these birds

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
  mutate(habitat.start = case_when(habitat.start %in% c("intertidal", "subtidal") & elevation.start < sub_inter_bound ~ "subtidal",
                             habitat.start %in% c("intertidal", "subtidal") & elevation.start >= sub_inter_bound ~ "intertidal",
                             TRUE ~ as.character(habitat.start)),
         habitat.end = case_when(habitat.end %in% c("intertidal", "subtidal") & elevation.end < sub_inter_bound ~ "subtidal",
                             habitat.end %in% c("intertidal", "subtidal") & elevation.end >= sub_inter_bound ~ "intertidal",
                             TRUE ~ as.character(habitat.start))) %>% 
  mutate(depth.end = round(water.level_end, 2) - round(elevation.end, 2)) %>% 
  mutate(cos_ta_ = cos(ta_),
         log_sl_ = log(sl_)) %>% 
  mutate(habitat.start = as.factor(habitat.start),
         habitat.start = relevel(habitat.start, "subtidal"),
         habitat.end = as.factor(habitat.end),
         habitat.end = relevel(habitat.end, "subtidal"))

# look at number of used vs available for all habitats ----
# this adapted from appendix A of Fieberg, J., Signer, J., Smith, B.J. and Avgar, T., 2020. A “How-to” Guide for Interpreting Parameters in Resource-and Step-Selection Analyses. bioRxiv.
# https://www.biorxiv.org/content/10.1101/2020.11.12.379834v1.abstract

# zbird = "GREG_2"
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


# starting out with objective 1, comparing relative habitat selection between habitats. ----
# testing prediction that egrets experience or percieve eelgrass as more valuable than shellfish areas (higher selection for eelgrass)

# need to fit models for each bird separately, hence functions and purrr::map() call
# this is following the examples in appendix B of Fieberg at al 2020
# use habitat at end of step as predictor of selection 
# see also Signer, J., J. Fieberg, and T. Avgar. 2019. Animal movement tools (amt): R package for managing tracking data and conducting habitat selection analyses. Ecology and Evolution 9:880–890.

# fitting 4 models to test 4 specific questions about habitat selection
# simpler linear water depth effect doesn't seem biologically likely so not fitting any models with that structure.
# only varying model structure for habitat and water depth; retaining all movement parms in all models.

# model 1 (fullest model) - habitat * water depth^2 - does relative selection differ between habitats and does selection of optimal water depth differ between habitats?
fit_full_mod <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end * (depth.end + I(depth.end^2)) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

# 
saveRDS(mod, paste("mod_objects/combined/", zbird, "_full", sep = ""))
}


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


# model 2 - habitat + water dept^2 - does relative selection differ between habitats and is there a single optimal water depth shared among all habitats?
fit_hab_depth2 <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end + depth.end + I(depth.end^2) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

saveRDS(mod, paste("mod_objects/combined/", zbird, "_hab_depth2", sep = ""))
}

map(wild_gregs$bird, fit_hab_depth2)
#


# model 3 - habitat - does relative selection differ between habitats, regardless of water depth?
fit_hab <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

# 
saveRDS(mod, paste("mod_objects/combined/", zbird, "_hab", sep = ""))
}

map(wild_gregs$bird, fit_hab)
#
# model 4 - water dept^2 - do egrets select for an optimal water depth, regardless of habitat?
fit_depth2 <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ depth.end + I(depth.end^2) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

saveRDS(mod, paste("mod_objects/combined/", zbird, "_depth2", sep = ""))
}

map(wild_gregs$bird, fit_depth2)
#

# model comparison for candidate models for first objective

compare_mods_obj1 <- function(zbird) {
  
  mod1 <- readRDS(paste("mod_objects/combined/", zbird, "_full", sep = ""))
  mod2 <- readRDS(paste("mod_objects/combined/", zbird, "_hab_depth2", sep = ""))
  mod3 <- readRDS(paste("mod_objects/combined/", zbird, "_hab", sep = ""))
  mod4 <- readRDS(paste("mod_objects/combined/", zbird, "_depth2", sep = ""))
  
  anova(mod1, mod2, mod3, mod4)
  
#aictab(list(mod1, mod2, mod3, mod4), modnames = c("hab.depth2", "hab_depth2", "hab", "depth2"))
  
  
}

compare_mods_obj1("GREG_1")

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
saveRDS(mod, paste("mod_objects/combined/", zbird, "_full_hab.mov", sep = ""))
}


map(wild_gregs$bird, fit_full_mod_hab.mov)

fit_full_mod_hab.mov("GREG_4")
#




# fit reduced models 

fit_hab_depth2_hab_mov <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end + depth.end + I(depth.end^2) + 
             sl_ + log_sl_ + cos_ta_ + 
             habitat.start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)

saveRDS(mod, paste("mod_objects/combined/", zbird, "_hab_depth2_hab_mov", sep = ""))
}


map(wild_gregs$bird, fit_hab_depth2_hab_mov)
#

fit_hab_depth_hab.mov <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end + depth.end + 
             sl_ + log_sl_ + cos_ta_ + 
             habitat.start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)

saveRDS(mod, paste("mod_objects/combined/", zbird, "_hab_depth_hab.mov", sep = ""))
}


map(wild_gregs$bird, fit_hab_depth_hab_mov)
#

fit_hab <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end + 
             sl_ + log_sl_ + cos_ta_ + 
             habitat.start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)

saveRDS(mod, paste("mod_objects/combined/", zbird, "_hab_depth_hab.mov", sep = ""))
}


map(wild_gregs$bird, fit_hab)
#

#
#look at how many used/avail points there are for each bird X habitat ----
greg_steps_habitat %>%  
  data.frame() %>% 
  dplyr::group_by(bird, case_, habitat.end) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(case_ = ifelse(case_ == TRUE, "used", "avail")) %>% 
  pivot_wider(names_from = c("case_"), values_from = c("n")) %>% 
  mutate(num.points = paste(used, avail, sep = " / ")) %>% 
  dplyr::select(-used, -avail) %>% 
  pivot_wider(names_from = habitat.end, values_from = num.points) %>% 
  view()
