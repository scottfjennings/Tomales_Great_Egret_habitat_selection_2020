


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
  filter(!bird %in% c("GREG_4", "GREG_7", "GREG_9"))
#  need to fix the classification of some raster cells based on their elevation and tide heights. 
# Point Reyes lowest observed tide = -2.69; highest observed = 8.54
# lowest elevation in LiDAR DEM = -1.38
sub_inter_bound = -2.69

# I don't quite understand the math justification for the movement parms transformations, but this is what the amt peeps do in their papers

# positive values for depth.end are 

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
         habitat.start = relevel(habitat.start, "intertidal"),
         habitat.end = as.factor(habitat.end),
         habitat.end = relevel(habitat.end, "intertidal"))

# look at number of used vs available for all habitats ----
# this adapted from appendix A of Fieberg, J., Signer, J., Smith, B.J. and Avgar, T., 2020. A “How-to” Guide for Interpreting Parameters in Resource-and Step-Selection Analyses. bioRxiv.
# https://www.biorxiv.org/content/10.1101/2020.11.12.379834v1.abstract

zbird = "GREG_11"
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



# fitting a model with interactions between movement parameters and habitat, plus interaction of habitat and water depth
# this is following the examples in appendix B of Fieberg at al 2020
# movement parms interact with habitat at start of step, but use habitat at end of step as predictor of selection (see also Signer, J., J. Fieberg, and T. Avgar. 2019. Animal movement tools (amt): R package for managing tracking data and conducting habitat selection analyses. Ecology and Evolution 9:880–890.)


# fit the full model ----
# in this full model, including the interaction between habitat and the quadratic of water depth to see if birds optimize a certain depth and if that differs between habitats.
# also include the interaction between starting habitat and movement parms to see if movement characteristics differ between habitats


fit_full_mod <- function(zbird) {
full <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ habitat.end*depth.end*I(depth.end^2) + 
             sl_ + log_sl_ + cos_ta_ + 
             habitat.start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)

# 
saveRDS(full, paste("mod_objects/combined/", zbird, "_full", sep = ""))
}


map(wild_gregs$bird, fit_full_mod)

fit_full_mod("GREG_11")
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

#look at how many used/avail points there are for each bird X habitat
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
