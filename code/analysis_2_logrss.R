





library(tidyverse)
library(amt)
library(lubridate)
library(here)
library(AICcmodavg)
library(gridExtra)
library(RColorBrewer)
source("code/utility_functions.r")
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/shift_label.R")
#greg_dat_dist <- distinct(greg_dat)

wild_gregs <- wild_gregs %>% 
  filter(!bird %in% c("GREG_4", "GREG_7", "GREG_9", "GREG_11")) # GREG_4 doesn't have enough points at all for analysis. 7, 9 and 11 don't have enough points in particular habitats (generally shellfish and subtidal) for coxph models.

# read data; from analysis_1_prep_data.R ----

greg_steps_habitat <- readRDS("derived_data/amt_bursts/greg_steps_habitat")
# nrow = 613021
greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::summarise(quants = quantile(depth.end, probs = c(0.025, 0.95))) %>% 
  mutate(depth.quant = rep(c("lwr", "upr"))) 


greg_steps_habitat <- greg_steps_habitat %>% 
  filter(between(depth.end, -0.5, 1))
# nrow = 498226
depth_quants <- greg_steps_habitat %>% 
  data.frame() %>% 
  filter(case_ == TRUE) %>% 
  dplyr::group_by(wetland.end) %>% 
  dplyr::summarise(quants = quantile(depth.end, probs = c(0.025, 0.975))) %>% 
  mutate(depth.quant = rep(c("lwr.depth", "upr.depth"))) %>% 
  pivot_wider(id_cols = wetland.end, names_from = depth.quant, values_from = quants) %>% 
  rename(wetland.end_x1 = wetland.end)

# mean step length and turn angle in greg_steps_habitat
newdat_sl_ = 128
newdat_cos_ta_ = 0



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
fit_hab.depth2 <- function(zbird) {
hab.depth2 <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ wetland.end * (depth.end + I(depth.end^2)) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

# 
saveRDS(hab.depth2, paste("mod_objects/combined/", zbird, "_habXdepth2", sep = ""))
}


map(wild_gregs$bird, fit_hab.depth2)

#fit_hab.depth2("GREG_1")

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
 
hab_depth2 <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ wetland.end + depth.end + I(depth.end^2) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)
 
saveRDS(hab_depth2, paste("mod_objects/combined/", zbird, "_hab_depth2", sep = ""))
 
}
 
map(wild_gregs$bird, fit_hab_depth2)
#
# model 3 - habitat - does relative selection differ between habitats, regardless of water depth?
fit_hab <- function(zbird) {
 
hab <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ wetland.end + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)
# 
saveRDS(hab, paste("mod_objects/combined/", zbird, "_hab", sep = ""))
}
 
map(wild_gregs$bird, fit_hab)
#
# model 4 - water dept^2 - do egrets select for an optimal water depth, regardless of habitat?
fit_depth2 <- function(zbird) {
depth2 <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ depth.end + I(depth.end^2) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

saveRDS(depth2, paste("mod_objects/combined/", zbird, "_depth2", sep = ""))
}

map(wild_gregs$bird, fit_depth2)

#
# adding 3 models in response to reviewer concerns about fitting quadratic and not linear
fit_hab.depth <- function(zbird) {
hab.depth <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ wetland.end * depth.end + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)
 
saveRDS(hab.depth, paste("mod_objects/combined/", zbird, "_habXdepth", sep = ""))
}
map(wild_gregs$bird, fit_hab.depth)

fit_hab_depth <- function(zbird) {
hab_depth <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ wetland.end + depth.end + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

saveRDS(hab_depth, paste("mod_objects/combined/", zbird, "_hab_depth", sep = ""))
}
map(wild_gregs$bird, fit_hab_depth)

fit_depth <- function(zbird) {
depth <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ depth.end + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

saveRDS(depth, paste("mod_objects/combined/", zbird, "_depth", sep = ""))
}
map(wild_gregs$bird, fit_depth)


# model comparison for candidate models for first objective ----
 
compare_mods_obj1 <- function(zbird) {

  hab.depth2 <- readRDS(paste("mod_objects/combined/", zbird, "_habXdepth2", sep = ""))
  hab_depth2 <- readRDS(paste("mod_objects/combined/", zbird, "_hab_depth2", sep = ""))
  hab <- readRDS(paste("mod_objects/combined/", zbird, "_hab", sep = ""))
  depth2 <- readRDS(paste("mod_objects/combined/", zbird, "_depth2", sep = ""))
  # 3 added models
  hab.depth <- readRDS(paste("mod_objects/combined/", zbird, "_habXdepth", sep = ""))
  hab_depth <- readRDS(paste("mod_objects/combined/", zbird, "_hab_depth", sep = ""))
  depth <- readRDS(paste("mod_objects/combined/", zbird, "_depth", sep = ""))
  
  mod_comp <- aictab(list(hab.depth2$model, hab_depth2$model, hab$model, depth2$model, hab.depth$model, hab_depth$model, depth$model), modnames = c("hab.depth2", "hab_depth2", "hab", "depth2", "hab.depth", "hab_depth", "depth")) %>% 
  data.frame() %>% 
  mutate(bird = zbird)
}

all_aic <- map_df(wild_gregs$bird, compare_mods_obj1)

saveRDS(all_aic, "mod_objects/aic/step1_aic")
all_aic <- readRDS("mod_objects/aic/step1_aic")

#view best model for each bird
filter(all_aic, Delta_AICc == 0) %>% view()
# hab.depth2 is best for all k = 17

# get D AICc of all second best models
filter(all_aic, Delta_AICc != 0) %>% group_by(bird) %>%  filter(Delta_AICc == min(Delta_AICc)) %>% view()
# deltaAICc for 2nd ranked models are between 166.8560 - 1741.5529. hab_depth2 is 2nd ranked for all birds (k = 9)



# read in best models
read_best_mods <- function(zbird) {
  hab.depth2 <- readRDS(paste("mod_objects/combined/", zbird, "_habXdepth2", sep = ""))
}

best_mods <- map(wild_gregs$bird, read_best_mods)

summary(best_mods[[1]])

# calculate and plot relative selection strength from best model ----



# first using a fixed value for depth in the reference level ----
make_big_rss_est <- function(zbird) {
    habXdepth2 <- readRDS(paste("mod_objects/combined/", zbird, "_habXdepth2", sep = ""))

    big_s1 <- expand.grid(depth.end = seq(-1.7, 2, by = 0.1),
                      wetland.end = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh")
                      ) %>% 
  mutate(wetland.end = factor(wetland.end,
                    levels = levels(habXdepth2$model$model$wetland.end)),
                    sl_ = newdat_sl_,
         log_sl_ = log(newdat_sl_),
         cos_ta_ = newdat_cos_ta_)

big_s2 <- data.frame(
  depth.end = 0, 
  wetland.end = factor("other.tidal", 
                    levels = levels(habXdepth2$model$model$wetland.end)),
  sl_ = newdat_sl_,
  log_sl_ = log(newdat_sl_),
  cos_ta_ = newdat_cos_ta_)

lr_g1_full <- log_rss(habXdepth2, big_s1, big_s2, ci = c("se"))$df %>% 
  mutate(bird = zbird)

}

big_rss <- make_big_rss_est("GREG_3")


big_rss <- map_df(wild_gregs$bird, make_big_rss_est)





plot_rss <- big_rss %>% 
  data.frame() %>% 
  mutate(wetland.label = case_when(wetland.end_x1 == "eelgrass" ~ "Eelgrass",
                                   wetland.end_x1 == "shellfish" ~ "Shellfish aquaculture",
                                   wetland.end_x1 == "tidal.marsh" ~ "Tidal marsh",
                                   wetland.end_x1 == "other.tidal" ~ "Other tidal"),
         wetland.label = factor(wetland.label, levels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"))) %>% 
  arrange(wetland.end_x1, depth.end_x1)

ggplot(plot_rss) +
  geom_point(aes(x = wetland.label, y = depth.end_x1)) +
  facet_wrap(~bird)

ggplot(plot_rss) +
  geom_line(aes(x = depth.end_x1, y = log_rss, color = wetland.end_x1)) +
  geom_ribbon(aes(x = depth.end_x1, ymin = lwr, ymax = upr, fill = wetland.label), alpha = 0.2) +
  scale_color_brewer(name = "Wetland type",
                       breaks = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     labels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     palette = "Set1") +
  scale_fill_brewer(name = "Wetland type",
                       breaks = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     labels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     palette = "Set1") +
  xlab("Tide-dependent water depth (m)\nnegative values indicate elevation above current water level") +
  ylab("log-Relative Selection Strength") +
  theme_bw() +
  facet_wrap(~bird)

ggsave("figures/log_rss_fig.png", width = 8, height = 5, dpi = 300)


# second, with varying depth for the reference level - THIS IS THE FIGURE USED IN THE PAPER ----
newdat_sl_ = 128
newdat_cos_ta_ = 0

make_depthvar_rss_est <- function(zbird, zdepth.end) {
    habXdepth2 <- readRDS(here(paste("mod_objects/combined/", zbird, "_habXdepth2", sep = "")))

    big_s1 <- expand.grid(depth.end = zdepth.end,
                      wetland.end = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh")
                      ) %>% 
  mutate(wetland.end = factor(wetland.end,
                    levels = levels(habXdepth2$model$model$wetland.end)),
                    sl_ = newdat_sl_,
         log_sl_ = log(newdat_sl_),
         cos_ta_ = newdat_cos_ta_)

big_s2 <- data.frame(
  depth.end = zdepth.end, 
  wetland.end = factor("other.tidal", 
                    levels = levels(habXdepth2$model$model$wetland.end)),
  sl_ = newdat_sl_,
  log_sl_ = log(newdat_sl_),
  cos_ta_ = newdat_cos_ta_)

lr_g1_full <- log_rss(habXdepth2, big_s1, big_s2, ci = c("se"))$df %>% 
  mutate(bird = zbird,
         depth.end_x2 = zdepth.end)

}

# big_rss <- make_depthvar_rss_est("GREG_1", 0)


wild_greg_depthvar <- expand.grid(bird = wild_gregs$bird,
                                  depth.end = seq(-0.5, 1, by = 0.1)) %>% 
  data.frame() %>% 
  mutate(bird = as.character(bird))

depthvar_rss <- map2_df(wild_greg_depthvar$bird, wild_greg_depthvar$depth.end, make_depthvar_rss_est)

depthvar_rss %>% 
  filter(depth.end_x1 == 0.5, wetland.end_x1 == "shellfish") %>% 
  mutate(rss = exp(log_rss)) %>% 
  view()



depthvar_plot <- depthvar_rss  %>% 
  rename(depth = depth.end_x1) %>% 
  mutate(wetland.label = case_when(wetland.end_x1 == "eelgrass" ~ "Eelgrass",
                                   wetland.end_x1 == "shellfish" ~ "Shellfish aquaculture",
                                   wetland.end_x1 == "tidal.marsh" ~ "Tidal marsh",
                                   wetland.end_x1 == "other.tidal" ~ "Other tidal"),
         wetland.label = factor(wetland.label, levels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"))) %>%
  ggplot() +
  geom_line(aes(x = depth, y = log_rss, color = wetland.label)) +
  geom_ribbon(aes(x = depth, ymin = lwr, ymax = upr, fill = wetland.label), alpha = 0.2) +
  scale_color_brewer(name = "Wetland type",
                       breaks = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     labels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     palette = "Dark2") +
  scale_fill_brewer(name = "Wetland type",
                       breaks = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     labels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     palette = "Dark2") +
  xlab("Tide-dependent water depth (m)") +
  ylab("log-Relative Selection Strength") +
  theme_bw() +
  facet_wrap(~bird)



png("figures/depthvar_log_rss_fig.png", width = 7, height = 5, units = "in", res = 300)


out_plot <- grid.arrange(shift_legend(depthvar_plot))


ggsave("figures/Fig2.tiff", out_plot, width = 7, height = 5, dpi = 300)
dev.off()

# sanity check, see how predicted rss changes if change the habitat type in s2 ----
make_depthvar_rss_est <- function(zbird, zdepth.end) {
    habXdepth2 <- readRDS(paste("mod_objects/combined/", zbird, "_habXdepth2", sep = ""))

    big_s1 <- expand.grid(depth.end = zdepth.end,
                      wetland.end = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh")
                      ) %>% 
  mutate(wetland.end = factor(wetland.end,
                    levels = levels(habXdepth2$model$model$wetland.end)),
                    sl_ = newdat_sl_,
         log_sl_ = log(newdat_sl_),
         cos_ta_ = newdat_cos_ta_)

big_s2 <- data.frame(
  depth.end = zdepth.end, 
  wetland.end = factor("other.tidal", 
                    levels = levels(habXdepth2$model$model$wetland.end)),
  sl_ = newdat_sl_,
  log_sl_ = log(newdat_sl_),
  cos_ta_ = newdat_cos_ta_)

lr_g1_full <- log_rss(habXdepth2, big_s1, big_s2, ci = c("se"))$df %>% 
  mutate(bird = zbird,
         depth.end_x2 = zdepth.end)

}

big_rss <- make_depthvar_rss_est("GREG_1", 0)


wild_greg_depthvar <- expand.grid(bird = wild_gregs$bird,
                                  depth.end = seq(-5, 3, by = 0.1)) %>% 
  data.frame() %>% 
  mutate(bird = as.character(bird))

depthvar_rss_eel_s2 <- map2_df(wild_greg_depthvar$bird, wild_greg_depthvar$depth.end, make_depthvar_rss_est)

depthvar_rss_eel_s2 %>% 
  mutate(depth = ft2m(depth.end_x1)) %>% 
  #filter(!(wetland.end_x1 == "subtidal" & depth < 0)) %>% 
  filter(!(wetland.end_x1 == "shellfish" & depth < -1.2)) %>% 
ggplot(aes(x = depth, y = log_rss)) +
  geom_line(aes(color = wetland.end_x1)) +
  geom_ribbon(aes(x = depth, ymin = lwr, ymax = upr, fill = wetland.end_x1), alpha = 0.2) +
  scale_color_brewer(name = "Wetland type",
                       breaks = c("other.tidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh"),
                     labels = c("Other tidal", "Subtidal", "Eelgrass", "Shellfish aquaculture", "Tidal marsh"),
                     palette = "Set1") +
  scale_fill_brewer(name = "Wetland type",
                       breaks = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh"),
                     labels = c("Other tidal", "Eelgrass", "Shellfish aquaculture", "Tidal marsh"),
                     palette = "Set1") +
  xlab("Tide-dependent water depth (m)") +
  ylab("log-Relative Selection Strength") +
  theme_bw() +
  facet_wrap(~bird, scales = "free")

# yup, results are as expected


# create scaling values for egret image size in scematic of rss ----
wild_greg_depthvar <- expand.grid(bird = wild_gregs$bird,
                                  depth.end = m2ft(c(-0.75, 0, 0.75))) %>% 
  data.frame() %>% 
  mutate(bird = as.character(bird))

depthvar_rss <- map2_df(wild_greg_depthvar$bird, wild_greg_depthvar$depth.end, make_depthvar_rss_est)

mean_rss <- depthvar_rss %>% 
  ungroup() %>% 
  group_by(wetland.end_x1, depth.end_x1) %>%
  summarise(mean.log.rss = mean(log_rss)) %>% 
  ungroup() %>% 
  mutate(depth.end_x1 = ft2m(depth.end_x1),
         scale.mean.log.rss = (mean.log.rss + abs(min(mean.log.rss)) + 0.01)/abs(min(mean.log.rss)),
         egret.scaler = 3 * scale.mean.log.rss)

saveRDS(mean_rss, "derived_data/rss_scematic_scaling")

mean_rss <- readRDS("derived_data/rss_scematic_scaling") %>% 
  mutate(egret.scaler = egret.scaler/2)
ggplot(mean_rss) +
  geom_line(aes(x = depth.end_x1, y = mean.log.rss, color = wetland.end_x1))



