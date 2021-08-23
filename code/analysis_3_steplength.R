




library(tidyverse)
library(amt)
library(survival)
library(AICcmodavg)
library(gridExtra)
source("code/utility_functions.r")
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/shift_label.R")

#exclude_birds <- c("GREG_4", "GREG_7", "GREG_9", "GREG_10", "GREG_11")

wild_gregs <- wild_gregs %>% 
  filter(!bird %in% c("GREG_4", "GREG_7", "GREG_9", "GREG_11")) # GREG_4 doesn't have enough points at all for analysis. 7, 9 and 11 don't have enough points in particular habitats (generally shellfish and subtidal) for coxph models.
# read data; from combined_analysis_1_prep_data.R ----

greg_steps_habitat <- readRDS("derived_data/amt_bursts/greg_steps_habitat") 

greg_steps_habitat <- greg_steps_habitat %>% 
  filter(depth.end < 1)
# objective 2a ----
# evidence of differences in step length between habitats?
# now add habitat:movement interactions to best model from analysis_2_logrss.R
# these models include the interaction between starting habitat and movement parms to see if movement characteristics differ between habitats


fit_full_mod_hab.mov <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ wetland.end * (depth.end + I(depth.end^2)) + 
             sl_ + log_sl_ + cos_ta_ + 
             wetland.start:(sl_ + log_sl_ + ta_) +
             strata(step_id_), model = TRUE)

# 
saveRDS(mod, paste("mod_objects/combined/", zbird, "_full_habXmov", sep = ""))
}

#map(wild_gregs$bird, fit_full_mod_hab.mov)

# then just the interaction between wetland.start and step length
fit_full_mod_habXsl <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ wetland.end * (depth.end + I(depth.end^2)) + 
             sl_ + log_sl_ + cos_ta_ + 
             wetland.start:(sl_ + log_sl_) +
             strata(step_id_), model = TRUE)

# 
saveRDS(mod, paste("mod_objects/combined/", zbird, "_full_habXsl", sep = ""))
}

map(wild_gregs$bird, fit_full_mod_habXsl)

# and finally the interaction between wetland.start and turn angle
fit_full_mod_habXta <- function(zbird) {
mod <- greg_steps_habitat %>% 
  filter(bird == zbird) %>% 
  fit_issf(case_ ~ wetland.end * (depth.end + I(depth.end^2)) + 
             sl_ + log_sl_ + cos_ta_ + 
             wetland.start:cos_ta_ +
             strata(step_id_), model = TRUE)

# 
saveRDS(mod, paste("mod_objects/combined/", zbird, "_full_habXta", sep = ""))
}

#map(wild_gregs$bird, fit_full_mod_habXta)

# and do AIC model comparison for for those 3 new models plus the best model from objective 1 ----
 
compare_mods_obj2a <- function(zbird) {

  hab.depth2 <- readRDS(paste("mod_objects/combined/", zbird, "_habXdepth2", sep = ""))
  #habXmov <- readRDS(paste("mod_objects/combined/", zbird, "_full_habXmov", sep = ""))
  habXsl <- readRDS(paste("mod_objects/combined/", zbird, "_full_habXsl", sep = ""))
  #habXta <- readRDS(paste("mod_objects/combined/", zbird, "_full_habXta", sep = ""))

mod_comp <- aictab(list(hab.depth2$model, habXsl$model), modnames = c("hab.depth2", "habXsl")) %>% 
  data.frame() %>% 
  mutate(bird = zbird)
}

all_aic_obj2a <- map_df(wild_gregs$bird, compare_mods_obj2a)

saveRDS(all_aic_obj2a, "mod_objects/aic/step2a_aic")

filter(all_aic_obj2a, Delta_AICc == 0) %>% view()

# get D AICc of all second best models
filter(all_aic_obj2a, Delta_AICc != 0) %>% group_by(bird) %>%  filter(Delta_AICc == min(Delta_AICc)) %>% arrange(Delta_AICc) %>% view()

# save object with best model name for each bird. for use below adjusting movement parms
filter(all_aic_obj2a, Delta_AICc == 0) %>% 
  dplyr::select(bird, Modnames) %>% 
  saveRDS("mod_objects/aic/best_mod_names")

newdat_sl_ = 128
newdat_cos_ta_ = 1



# testing out estimates for rss ----
# Make a new data.frame for s1
make_big_rss_est <- function(zbird) {
    habXsl <- readRDS(paste("mod_objects/combined/", zbird, "_full_habXsl", sep = ""))

    big_s1 <- expand.grid(depth.end = seq(-5, 3, by = 0.1),
                      wetland.end = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh")
                      ) %>% 
  mutate(wetland.end = factor(wetland.end,
                    levels = levels(habXsl$model$model$wetland.end)),
         wetland.start = wetland.end,
                    sl_ = newdat_sl_,
         log_sl_ = log(newdat_sl_),
         cos_ta_ = newdat_cos_ta_)

big_s2 <- data.frame(
  depth.end = 0, 
  wetland.end = factor("other.tidal", 
                    levels = levels(habXsl$model$model$wetland.end)),
  sl_ = newdat_sl_,
  log_sl_ = log(newdat_sl_),
  cos_ta_ = newdat_cos_ta_) %>% 
  mutate(wetland.start = wetland.end)

lr_g1_full <- log_rss(habXsl, big_s1, big_s2, ci = c("se"))$df %>% 
  mutate(bird = zbird)

}

big_rss <- make_big_rss_est("GREG_1")


big_rss <- map_df(wild_gregs$bird, make_big_rss_est)




big_rss %>% 
  mutate(depth = ft2m(depth.end_x1)) %>% 
  #filter(!(wetland.end_x1 == "subtidal" & depth < 0)) %>% 
  filter(!(wetland.end_x1 == "shellfish" & depth < -1.2)) %>% 
ggplot(aes(x = depth, y = log_rss)) +
  geom_line(aes(color = wetland.end_x1)) +
  geom_ribbon(aes(x = depth, ymin = lwr, ymax = upr, fill = wetland.end_x1), alpha = 0.2) +
  scale_color_brewer(name = "Wetland type",
                       breaks = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh"),
                     labels = c("Other tidal", "Eelgrass", "Shellfish aquaculture", "Tidal marsh"),
                     palette = "Dark2") +
  scale_fill_brewer(name = "Wetland type",
                       breaks = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh"),
                     labels = c("Other tidal", "Eelgrass", "Shellfish aquaculture", "Tidal marsh"),
                     palette = "Dark2") +
  xlab("Tide-dependent water depth (m)\n note: negative values indicate elevation above current water level") +
  ylab("log-Relative Selection Strength") +
  theme_bw() +
  facet_wrap(~bird, scales = "free")

# overal results and conclusions are the same as the model without the wetland.start * sl_ interaction, but interpretation of rss coefs is still somewhat tricky when wetland.start is also included in the models

# so, sticking with evaluating relative selection strength and step lengths with different models.


# plot model estimates for movement parameters ----
# this adapted from appendix b of the how to guide

# "intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh"
# wetland.starteelgrass                   
# wetland.startshellfish                  
# wetland.startsubtidal
# wetland.starttidal.marsh

adjust_move_parms <- function(zbird) {
  
    best_mod_name <- readRDS("mod_objects/aic/best_mod_names") %>% 
  filter(bird == zbird)
  
  best_mod <- readRDS(paste("mod_objects/combined/", zbird, "_full_", best_mod_name$Modnames, sep = ""))
  
# first step lengths
# intertidal step-length distribution
other.tidal_sl <- update_gamma(
  dist = best_mod$sl_,
  beta_sl = best_mod$model$coefficients["sl_"],
  beta_log_sl = best_mod$model$coefficients["log_sl_"])


# eelgrass step-length distribution
eelgrass_sl <- update_gamma(
  dist = best_mod$sl_,
  beta_sl = best_mod$model$coefficients["sl_"] +
    best_mod$model$coefficients["sl_:wetland.starteelgrass"],
  beta_log_sl = best_mod$model$coefficients["log_sl_"] +
    best_mod$model$coefficients["log_sl_:wetland.starteelgrass"])

# shellfish step-length distribution
shellfish_sl <- update_gamma(
  dist = best_mod$sl_,
  beta_sl = best_mod$model$coefficients["sl_"] +
    best_mod$model$coefficients["sl_:wetland.startshellfish"],
  beta_log_sl = best_mod$model$coefficients["log_sl_"] +
    best_mod$model$coefficients["log_sl_:wetland.startshellfish"])

# tidal.marsh step-length distribution
tidal.marsh_sl <- update_gamma(
  dist = best_mod$sl_,
  beta_sl = best_mod$model$coefficients["sl_"] +
    best_mod$model$coefficients["sl_:wetland.starttidal.marsh"],
  beta_log_sl = best_mod$model$coefficients["log_sl_"] +
    best_mod$model$coefficients["log_sl_:wetland.starttidal.marsh"])


#Now, we can plot the original and updated distributions for each habitat

# data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 400))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 1, to = 400, length.out = 400)

# y-axis is the probability density under the given gamma distribution
# intertidal
plot_sl$other.tidal <- dgamma(x = plot_sl$x, 
                         shape = other.tidal_sl$params$shape,
                         scale = other.tidal_sl$params$scale)
# subtidal
#plot_sl$subtidal <- dgamma(x = plot_sl$x, 
#                        shape = subtidal_sl$params$shape,
#                        scale = subtidal_sl$params$scale)
# eelgrass
plot_sl$eelgrass <- dgamma(x = plot_sl$x, 
                      shape = eelgrass_sl$params$shape,
                      scale = eelgrass_sl$params$scale)
# shellfish
plot_sl$shellfish <- dgamma(x = plot_sl$x, 
                      shape = shellfish_sl$params$shape,
                      scale = shellfish_sl$params$scale)
# tidal.marsh
plot_sl$tidal.marsh <- dgamma(x = plot_sl$x, 
                      shape = tidal.marsh_sl$params$shape,
                      scale = tidal.marsh_sl$params$scale)
# Pivot from wide to long data
plot_sl <- plot_sl %>% 
  pivot_longer(cols = -x) %>% 
  rename(step.length = x, wetland.start = name)%>% 
  mutate(wetland.start = factor(wetland.start,
                                levels = levels(best_mod$model$model$wetland.start))) %>% 
  mutate(bird = zbird)
}

adjusted_sl <- map_df(wild_gregs$bird, adjust_move_parms)

# Plot
plot_adjusted_sl <- adjusted_sl %>% 
  mutate(wetland.label = case_when(wetland.start == "eelgrass" ~ "Eelgrass",
                                   wetland.start == "shellfish" ~ "Shellfish aquaculture",
                                   wetland.start == "tidal.marsh" ~ "Tidal marsh",
                                   wetland.start == "other.tidal" ~ "Other tidal"),
         wetland.label = factor(wetland.label, levels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"))) %>%
  filter(step.length < 100) %>% 
ggplot(aes(x = step.length, y = value)) +
  geom_line(aes(color = wetland.label)) +
  scale_color_brewer(name = "Wetland type",
                       breaks = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     labels = c("Eelgrass", "Shellfish aquaculture", "Tidal marsh", "Other tidal"),
                     palette = "Dark2") +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() +
  facet_wrap(~bird)

ggsave("figures/step_length_fig.png", width = 8, height = 5, dpi = 300)

out_plot <- grid.arrange(shift_legend(plot_adjusted_sl))


ggsave("figures/step_length_fig_7x5.png", out_plot, width = 7, height = 5, dpi = 300)
dev.off()




