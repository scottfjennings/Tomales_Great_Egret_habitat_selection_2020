




library(tidyverse)
library(amt)
library(survival)

source("code/utility_functions.r")

#exclude_birds <- c("GREG_4", "GREG_7", "GREG_9", "GREG_10", "GREG_11")

wild_gregs <- wild_gregs %>% 
  filter(!bird %in% c("GREG_4", "GREG_7", "GREG_9", "GREG_11")) # GREG_4 doesn't have enough points at all for analysis. 7, 9 and 11 don't have enough points in particular habitats (generally shellfish and subtidal) for coxph models.









full_habXmov_model_reader <- function(zbird) {
  full <- readRDS(paste("mod_objects/combined/", zbird, "_full_habXmov", sep = ""))
return(full)
  }

full_habXmov <- full_habXmov_model_reader("GREG_11")



# Make a new data.frame for s1
make_big_rss_est <- function(zbird) {
    full_habXmov <- readRDS(paste("mod_objects/combined/", zbird, "_full_habXmov", sep = ""))

    big_s1 <- expand.grid(depth.end = seq(-5, 3, by = 1),
                      habitat.end = c("intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh"),
                      habitat.start = c("intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh")) %>% 
  mutate(sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

big_s2 <- data.frame(
  depth.end = 0, 
  habitat.end = factor("subtidal", levels = c("intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh")),
  habitat.start = factor("subtidal", levels = c("intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh")),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

lr_g1_full <- log_rss(full_habXmov, big_s1, big_s2, ci = c("se"))$df %>% 
  mutate(bird = zbird)

}

big_rss <- make_big_rss_est("GREG_1")


big_rss <- map_df(wild_gregs$bird, make_big_rss_est)


make_rss_hab_est <- function(zbird, zdepth.end) {
  
  full <- readRDS(paste("mod_objects/combined/", zbird, "_full", sep = ""))
  
s1 <- data.frame(
  depth.end = zdepth.end,
  habitat.end = factor("eelgrass", levels = c("intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh")),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1,
  step_id_ = 2)

# data.frame for s2
s2 <- data.frame(
  depth.end = zdepth.end, 
  habitat.end = factor("shellfish", levels = c("intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh")),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1,
  step_id_ = 2)

# Calculate log-RSS
lr_full <- log_rss(full, s1, s2, ci = "se")$df %>% 
  mutate(bird = zbird)

return(lr_full)
}


bird_depth <- expand.grid(bird = wild_gregs$bird,
                          depth.end = seq(-5, 3, length.out = 20))

zzz <- map2_df(bird_depth$bird, bird_depth$depth.end, make_rss_hab_est)


zzz <- make_rss_hab_est("GREG_1", -1)


make_rss <- function(zbird) {
  
  full <- readRDS(paste("mod_objects/combined/", zbird, "_full", sep = ""))
  


x1 <- data.frame(
  depth.end = seq(from = -5, to = 3, length.out = 20),
  habitat.end = factor("eelgrass", levels = c("intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh")),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
x2 <- data.frame(
  depth.end = 0, # mean of elev, since we scaled and centered
  habitat.end = factor("eelgrass", levels = c("intertidal", "subtidal", "eelgrass", "shellfish", "tidal.marsh")),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS
lr_m2 <- log_rss(full, x1, x2, ci = "se")
}

xxx <- make_rss("GREG_1")
# Plot using ggplot2

ggplot(zzz) +
  geom_line(aes(x = ft2m(depth.end_x1), y = log_rss, color = habitat.end_x1)) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "gray30") +
  xlab("Elevation relative to current tide height (m)") +
  ylab("log-RSS") +
  theme_bw() +
  facet_wrap(~bird, scales = "free")


big_rss %>% 
  filter(habitat.end_x1 == habitat.start_x1, habitat.end_x1 %in% c("eelgrass", "shellfish")) %>% 
ggplot(aes(x = ft2m(depth.end_x1), y = log_rss)) +
  geom_line(aes(color = habitat.end_x1)) +
  #geom_line(aes(x = ft2m(depth.end_x1), y = lwr, color = habitat.end_x1), linetype = "dashed") +
  #geom_line(aes(x = ft2m(depth.end_x1), y = upr, color = habitat.end_x1), linetype = "dashed") +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = habitat.end_x1), alpha = 0.2) +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation relative to current tide height (m)") +
  ylab("log-RSS") +
  theme_bw() +
  facet_wrap(~bird, scales = "free")


big_rss %>% 
  filter(habitat.end_x1 == habitat.start_x1, habitat.end_x1 != "subtidal") %>% 
ggplot(aes(x = ft2m(depth.end_x1), y = log_rss)) +
  geom_line(aes(color = bird)) +
  geom_line(aes(x = ft2m(depth.end_x1), y = lwr, color = bird), linetype = "dashed") +
  geom_line(aes(x = ft2m(depth.end_x1), y = upr, color = bird), linetype = "dashed") +
  #geom_ribbon(aes(ymin = lwr, ymax = upr, fill = habitat.end_x1), alpha = 0.2) +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation relative to current tide height (m)") +
  ylab("log-RSS") +
  theme_bw() +
  facet_wrap(~habitat.end_x1, scales = "free")


