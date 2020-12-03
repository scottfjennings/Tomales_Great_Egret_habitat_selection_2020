

library(tidyverse)
library(amt)
library(survival)

source("code/utility_functions.r")

#exclude_birds <- c("GREG_4", "GREG_7", "GREG_9", "GREG_10", "GREG_11")

exclude_birds <- c("GREG_4") # 4 is the only one with very few points


zbird = "GREG_7"

# model reading functions ----
read_hab.depth_coefs <- function(zbird) {
  
hab.depth_mod <- readRDS(paste("mod_objects/", zbird, "_hab.depth", sep = ""))
zzz <- summary(hab.depth_mod)
zback_coefs_ci <- zzz$coefficients %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  mutate(bird = zbird,
         mod = "hab.depth")

}



read_hab.depth_backtranscoefs <- function(zbird) {
  
hab.depth_mod <- readRDS(paste("mod_objects/", zbird, "_hab.depth", sep = ""))

zzz <- summary(hab.depth_mod) 

zback_coefs_ci <- zzz$conf.int %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  mutate(bird = zbird,
         mod = "hab.depth")

}

read_hab.tide_backtranscoefs <- function(zbird) {
  
hab.tide_mod <- readRDS(paste("mod_objects/", zbird, "_hab.tide", sep = ""))

zzz <- summary(hab.tide_mod) 

zback_coefs_ci <- zzz$conf.int %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  mutate(bird = zbird,
         mod = "hab.tide")

}

read_hab_backtranscoefs <- function(zbird) {
  
hab_mod <- readRDS(paste("mod_objects/", zbird, "_hab", sep = ""))

zzz <- summary(hab_mod) 

zback_coefs_ci <- zzz$conf.int %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  mutate(bird = zbird,
         mod = "hab")

}

read_mod_vcov <- function(zbird){
  
hab.depth_mod <- readRDS(paste("mod_objects/", zbird, "_hab.depth", sep = ""))

vb <- hab.depth_mod$model$var

}

# read in model coefficients (calling functions defined above) ----

all_coefs <- map_df(wild_gregs$bird, read_hab.depth_coefs) # doesn't include 95% ci

all_coefs_backtrans <- map_df(wild_gregs$bird, read_hab.depth_backtranscoefs) %>% # includes 95% ci
  rename(exp.coef = exp.coef., exp.neg.coef = exp..coef., lci = lower..95, uci = upper..95)

#all_coefs_backtrans <- map_df(wild_gregs$bird, read_hab.tide_backtranscoefs) %>% # includes 95% ci
#  rename(exp.coef = exp.coef., exp.neg.coef = exp..coef., lci = lower..95, uci = upper..95)

#all_coefs_backtrans <- map_df(wild_gregs$bird, read_hab_backtranscoefs) %>% # includes 95% ci
#  rename(exp.coef = exp.coef., exp.neg.coef = exp..coef., lci = lower..95, uci = upper..95)

#
# basic plotting main effects only (water depth = 0) ----
exclude_birds <- c("GREG_4", "GREG_7", "GREG_9", "GREG_10", "GREG_11")
exclude_birds <- c("GREG_4", "GREG_7", "GREG_9", "GREG_11")
exclude_birds <- c("GREG_4", "GREG_9")
exclude_birds <- c("GREG_4")
exclude_birds <- c("")

mean_selection <- all_coefs_backtrans  %>% 
  filter(!grepl("water.depth", variable), !bird %in% exclude_birds) %>% 
  dplyr::select(variable, exp.coef) %>% 
  filter(!is.na(exp.coef)) %>% 
   nest(data = exp.coef) %>% 
   mutate(boot = map(data, ~ replicate(1e3, mean(sample(.$exp.coef, 8, replace = TRUE)))), 
          mean.coef = map_dbl(boot, mean), 
          lci.grand = map_dbl(boot, quantile, prob = 0.025), 
          uci.grand = map_dbl(boot, quantile, prob = 0.975)) %>% 
  rownames_to_column("hab.num") %>% 
  mutate(habitat = c("Eelgrass", "Aquaculture \nfootprint", "Subtidal", "Tidal marsh"),
         hab.num = as.numeric(as.character(hab.num))) %>% 
  dplyr::select(-data, -boot)

  
all_coefs_backtrans  %>% 
  filter(!grepl("water.depth", variable), !bird %in% exclude_birds) %>% 
  full_join(., mean_selection) %>% 
  ggplot(aes(x = habitat, y = exp.coef, color = bird)) +
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x = habitat, ymin = lci, ymax = uci),
                width = 0.5,
                position=position_dodge(width=0.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") + 
  geom_point(aes(x = habitat, y = mean.coef), size = 2, color = "black") +
  geom_errorbar(aes(ymin = lci.grand, ymax = uci.grand),
                width = 0.5, color = "black") +
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("Relative selection strength") +
  xlab("") +
  #scale_y_continuous(breaks = seq(-1, 6, by = 1)) +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=12),
        axis.text=element_text(colour="black"))


ggsave("figures/relative_selection_strength_clear_4_5.png", height = 4, width = 5, units = "in", bg = "transparent")



# plotting selection at multiple water depths ----
exclude_birds <- c("")
exclude_birds <- c("GREG_4")

main_coefs <- all_coefs_backtrans  %>% 
  filter(!bird %in% exclude_birds, !grepl("water.depth", variable)) %>% 
  dplyr::select(variable, exp.coef, bird)

water_depth_main <- all_coefs_backtrans %>% 
  filter(!bird %in% exclude_birds, variable == "water.depth")%>% 
  dplyr::select(depth.coef = exp.coef, bird)


interaction_coefs <- all_coefs_backtrans  %>% 
  filter(!bird %in% exclude_birds, variable != "water.depth") %>% 
  dplyr::select(variable, exp.coef, bird) %>% 
  mutate(parsed.varb = gsub(":water.depth", "", variable),
         interac.coef = ifelse(grepl(":water.depth", variable), exp.coef, NA)) %>% 
  dplyr::select(parsed.varb, interac.coef, bird) %>% 
  rename(variable = parsed.varb) %>% 
  filter(!is.na(interac.coef))
  


zbird = distinct(main_coefs, bird) %>% 
  mutate(bird = as.character(bird))

zvariable = distinct(main_coefs, variable) %>% 
  mutate(variable = as.character(variable),
         habitat = c("Eelgrass", "Aquaculture \nfootprint", "Subtidal", "Tidal marsh"))

zwater_depth = data.frame(water.depth = seq(-5, 3, by = 1))


depths <- expand.grid(bird = zbird$bird,
                      water.depth = zwater_depth$water.depth)


selection_all_depths <- full_join(main_coefs, water_depth_main, by = c("bird")) %>% 
  full_join(., interaction_coefs, by = c("bird", "variable")) %>% 
  full_join(., depths) %>% 
  mutate(full.selection = exp.coef + (water.depth * depth.coef) + (water.depth * interac.coef),
         ref.selection = 1 + (water.depth * depth.coef))


mean_selection_all_depths <- selection_all_depths  %>% 
  filter(!grepl("water.depth", variable), !bird %in% exclude_birds) %>% 
  dplyr::select(variable, full.selection, water.depth) %>% 
  filter(!is.na(full.selection)) %>% 
  group_by(variable, water.depth) %>% 
   nest(data = full.selection) %>% 
   mutate(boot = map(data, ~ replicate(1e3, mean(sample(.$full.selection, 8, replace = TRUE)))), 
          mean.coef = map_dbl(boot, mean), 
          lci.grand = map_dbl(boot, quantile, prob = 0.025), 
          uci.grand = map_dbl(boot, quantile, prob = 0.975)) %>% 
  dplyr::select(-data, -boot) %>% 
  rownames_to_column("hab.num") %>% 
    full_join(zvariable) %>% 
  mutate(hab.num = as.numeric(as.character(hab.num)))

mean_refselection_all_depths <- selection_all_depths  %>% 
  filter(!grepl("water.depth", variable), !bird %in% exclude_birds) %>% 
  dplyr::select(ref.selection, water.depth) %>% 
  group_by(water.depth) %>% 
   nest(data = ref.selection) %>% 
   mutate(boot = map(data, ~ replicate(1e3, mean(sample(.$ref.selection, 8, replace = TRUE)))), 
          mean.ref = map_dbl(boot, mean)) %>% 
  dplyr::select(-data, -boot) %>%
  mutate(water.depth = (-1 * water.depth))



selection_all_depths  %>% 
  full_join(mean_selection_all_depths) %>% 
  ggplot(aes(x = (-1 * water.depth), y = full.selection, color = bird)) +
  geom_line() +
  geom_hline(aes(yintercept = 1), linetype = "dashed") + 
  geom_point(aes(x = (-1 * water.depth), y = mean.coef), size = 2, color = "black") +
  geom_errorbar(aes(ymin = lci.grand, ymax = uci.grand),
                width = 0.5, color = "black") + 
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("Relative selection strength") +
  xlab("Elevation relative to current tide level") +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=12),
        axis.text=element_text(colour="black")) + 
  facet_wrap(~variable)


selection_all_depths  %>% 
  full_join(mean_selection_all_depths) %>%
  mutate(water.depth = (-1 * water.depth)) %>% 
  filter(!(variable == "habitatsubtidal" & water.depth > 0)) %>% 
  ggplot() +
  geom_point(aes(x = water.depth, y = mean.coef, color = habitat), size = 2, position=position_dodge(width=0.25)) +
  geom_line(aes(x = water.depth, y = mean.coef, color = habitat), position=position_dodge(width=0.25)) +
  geom_errorbar(aes(x = water.depth, ymin = lci.grand, ymax = uci.grand, color = habitat), position=position_dodge(width=0.25),
                width = 0.5) + 
  #geom_point(data = mean_refselection_all_depths, aes(x = (-1 * water.depth), y = mean.ref)) +
  geom_segment(data = mean_refselection_all_depths, aes(x = water.depth - 0.25, xend = water.depth + 0.25, y = mean.ref, yend = mean.ref), size = 1) +
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("Relative selection strength") +
  xlab("Elevation relative to current tide level (ft)") +
  theme(legend.title = element_blank(),
        #legend.position = "none",
        legend.background = element_rect(color = "white"),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=12),
        axis.text=element_text(colour="black"),
        panel.grid = element_line(color = "gray60")) +
  #scale_y_continuous(breaks = seq(-8, 8, by = 2)) +
  scale_x_continuous(breaks = seq(-3, 5))
ggsave("figures/relative_selection_strength_alldepths_clear_3_4_noleg_2m.png", height = 3, width = 4, units = "in", bg = "transparent")


selection_all_depths  %>% 
  full_join(mean_selection_all_depths) %>% 
  ggplot() +
  geom_point(aes(x = habitat, y = full.selection, color = bird)) +
  geom_hline(aes(yintercept = ref.selection), linetype = "dashed") + 
  geom_point(aes(x = habitat, y = mean.coef), size = 2, color = "black") +
  geom_errorbar(aes(x = habitat, ymin = lci.grand, ymax = uci.grand),
                width = 0.5, color = "black") + 
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("Relative selection strength") +
  xlab("") +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=12),
        axis.text=element_text(colour="black")) + 
  facet_wrap(~(-1 * water.depth))
