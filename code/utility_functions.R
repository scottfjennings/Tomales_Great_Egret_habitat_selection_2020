
# packages ----
library(tidyverse)
library(lubridate)
library(move)

source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")


#  list of tagged bird ids
wild_gregs <- data.frame(bird = paste("GREG_", seq(1:11), sep = ""))

# flight speed for filtering steps. 
# from BNA account: upper flight speed = 50 km/h; av speeds 35.8 km/h and 41.8 km/h
# Brzorad et al 2004: GREG foraging speed = 0.04 m/s (SNEG = 0.13)

# divide by 3.6 to convert km/h to m/s
# speed buffer is a multiplier to buffer below the average flight speed, currently using 0.5, so we keep all steps that slower than half the (lower reported) average flight speed

speed.buffer = 0.5
lowest_flight_speed_ms = (35.8 * speed.buffer)/ 3.6

# habitat raster classifications
make_hab_names_df <- function() {
hab_names_matrix <- matrix(c(-999, "NA", "NA", 10,  "NA",
                             0, "NA", "NA", 11, "NA",
                             1, 1, "shellfish.footprint", 1, "shellfish.footprint", 
                             100, 2, "eelgrass", 2,  "eelgrass", 
                             11000, 3, "estuarine.intertidal", 3, "intertidal", 
                             15000, 4, "tidal.marsh", 3, "intertidal", 
                             18000, 5, "estuarine.subtidal", 4, "subtidal", 
                             21000, 6, "pond.and.vegetation", 5, "freshwater.wetland", 
                             22000, 6, "pond.and.vegetation", 5, "freshwater.wetland", 
                             24000, 6, "pond.and.vegetation", 5, "freshwater.wetland", 
                             28000, 6, "pond.and.vegetation", 5, "freshwater.wetland", 
                             29000, 7, "slope.seep", 5, "freshwater.wetland", 
                             30000, 8, "fluvial.channel", 5, "freshwater.wetland", 
                             39000, 9, "estuarine.pond", 3, "intertidal", 
                             40000, 10, "tidal.channel", 4, "subtidal", 
                             44000, 11, "fluvial.channel", 6, "freshwater.channel"), ncol = 5, byrow=TRUE)


hab_names_df <- data.frame(hab_names_matrix) %>% 
  rename(old.num = 1, new.num = 2, fine.name = 3, coarse.num = 4, coarse.name = 5) %>% 
  mutate(old.num = as.numeric(as.character(old.num)), new.num = as.numeric(as.character(new.num)), coarse.num = as.numeric(as.character(coarse.num)))
}
hab_names_df <- make_hab_names_df()

ft2m <- function(x) { x  / 3.28084}
m2ft <- function(x) { x  * 3.28084}



## model utility functions ----
get_mod_coefs <- function(zbird) {

  mod <- readRDS(paste("mod_objects/", zbird, "_tom2km_mod.rds", sep = ""))
  
coef_names <-  mod$model$coefficients %>% 
  data.frame() %>% 
  rename(coef = 1) %>% 
  rownames_to_column("coef.name") 
  
  mod_coefs <- summary(mod)$coefficients %>% 
  data.frame() %>% 
  mutate(bird = zbird)
  
  out <- full_join(coef_names, mod_coefs)
}

# foo <- get_mod_coefs("GREG_1")
# mod_coefs <- map_df(gregs$zbird, get_mod_coefs)

# mod_coefs2 <- mod_coefs %>% 
#   mutate(lci = exp(coef - (se.coef. * 1.96)),
#          uci = exp(coef + (se.coef. * 1.96))) %>% 
#   select(bird, coef.name, exp.coef., lci, uci) %>% 
#   left_join(., select(cari_leglab, coef.name = cari, habitat = leglabellevel1) %>% distinct()) %>% 
#   mutate(habitat = as.character(habitat),
#          habitat = ifelse(is.na(habitat) & grepl("eel", coef.name), "Eelgrass", habitat),
#          habitat = ifelse(is.na(habitat) & grepl("shell", coef.name), "Shellfish", habitat))


# ggplot(data = mod_coefs2, aes(x = habitat, y = exp.coef., group = bird, col = bird)) +
#   geom_pointrange(aes(ymin = lci, ymax = uci), position = position_dodge(width = 0.7), size = 0.2) +
#   geom_hline(yintercept = 1, lty = 2) +
#   labs(x = "Habitat", y = "Relative Selection Strength") +
#   theme_light() 


# ggsave("figures/preliminar_habitat_selection.png", height = 3, width = 12)
