
# add habitat classification to ODBA values
# can run all this as of 12/8/2020


# packages, source ----
library(tidyverse)
library(lubridate)
library(amt)
library(lme4)
library(nlme)
#options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")
source("code/utility_functions.r")

# read habitat data
tomales_habitat <- raster("derived_data/habitat/Marigear_Eelgrass_CARI_mo.tif")
hab_names_df <- read.csv("derived_data/habitat/Tomales_habitat_raster_key_2.csv") %>% 
  mutate(coarse.name = as.character(coarse.name))

tomales_dem_bathy <- raster("derived_data/habitat/tomales_dem_bathy_max.tif")

# no odba for GREG 6
wild_gregs <- filter(wild_gregs, bird != "GREG_6")

# read odba conversion data (convert mv to m/s/s)
odba_convert <- readRDS("derived_data/tag_axis_eqtn_values")


# read data with calculated ODBA, has UTM for closest timestamped GPS location.
# this created in hetp_data_work/code_HETP/data_management/add_covariates.R
hetp_odba <- readRDS("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/ready_for_analysis/hetp_for_odba")


# define functions ----
# function using amt::extract_covariates to assign 
make_odba_habitat <- function(zbird) {
odba_track <- hetp_odba %>% 
  filter(!is.na(odba.timestamp), !is.na(timestamp.x), bird == zbird) %>% 
  arrange(timestamp.x) %>% 
  mk_track(.x = utm.easting, .y = utm.northing, .t = timestamp.x, crs = sp::CRS("+init=epsg:32710"), all_cols = T)


odba_habitat <- odba_track %>% 
  amt::extract_covariates(tomales_habitat) %>% 
  amt::extract_covariates(tomales_dem_bathy) %>% 
  filter(!is.na(tomales_dem_bathy_max)) %>% 
  rename(habitat.type = Marigear_Eelgrass_CARI_mo,
         elevation = tomales_dem_bathy_max) 
}



# call functions and save output


all_greg_odba_habitat <- map_df(wild_gregs$bird, make_odba_habitat) %>% 
  full_join(., dplyr::select(hab_names_df, coarse.name, habitat.type = Value))%>% 
  mutate(coarse.name = ifelse(coarse.name %in% c("subtidal", "intertidal"), "other.tidal", coarse.name))


saveRDS(all_greg_odba_habitat, "derived_data/birds/odba_habitat")



# ODBA random effect for bird ----
odba_habitat <- readRDS("derived_data/birds/odba_habitat") %>% 
  rename(odba = odba.timestamp, habitat = coarse.name) %>% 
  mutate(zmonth = month(t_),
         zmonth = as.character(zmonth),
         habitat = as.factor(habitat),
         habitat = relevel(habitat, "other.tidal")) %>% 
  filter(!is.na(habitat), habitat != "freshwater.wetland")

odba_mod <- lmer(odba ~ habitat + (1 | bird), data = odba_habitat, REML = F)
odba_mod.lme <- lme(odba ~ habitat, random = ~1 | bird, data = odba_habitat)

odba_red <- lmer(odba ~ 1 + (1 | bird), data = odba_habitat, REML = F)

odba_lrt <- anova(odba_red,odba_mod, test = 'LRT')
# odba_mod gets better support

anova(odba_mod)

mod_coefs <- summary(odba_mod)$coefficients
mod_ci <- confint(odba_mod)


coef_ci <- full_join(mod_coefs %>% data.frame() %>% rownames_to_column("coefficient"),
                     mod_ci %>% data.frame() %>% rownames_to_column("coefficient")) %>% 
  filter(!is.na(Estimate)) %>% 
  dplyr::select(-Std..Error, -t.value) 

intercept <- filter(coef_ci, coefficient == "(Intercept)")$Estimate


estimates <- coef_ci %>% 
  mutate(est = ifelse(coefficient == "(Intercept)", Estimate, intercept[[1]] + Estimate),
         lwr = ifelse(coefficient == "(Intercept)", X2.5.., intercept[[1]] + X2.5..),
         upr = ifelse(coefficient == "(Intercept)", X97.5.., intercept[[1]] + X97.5..),
         habitat = ifelse(coefficient == "(Intercept)", "other.tidal", gsub("habitat", "", coefficient)),
         habitat = factor(habitat, levels = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh"),
                     labels = c("Other tidal", "Eelgrass", "Shellfish aquaculture", "Tidal marsh")))

ggplot(estimates) +
  geom_point(aes(x = habitat, y = est, color = habitat)) +
  geom_errorbar(aes(x = habitat, ymin = lwr, ymax = upr, color = habitat), width = 0.5) +
  scale_color_brewer(breaks = c("other.tidal", "eelgrass", "shellfish", "tidal.marsh"),
                     palette = "Set1") +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Wetland type") +
  ylab(bquote("Estimated ODBA "~ (m~ s^-1~s^-1)))


ggsave("figures/odba_fig.png", width = 8, height = 5, dpi = 300)
