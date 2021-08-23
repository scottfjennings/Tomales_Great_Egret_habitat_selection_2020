
# add habitat classification to ODBA values
# can run all this as of 12/8/2020


# packages, source ----
library(tidyverse)
library(lubridate)
library(amt)
library(lme4)
library(nlme)
library(raster)
#options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")
source("code/utility_functions.r")
# no odba for GREG 6
wild_gregs <- filter(wild_gregs, bird != "GREG_6")

# read habitat data ----


tomales_dem_bathy <- raster("derived_data/habitat/tomales_dem_bathy_mean.tif")
tomales_dem_bathy_m <- ft2m(tomales_dem_bathy)


tomales_wetlands <- raster("derived_data/habitat/tomales_wetlands.tif")

wetlands_key <- data.frame(Value = seq(1, 4),
                           wetland.type = c("tidal.marsh", "other.tidal", "shellfish", "eelgrass")) %>% 
  mutate(Value = as.numeric(Value))

tomales_wetlands <- projectRaster(tomales_wetlands, crs = proj4string(tomales_dem_bathy), method = "ngb")

# read odba conversion data (convert mv to m/s/s)
odba_convert <- readRDS("derived_data/tag_axis_eqtn_values")


# read data with calculated ODBA, has UTM for closest timestamped GPS location.
# this created in hetp_data_work/code_HETP/data_management/add_covariates.R
hetp_odba <- readRDS("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/ready_for_analysis/hetp_for_odba")


# define functions ----
# function using amt::extract_covariates to assign 
make_odba_wetlands <- function(zbird) {
odba_track <- hetp_odba %>% 
  filter(!is.na(odba.timestamp), !is.na(timestamp.x), bird == zbird) %>% 
  arrange(timestamp.x) %>% 
  mk_track(.x = utm.easting, .y = utm.northing, .t = timestamp.x, crs = sp::CRS("+init=epsg:32710"), all_cols = T)


odba_wetlands <- odba_track %>% 
  amt::extract_covariates(tomales_wetlands) %>% 
  amt::extract_covariates(tomales_dem_bathy_m) %>% 
  filter(!is.na(tomales_dem_bathy_mean)) %>% 
  rename(wetland.num = tomales_wetlands,
         elevation = tomales_dem_bathy_mean) 
}



# call functions and save output ----


all_greg_odba_wetlands <- map_df(wild_gregs$bird, make_odba_wetlands) %>% 
  full_join(., rename(wetlands_key, wetland.num = Value)) %>% 
  filter(!is.na(wetland.type))


saveRDS(all_greg_odba_wetlands, "derived_data/birds/odba_wetlands")



# ODBA random effect for bird ----
odba_wetlands <- readRDS("derived_data/birds/odba_wetlands") %>% 
  data.frame() %>% 
  rename(odba = odba.timestamp) %>% 
  mutate(wetland.type = as.factor(wetland.type),
         wetland.type = relevel(wetland.type, "other.tidal"),
         bird = as.factor(bird)) 

odba_mod <- lmer(odba ~ wetland.type + (1 | bird), data = odba_wetlands, REML = F)
odba_mod.lme <- lme(odba ~ wetland.type, random = ~1 | bird, data = odba_wetlands)

odba_red <- lmer(odba ~ 1 + (1 | bird), data = odba_wetlands, REML = F)

odba_lrt <- anova(odba_red,odba_mod, test = 'LRT')
# odba_mod gets better support

anova(odba_mod)

mod_coefs <- summary(odba_mod)$coefficients
mod_ci <- confint(odba_mod)


coef_ci <- full_join(mod_coefs %>% data.frame() %>% rownames_to_column("coefficient"),
                     mod_ci %>% data.frame() %>% rownames_to_column("coefficient")) %>% 
  filter(!is.na(Estimate)) %>% 
  dplyr::select(-Std..Error, -t.value) %>% 
  mutate(out.text = paste(round(Estimate, 1), ", 95% CI ", round(X2.5.., 1), " to ", round(X97.5.., 1), sep = ""))

intercept <- filter(coef_ci, coefficient == "(Intercept)")$Estimate


estimates <- coef_ci %>% 
  mutate(est = ifelse(coefficient == "(Intercept)", Estimate, intercept[[1]] + Estimate),
         lwr = ifelse(coefficient == "(Intercept)", X2.5.., intercept[[1]] + X2.5..),
         upr = ifelse(coefficient == "(Intercept)", X97.5.., intercept[[1]] + X97.5..),
         wetland = ifelse(coefficient == "(Intercept)", "other.tidal", gsub("wetland.type", "", coefficient))) %>% 
  mutate(wetland.label = case_when(wetland == "eelgrass" ~ "Eelgrass",
                                   wetland == "shellfish" ~ "Shellfish\naquaculture",
                                   wetland == "tidal.marsh" ~ "Tidal marsh",
                                   wetland == "other.tidal" ~ "Other tidal"),
         wetland.label = factor(wetland.label, levels = c("Eelgrass", "Shellfish\naquaculture", "Tidal marsh", "Other tidal"))) 

ggplot(estimates) +
  geom_point(aes(x = wetland.label, y = est, color = wetland), size = 3) +
  geom_errorbar(aes(x = wetland.label, ymin = lwr, ymax = upr, color = wetland), size = 1.5, width = 0.5) +
  scale_color_brewer(breaks = c("Eelgrass", "Shellfish\naquaculture", "Tidal marsh", "Other tidal"),
                     palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=11),
        axis.text = element_text(size=11)) +
  xlab("\nWetland type") +
  scale_y_continuous(limits = c(400, 1000), breaks = seq(400, 1000, by = 100)) +
  ylab(bquote("Estimated ODBA "~ (m~ s^-1~s^-1)))


ggsave("figures/odba_fig_5x4.png", width = 5, height = 4, dpi = 300)
