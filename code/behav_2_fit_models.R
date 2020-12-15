


library(tidyverse)
library(lme4)

source("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/code_HETP/data_management/hetp_utility_functions.r")
source("code/utility_functions.r")



# ODBA ----
odba_habitat <- readRDS("derived_data/birds/odba_habitat") %>% 
  rename(odba = odba.timestamp, habitat = coarse.name) %>% 
  mutate(zmonth = month(t_),
         zmonth = as.character(zmonth),
         habitat = as.factor(habitat),
         habitat = relevel(habitat, "subtidal")) %>% 
  filter(!is.na(habitat), habitat != "freshwater.wetland")

odba_mod <- lmer(odba ~ habitat + (1 | bird), data = odba_habitat, REML = F)
odba_red <- lmer(odba ~ 1 + (1 | bird), data = odba_habitat, REML = F)

odba_lrt <- anova(odba_red,odba_mod, test = 'LRT')

saveRDS(odba_mod, "mod_objects/behavior/odba_full")
saveRDS(odba_lrt, "mod_objects/behavior/odba_lrt")


## step lengths ----
steps_habitat <- readRDS("derived_data/birds/steps_habitat") %>% 
  filter(bird != "GREG_4")%>% # very few observations for this bird
  filter(!is.na(coarse.name.start), coarse.name.start != "freshwater.wetland") %>% 
  mutate(sl_ = sl_ + 0.01,
         coarse.name.start = as.factor(coarse.name.start),
         coarse.name.start = relevel(coarse.name.start, "subtidal"))


sl_mod <- glmer(sl_ ~ coarse.name.start + (1 | bird), family = Gamma(link="log"), data = steps_habitat)
sl_red <- glmer(sl_ ~ 1 + (1 | bird), family = Gamma(link="log"), data = steps_habitat)

odba_lrt <- anova(sl_red, sl_mod, test = 'LRT')


## turn angle ----
steps_habitat <- readRDS("derived_data/birds/steps_habitat") %>% 
  filter(bird != "GREG_4")%>% # very few observations for this bird
  filter(!is.na(coarse.name.start), coarse.name.start != "freshwater.wetland") %>% 
  mutate(sl_ = sl_ + 0.01,
         coarse.name.start = as.factor(coarse.name.start),
         coarse.name.start = relevel(coarse.name.start, "subtidal"))


sl_mod <- glmer(ta.deg.abs ~ coarse.name.start + (1 | bird), family = Gamma(link="log"), data = steps_habitat)
sl_red <- glmer(ta.deg.abs ~ 1 + (1 | bird), family = Gamma(link="log"), data = steps_habitat)

odba_lrt <- anova(sl_red, sl_mod, test = 'LRT')
