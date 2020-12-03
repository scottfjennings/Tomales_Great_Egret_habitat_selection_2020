
library(tidyverse)



# calculate area of eelgrass coverage in tomales bay
eelgrass_table <- read.csv("C:/Users/scott.jennings/Documents/Projects/hetp/analyses/tomales_habitat_selection_2020/derived_data/habitat/eelgrass_2017_attribute_table.csv")

eelgrass_table %>% 
  filter(location == "Tomales Bay") %>% 
  summarise(eel.area = sum(area_m2)) %>% 
  mutate(eel.area.ha = eel.area/10000)
