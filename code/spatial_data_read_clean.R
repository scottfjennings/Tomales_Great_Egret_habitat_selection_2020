



library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(RColorBrewer)
source("code/utility_functions.r")

# read combined habitat type raster _ this has the wrong eelgrass layer 2020/04/16
#tomales_habitat <- raster("derived_data/habitat/Marigear_Eelgrass_CARI_mo.tif")


# wetland type data ----
# ---
wetlands <- raster("derived_data/habitat/wetlands/wetlands.tif")

wetlands_rc <- reclassify(wetlands, cbind(0, NA))
wetlands_rc <- round(wetlands_rc, 0)

# ---
eelgrass <- raster("derived_data/habitat/eelgrass_2017_5mbuffer_raster/eelgrass_2017_5mbuffer.tif")
eelgrass_rc <- projectRaster(eelgrass, wetlands_rc, method = 'ngb')
#eelgrass_rc <- eelgrass
eelgrass_rc[eelgrass_rc > 0] <- 11
eelgrass_rc[eelgrass_rc < 11] <- NA


#values(eelgrass_rc) %>% data.frame() %>% rename(values = 1) %>% group_by(values) %>% summarise(num.each = n())
# ---
shellfish <- raster("derived_data/habitat/shellfish2/shellfish2.tif") 
shellfish <- projectRaster(shellfish, wetlands, method = 'ngb')
shellfish_rc <- shellfish
shellfish_rc[shellfish_rc > 0] <- 10
shellfish_rc <- reclassify(shellfish_rc, cbind(0, NA))
shellfish_rc <- round(shellfish_rc, 0)




tomales_wetlands <- merge(eelgrass_rc, shellfish_rc, wetlands_rc)

plot(tomales_wetlands, colNA = "red")

tomales_wetlands <- round(tomales_wetlands, 0)
values(tomales_wetlands) %>% data.frame() %>% rename(values = 1) %>% group_by(values) %>% summarise(num.each = n())

x = -257000
y = 25900
buf = 1000

crop(tomales_wetlands, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot(colNA = "red")

wetlands_key <- data.frame(Value = c(seq(1, 8), 10, 11),
                           wetland.type = c("tidal.marsh", "tidal.flat.marsh.panne", "subtidal", "pond.and.veg", "tidal.channel", "slope.seep", "fluvial", "pond", "shellfish", "eelgrass")) %>% 
  mutate(Value = as.numeric(Value))


# reclassifiy the raster; better to do it now at the beginning than several places later
tomales_wetlands[tomales_wetlands == 3] <- 2
tomales_wetlands[tomales_wetlands == 4] <- NA
tomales_wetlands[tomales_wetlands == 5] <- 2
tomales_wetlands[tomales_wetlands == 6] <- NA
tomales_wetlands[tomales_wetlands == 7] <- NA
tomales_wetlands[tomales_wetlands == 8] <- NA
tomales_wetlands[tomales_wetlands == 10] <- 3
tomales_wetlands[tomales_wetlands == 11] <- 4


writeRaster(tomales_wetlands, "derived_data/habitat/tomales_wetlands.tif", overwrite = TRUE)


# zzz <- getValues(tomales_habitat) %>% data.frame() %>% rename(habitat.num = 1) %>% group_by(habitat.num) %>% summarise(hab.num = n())


#hab_names_df <- read.csv("derived_data/habitat/Tomales_habitat_raster_key_2.csv") %>% 
  mutate(coarse.name = as.character(coarse.name))


 zzz <- getValues(tomales_wetlands) %>% 
   data.frame() %>% 
   rename(Value = 1) %>%  
   mutate(Value = round(Value, 0)) %>% 
   full_join(wetlands_key)
 
xxx <- table(zzz$wetland.type) %>% 
  data.frame() %>% 
  rename(habitat = 1, num.cells = 2) %>% 
  mutate(tot.cells = sum(num.cells),
         prop.hab = num.cells/tot.cells)

write.csv(xxx, "derived_data/habitat/habitat_proportions.csv", row.names = F)


hab_props <- read.csv("derived_data/habitat/habitat_proportions.csv")

# test plots to check classification
# plot(tomales_habitat_rclsfy)

# y = 4229000
# x = 505500
# buf = 500

# crop(tomales_habitat_rclsfy, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot()

# test_spdf <- as(tomales_habitat_rclsfy, "SpatialPixelsDataFrame")
# test_df <- as.data.frame(test_spdf) %>% rename(habitat = 1, utm.e = 2, utm.n = 3)

# test_df %>% 
#   filter(utm.e > x - buf, utm.e < x + buf, utm.n > y - buf, utm.n < y + buf, !habitat %in% c("10", "11")) %>% 
# ggplot() +  
#   geom_tile(aes(x=utm.e, y=utm.n, fill= as.factor(habitat)), alpha=0.8) +
#   scale_fill_brewer(type = "div") +
#   geom_point(data = greg_dat %>% filter(x2_ > x - buf, x2_ < x + buf, y2_ > y - buf, y2_ < y + buf), aes(x = x2_, y = y2_, color = habitat, shape = case_), size = 1)


# elevation data ----
# read LiDAR derived DEM

#tomales_dem <- raster("derived_data/habitat/Marin_DEM_2019_UTM_fullres_cliptoSJStudyAreaR1.tif") # elevation in feet
tomales_bathy <- raster("derived_data/habitat/tomales_bay_P110_2018.tif") # elevation in meters

#res(tomales_dem)
res(tomales_bathy)



tomales_bathy_utm <- projectRaster(tomales_bathy, crs = "+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#res(tomales_bathy_utm)


# make resolution match tomales_bay_P110_2018.tif -- old fact = c(17.73837, 22.52839)
tomales_dem_agg <- aggregate(tomales_dem, fact = c(17.73837, 22.52839), fun = max)
#res(tomales_dem_agg)
writeRaster(tomales_dem_agg, "derived_data/habitat/Marin_DEM_2019_UTM_fullres_cliptoSJStudyAreaR1_res8x10_utm_max.tif")
tomales_dem_agg <- raster("derived_data/habitat/Marin_DEM_2019_UTM_fullres_cliptoSJStudyAreaR1_res8x10_utm_meam.tif")



#tomales_dem2 <- projectRaster(tomales_dem, wetlands, method = 'bilinear')



# make all cells -0.85 and deeper = NA, want to use tomales_bay_P110_2018.tif values for those cells, but where elevation > -0.85, want to use whichever value is higher b/w tomales_bay_P110_2018.tif and tomales_dem_agg
tomales_dem_agg[tomales_dem_agg < 0] <- NA


tomales_bathy_utm_cut <- projectRaster(tomales_bathy_utm, tomales_dem_agg)

tomales_bathy_utm_cut_ft <- tomales_bathy_utm_cut * 3.281

tomales_bathy_utm_cut_ft[tomales_bathy_utm_cut_ft > 0] <- NA

#tomales_bathy_utm_cut_ft_disag <- disaggregate(tomales_bathy_utm_cut_ft, fact = c(0.05555556, 0.04347826))

tomales_dem_bathy_max <- mosaic(tomales_dem_agg, tomales_bathy_utm_cut_ft, fun = max, tolerance = 0.25)
#tomales_dem_bathy_min <- mosaic(tomales_dem_agg, tomales_bathy_utm_cut_ft, fun = min, tolerance = 0.25)
#tomales_dem_bathy_mean <- mosaic(tomales_dem_agg, tomales_bathy_utm_cut_ft, fun = mean, tolerance = 0.25)

tomales_dem_bathy_max[tomales_dem_bathy_max > 20] <- NA
tomales_dem_bathy_max[tomales_dem_bathy_max < -1.4] <- NA


writeRaster(tomales_dem_bathy_max, "derived_data/habitat/tomales_dem_bathy_max_0.tif", overwrite = TRUE)

plot(tomales_dem_bathy)
 y = 4229500
 x = 505000
 buf = 1000

#crop(tomales_dem_agg, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot()
#crop(tomales_bathy_utm_cut_ft, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot()
#crop(tomales_dem_bathy_min, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot(main = "min for overlapping cells")
crop(tomales_dem_bathy_max, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot(main = "max for overlapping cells", colNA = "red")

crop(tom_dem_test, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot()

# tomales_dem_data <- getValues(tomales_dem)

rm(tomales_habitat, hab_names_matrix)

