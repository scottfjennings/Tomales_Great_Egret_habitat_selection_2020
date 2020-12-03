



library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(RColorBrewer)
source("code/utility_functions.r")

# read combined habitat type raster
tomales_habitat <- raster("derived_data/habitat/Marigear_Eelgrass_CARI_mo.tif")

# zzz <- getValues(tomales_habitat) %>% data.frame() %>% rename(habitat.num = 1) %>% group_by(habitat.num) %>% summarise(hab.num = n())


hab_names_df <- read.csv("derived_data/habitat/Tomales_habitat_raster_key_2.csv") %>% 
  mutate(coarse.name = as.character(coarse.name))


 zzz <- getValues(tomales_habitat) %>% 
   data.frame() %>% 
   rename(Value = 1) %>%  
   full_join(hab_names_df)
 
xxx <- table(zzz$coarse.name) %>% 
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

# read LiDAR derived DEM

tomales_dem <- raster("derived_data/habitat/Marin_DEM_2019_UTM_fullres_cliptoSJStudyAreaR1.tif")

# make resolution match tomales_bay_P110_2018.tif
tomales_dem_agg <- aggregate(tomales_dem, fact = c(17.73837, 22.52839))

# make all cells -0.85 and deeper = NA, want to use tomales_bay_P110_2018.tif values for those cells, but where elevation > -0.85, want to use whichever value is higher b/w tomales_bay_P110_2018.tif and tomales_dem_agg
tomales_dem_agg[tomales_dem_agg < -0.85] <- NA

tomales_bathy <- raster("derived_data/habitat/tomales_bay_P110_2018.tif")
#tomales_bathy[tomales_bathy >= 0] <- NA
tomales_bathy_utm <- projectRaster(tomales_bathy, crs = crs(tomales_dem))

tomales_bathy_utm_cut <- projectRaster(tomales_bathy_utm, tomales_dem_agg)

tomales_bathy_utm_cut_ft <- tomales_bathy_utm_cut * 3.281
#tomales_bathy_utm_cut_ft_disag <- disaggregate(tomales_bathy_utm_cut_ft, fact = c(0.05555556, 0.04347826))

tomales_dem_max_bathy <- mosaic(tomales_dem_agg_max, tomales_bathy_utm_cut_ft, fun = min, tolerance = 0.25)
tomales_dem_min_bathy <- mosaic(tomales_dem_agg_min, tomales_bathy_utm_cut_ft, fun = min, tolerance = 0.25)
tomales_dem_bathy <- mosaic(tomales_dem_agg, tomales_bathy_utm_cut_ft, fun = min, tolerance = 0.25)
tomales_dem_bathy_max <- mosaic(tomales_dem_agg, tomales_bathy_utm_cut_ft, fun = max, tolerance = 0.25)

tomales_dem_bathy[tomales_dem_bathy > 20] <- NA


writeRaster(tomales_dem_bathy_max, "derived_data/habitat/tomales_dem_bathy_max.tif", overwrite = TRUE)

plot(tomales_dem_bathy)
 y = 4230000
 x = 503250
 buf = 500

crop(tomales_dem_agg, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot()
crop(tomales_bathy_utm_cut_ft, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot()
crop(tomales_dem_bathy, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot(main = "min for overlapping cells")
crop(tomales_dem_bathy_max, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot(main = "max for overlapping cells")


crop(tom_dem_test, extent(c(x - buf, x + buf, y - buf, y + buf))) %>% plot()

# tomales_dem_data <- getValues(tomales_dem)

rm(tomales_habitat, hab_names_matrix)

