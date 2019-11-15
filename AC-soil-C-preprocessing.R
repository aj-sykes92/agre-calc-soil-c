# packages
library(raster)
library(tidyverse)
library(sp)

data_repo <- "AgRE Calc PLC/"

UK <- find_onedrive(dir = data_repo, path = "GIS data/DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

# read in soil raster data and stack
Soil_stack <- stack(find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Soil pH/Fixed/PHIHOX_M_sl4_5km_ll.tif") %>% raster(), # pH
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif"), # sand %
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Silt content/Fixed/SLTPPT_M_sl4_5km_ll.tif"), # silt %
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Clay content/Fixed/CLYPPT_M_sl4_5km_ll.tif"), # clay %
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/OC tonnes per ha/Fixed/OCSTHA_M_sd4_5km_ll.tif")) # OC tonnes per ha

# read in crop area raster data and stack
readdir <- find_onedrive(dir = data_repo, path = "GIS data/MapSPAM data/Physical area")
file.names <- dir(readdir, pattern =".tif")

Crop_area_stack <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Crop_area_stack <- addLayer(Crop_area_stack, x)
  rm(x)
  print(file.names[i])
}

# read in crop yield raster data and stack
readdir <- find_onedrive(dir = data_repo, path = "GIS data/MapSPAM data/Yield")
file.names <- dir(readdir, pattern =".tif")

Crop_yield_stack <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Crop_yield_stack <- addLayer(Crop_yield_stack, x)
  rm(x)
  print(file.names[i])
}
rm(readdir, file.names, readpath, i)

# add area layer
Area <- Soil_stack[[1]] %>% area()
Soil_stack <- addLayer(Soil_stack, Area)
rm(Area)

# aggregate
Master_stack <- stack(Soil_stack, Crop_area_stack, Crop_yield_stack)
rm(Soil_stack, Crop_area_stack, Crop_yield_stack)

# mask out to UK only
Master_stack <- Master_stack %>% crop(UK) %>% mask(UK)
plot(Master_stack[[1]])
# plot(UK, add=T)

# convert to dataframe
Dat_main <- Master_stack %>% as.data.frame(xy = T) %>% drop_na(PHIHOX_M_sl4_5km_ll)
# we have zeros, >0s and NAs in the area data, NAs and >0s only in the yield data