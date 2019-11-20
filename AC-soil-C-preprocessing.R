# packages
library(raster)
library(tidyverse)
library(sp)
library(rgdal)

data_repo <- "GIS data repository"

UK <- find_onedrive(dir = data_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

# below code is commented out — not req'd since masking to UK only need happen once
soil_wrb <- find_onedrive(dir = data_repo, path = "SoilGrids250/TAXNWRB_250m_ll_UKonly.tif") %>% raster()

# read in soil raster data
#soil_wrb <- find_onedrive(dir = data_repo, path = "SoilGrids250/TAXNWRB_250m_ll.tif") %>% raster()

# check projections
#soil_wrb@crs@projargs
#UK@proj4string@projargs

# mask to UK only
#soil_wrb <- soil_wrb %>% crop(UK) %>% mask(UK)
#writeRaster(soil_wrb, find_onedrive(dir = data_repo, path = "SoilGrids250/TAXNWRB_250m_ll_UKonly.tif"))

# create dataframe from raster
Dat_main <- soil_wrb %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  rename(wrb_int = TAXNWRB_250m_ll_UKonly) %>%
  drop_na(wrb_int)

# read in legend for WRB and add shortened soil name to main data
Dat_leg <- find_onedrive(dir = data_repo, path = "SoilGrids250/TAXNWRB_250m_ll_legend.csv") %>% read_csv()

Dat_main <- Dat_main %>%
  left_join(Dat_leg %>% select(wrb_int = Number, wrb_name = Group), by = "wrb_int")

# read in postcode shapefiles from Mike S
Postcodes <- find_onedrive(dir = data_repo, path = "UK postcodes/postcode_polygons.gpkg") %>% readOGR(layer = "postcode_district")
Postcodes_wgs84 <- Postcodes %>% spTransform(CRS("+proj=longlat +datum=WGS84")) # reproject

# rasterise postcodes and assign to main data (code commented and bridged because this takes f*cking ages)
#template <- find_onedrive(dir = data_repo, path = "SoilGrids250/SoilGrids250_blank_template_UKonly.tif") %>% raster()

#postcodes_raster <- rasterize(Postcodes_wgs84, template, field = Postcodes@data[,1], fun = "mean", 
#                              update = TRUE, updateValue = "NA")

#writeRaster(postcodes_raster, find_onedrive(dir = data_repo, path = "UK postcodes/UK_postcode_raster_250m.tif"))

# this reads in the results of the commented code above
postcodes_raster <- find_onedrive(dir = data_repo, path = "UK postcodes/UK_postcode_raster_250m.tif") %>% raster()

UK_postcodes <- tibble(index = Postcodes@data[, 1],
                       postcode = Postcodes@data[, 2])

# convert postcodes to dataframe and join actual postcodes into data
temp <- postcodes_raster %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  rename(index = UK_postcode_raster_250m) %>%
  drop_na(index) %>%
  left_join(UK_postcodes, by = "index")

# join dataframes together and write out main data as .csv
Dat_main <- Dat_main %>%
  left_join(temp, by = c("x", "y"))
rm(temp)

write_csv(Dat_main, find_onedrive(dir = data_repo, path = "UK postcodes/Soil types by postcode 250m.csv"))