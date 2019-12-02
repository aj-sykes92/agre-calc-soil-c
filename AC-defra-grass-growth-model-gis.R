library(raster)
library(tidyverse)

projdata_repo <- "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing/Defra RB209 grass model"
bigdata_repo <- "GIS data repository"

# read in a raster with ~250m grid cells with an integer index value indicating which postcode the cell lies in
Postcodes <- find_onedrive(dir = bigdata_repo, path = "UK postcodes/UK_postcode_raster_250m.tif") %>% raster()

# read in soil wrb raster
Soil_wrb <- find_onedrive(dir = bigdata_repo, path = "SoilGrids250/TAXNWRB_250m_ll_UKonly.tif") %>% raster()

# create monthly precipitation dataset
# we're only interested in April to September i.e. months 4-9 inclusive
Precip <- raster::stack()
for(i in 4:9){
  path <- find_onedrive(dir = bigdata_repo, path = paste("WorldClim data/Precipitation (mm) 30-arc-secs/wc2.0_30s_prec_", formatC(i, width=2, flag="0"), ".tif", sep=""))
  x <- raster(path)
  Precip <- addLayer(Precip, x)
}
rm(x, i , path)

# adjust precipitation stack so extent and resolution match other rasters
Precip <- crop(Precip, Postcodes)
Precip <- resample(Precip, Postcodes)

# create master stack and convert to df
Master <- stack(Postcodes, Soil_wrb, Precip)
Dat_main <- as.data.frame(Master, xy = T)

# sum up precipitation
precip_temp <- Dat_main %>% select(5:10)
Dat_main <- Dat_main %>%
  rename(PC_index = UK_postcode_raster_250m, WRB_index = TAXNWRB_250m_ll_UKonly) %>%
  select(-(5:10)) %>%
  mutate(Precip_mm = rowSums(precip_temp))
rm(precip_temp)

# remove blank rows
Dat_main <- Dat_main %>%
  drop_na(PC_index)

# assign actual soil types
# read in legend for WRB and add shortened soil name to main data
Dat_leg <- find_onedrive(dir = bigdata_repo, path = "SoilGrids250/TAXNWRB_250m_ll_legend.csv") %>% read_csv()

Dat_main <- Dat_main %>%
  left_join(Dat_leg %>% select(WRB_index = Number, wrb_name = Group), by = "WRB_index")

# assign actual postcodes
library(RSQLite)
# we need the original .gpkg data, but no need to actually read in the polygons
postcode_data <- tbl(src_sqlite(find_onedrive(dir = bigdata_repo, path = "UK postcodes/postcode_polygons.gpkg")), "postcode_district") %>%
  as_tibble() %>%
  rename(PC_index = cat, Postcode = pc_district) %>%
  select(-fid, -geom)

Dat_main <- left_join(Dat_main, postcode_data, by = "PC_index")

# assign soil type based on Defra's RB209 definitions
Dat_main <- Dat_main %>%
  mutate(RB209_soiltype = NA,
         RB209_soiltype = ifelse())


