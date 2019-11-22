library(rgdal)
library(sp)
library(raster)
library(tidyverse)

bigdata_repo <- "GIS data repository"
projdata_repo <- "AgRE Calc PLC/Soil C methodology/Model update"

# find_onedrive(dir = data_repo, path = "UK postcodes/Soil types by postcode 250m.csv") %>% read_csv()

# read in soil wrb raster
soil_wrb <- find_onedrive(dir = bigdata_repo, path = "SoilGrids250/TAXNWRB_250m_ll_UKonly.tif") %>% raster()

# read in a raster with ~250m grid cells with an integer index value indicating which postcode the cell lies in
postcodes_raster <- find_onedrive(dir = bigdata_repo, path = "UK postcodes/UK_postcode_raster_250m.tif") %>% raster()

# create dataframe from raster stack
Dat_main <- stack(soil_wrb, postcodes_raster) %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  rename(wrb_int = TAXNWRB_250m_ll_UKonly, index = UK_postcode_raster_250m)

# read in legend for WRB and add shortened soil name to main data
Dat_leg <- find_onedrive(dir = bigdata_repo, path = "SoilGrids250/TAXNWRB_250m_ll_legend.csv") %>% read_csv()

Dat_main <- Dat_main %>%
  left_join(Dat_leg %>% select(wrb_int = Number, wrb_name = Group), by = "wrb_int")

# digitised version of IPCC 2019 resting soil C stocks for world soils/climates
IPCC_T2.3 <- find_onedrive(dir = projdata_repo, path = "Data preprocessing/IPCC table 2.3.csv") %>% read_csv(na = "ND")

# variables dropped and grouped for current purpose
Dat_soilc <- IPCC_T2.3 %>%
  select(-(ends_with("ci"))) %>%
  gather(-ipcc_climate_zone, key = "soil_group", value = "resting_C") %>%
  mutate(soil_group = soil_group %>% str_replace_all("_est", ""))

# key to link WRB soil types to IPCC categories
Soil_key <- Dat_soilc %>%
  distinct(soil_group) %>%
  arrange(soil_group) %>%
  mutate(soil_types = list(c("Leptosol", "Vertisol", "Kastanozem", "Chernozem", "Phaeozem", "Luvisol", "Alisol", "Albeluvisol", "Solonetz", "Calcisol", "Gypsisol", "Umbrisol", "Cambisol", "Regosol"),
                           c("Acrisol", "Lixisol", "Nitisol", "Ferralsol", "Durisol"),
                           c("Podzol"),
                           c("Arenosol"),
                           c("Andosol"),
                           c("Gleysol"))) %>%
  unnest(cols = c(soil_types))

# include wrb name in soil C data
Dat_soilc <- full_join(Dat_soilc, Soil_key, by = "soil_group") %>%
  rename(wrb_name_short = soil_types)

# extract equivalent soil types from main data entries
extract_string <- Soil_key$soil_types %>% str_c(collapse = "|")

# extract matching soil short names. This misses fluvisols, histosols, stagnosols and planosols.
# join to main data
Dat_main <- Dat_main %>%
  mutate(wrb_name_short = wrb_name %>% str_extract(extract_string)) %>%
  left_join(Dat_soilc %>%
              filter(ipcc_climate_zone == "C1") %>%
              select(wrb_name_short, resting_C),
            by = "wrb_name_short")

# add in actual postcode values
library(RSQLite)
# we need the original .gpkg data, but no need to actually read in the polygons
postcode_data <- tbl(src_sqlite(find_onedrive(dir = bigdata_repo, path = "UK postcodes/postcode_polygons.gpkg")), "postcode_district") %>%
  as_tibble() %>%
  rename(index = cat, postcode = pc_district) %>%
  select(-fid, -geom)

Dat_main <- left_join(Dat_main, postcode_data, by = "index")

# we could work out cell areas if we were being super anal. I think given the size of the cells/postcode areas it's not
# really necessary.

# calculate average soil C stocks in tonnes per ha for each postcode
Dat_out <- Dat_main %>%
  group_by(index, postcode) %>%
  summarise(resting_C_mean = mean(resting_C, na.rm = T),
            resting_C_sd = sd(resting_C, na.rm = T))

# make some plots and write out files
ggplot(Dat_out, aes(x = resting_C)) +
  #geom_density(fill = "darkred", alpha = 0.5) +
  geom_histogram(fill = "darkgreen", alpha = 0.5, binwidth = 2) +
  #scale_y_log10() +
  labs(x = "Mean resting C stocks by postcode area, t / ha",
       y = "Count") +
  theme_classic()
ggsave(find_onedrive(dir = projdata_repo, path = "Data preprocessing/Soil C stock summary plot.png"),
       width = 5, height = 5)

write_csv(Dat_out, find_onedrive(dir = projdata_repo, path = "Data preprocessing/Soil C stocks by postcode index.csv"))

resting_C_raster <- rasterFromXYZ(Dat_main %>% select(x, y, resting_C))
plot(resting_C_raster)
writeRaster(resting_C_raster, find_onedrive(dir = bigdata_repo, path = "SoilGrids250/UK resting soil C stocks.tif"), overwrite = T)
