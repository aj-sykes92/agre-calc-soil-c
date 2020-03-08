library(raster)
library(tidyverse)
library(lubridate)

projdata_repo <- "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing/"
bigdata_repo <- "GIS data repository"

#######################################
# read in main data from Defra grass model
#######################################
Dat_main <- read_rds(find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model/RB209 grass growth GIS full data.rds"))

#######################################
# manipulate data from Armstrong et al. (1997) to estimate altitude effect
#######################################
Dat_alt <- read_csv(find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model modifiers/armstrong-1997-grass-alt-model.csv"))

Dat_alt <- tibble(day = dmy("01-01-2019") + 0:364,
       month = month(day)) %>%
  full_join(Dat_alt %>%
  mutate(alt_mean = (alt_min + alt_max) / 2,
         month = paste0("01-", month, "-2019") %>%
           dmy() %>%
           month()),
  by = "month")
  
Dat_alt_summ <- Dat_alt %>%
  group_by(alt_mean) %>%
  summarise(adj_fac = mean(adj_fac))

alt_adj_model <- lm(adj_fac ~ alt_mean, data = Dat_alt_summ)

# function to predict loss of productivity with increasing altitude
alt_adj <- function(altitude){
  x <- predict(alt_adj_model, newdata = tibble(alt_mean = altitude))
  x <- ifelse(x > 1, 1, x)
  x <- ifelse(x < 0, 0, x)
  return(x)
}

#######################################
# read in data for Woodlands Field model predict grass yield response to pH
#######################################

Dat_pH <- read_csv(find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model modifiers/woodlands-ph-rotation-model-parameters.csv"))
Dat_pH <- Dat_pH %>% filter(Crop == "Hay")

# calculate relative yields at given pH
# based on analyses by Kairsty Topp of Woodlands Field trial data
pH_adj <- function(pH){
  x <- Dat_pH$a_est + (Dat_pH$b_est / (1 + Dat_pH$d_est * pH))
  x <- ifelse(x > 1, 1, x)
  x <- ifelse(x < 0, 0, x)
  return(x)
}

#######################################
# check out yield response functions
#######################################

# altitude
tibble(x = 0:1200,
       y = alt_adj(x)) %>%
  ggplot(aes(x = x, y = y)) + geom_line()

# pH
tibble(x = seq(from = 3, to = 8, length.out = 41),
       y = pH_adj(x)) %>%
  ggplot(aes(x = x, y = y)) + geom_line()

#######################################
# read in new rasters needed for analysis + add to main dataset
#######################################

# vis aids
library(rasterVis)
Shp_UK <- find_onedrive(dir = bigdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

# altitude read + crop
Ras_alt <- find_onedrive(dir = bigdata_repo, path = "GMTED2010/W-Europe-dem-GMTED2010-30-arc-secs-wgs84-mean-elev-m.tif") %>%
  raster() %>%
  crop(Shp_UK)

# pH read + crop
Ras_pH <- find_onedrive(dir = bigdata_repo, path = "SoilGrids250/PHIHOX_M_sl4_250m_ll.tif") %>%
  raster() %>%
  crop(Shp_UK)

# join pH (correct res/proj already)
Dat_main <- Dat_main %>%
  left_join(Ras_pH %>%
              as.data.frame(xy = T) %>%
              as_tibble() %>%
              drop_na(),
            by = c("x", "y")) %>%
  rename(pH = "PHIHOX_M_sl4_250m_ll")
rm(Ras_pH)

# resample altitude, convert to df and join
Ras_alt <- Ras_alt %>% resample(Ras_pH)
Dat_main <- Dat_main %>%
  left_join(Ras_alt %>%
              as.data.frame(xy = T) %>%
              as_tibble() %>%
              drop_na(),
            by = c("x", "y")) %>%
  rename(Altitude_m = "W.Europe.dem.GMTED2010.30.arc.secs.wgs84.mean.elev.m")
rm(Ras_alt)

# write out raw data for future use as needed
Dat_main %>% write_rds(find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model modifiers/RB209 grass growth GIS full data with modifiers.rds"))

#######################################
# calculate adjustment factors based on defined models + summarise
#######################################

# apply models
Dat_main <- Dat_main %>%
  mutate(pH = pH / 10, # for some reason it's x10 in original raster
         pH_adj = pH_adj(pH),
         Alt_adj = alt_adj(Altitude_m))

# check results
set.seed(2605)
Dat_main %>% sample_n(10^4, replace = F) %>% pull(pH_adj) %>% qplot()
Dat_main %>% sample_n(10^4, replace = F) %>% pull(Alt_adj) %>% qplot()

# summarise
Dat_summ <- Dat_main %>%
  group_by(Postcode) %>%
  summarise(GGC_mean = mean(GGC),
            GGC_sd = sd(GGC),
            Alt_adj_mean = mean(Alt_adj),
            Alt_adj_sd = sd(Alt_adj),
            pH_adj_mean = mean(pH_adj),
            pH_adj_sd = sd(pH_adj))

# join up to existing soil C x postcode data and write out as one file
Dat_soilC <- read_csv(find_onedrive(dir = "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing",
                                    path = "Soil C stocks by postcode index.csv"))

Both <- full_join(Dat_soilC %>%
                    rename(Postcode = postcode,
                           Resting_C_mean = resting_C_mean,
                           Resting_C_sd = resting_C_sd),
                  Dat_summ,
                  by = "Postcode")

# write out final joined dataset
write_csv(Both, find_onedrive(dir = "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing",
                              path = "Postcode soil C and GGC joined with modifiers.csv"))

#######################################
# test plots
#######################################
Dat_main %>%
  mutate(GGC_mod = GGC * Alt_adj * pH_adj) %>%
  select(x, y, GGC_mod) %>%
  rasterFromXYZ(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  levelplot() +
  layer(sp.polygons(Shp_UK, lwd=0.1))

Dat_main %>%
  #filter(str_detect(Postcode, "^IV")) %>%
  mutate(GGC_mod = GGC * Alt_adj * pH_adj) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = GGC_mod), alpha = 0.7) +
  coord_quickmap() +
  theme_void()
ggsave(find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model modifiers/ggc-modified-map-uk.png"))  
  