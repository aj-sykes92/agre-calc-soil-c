library(raster)
library(tidyverse)

projdata_repo <- "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing/Defra RB209 grass model"
bigdata_repo <- "GIS data repository"

# read in a raster with ~250m grid cells with an integer index value indicating which postcode the cell lies in
Postcodes <- find_onedrive(dir = bigdata_repo, path = "UK postcodes/UK_postcode_raster_250m.tif") %>% raster()

# read in soils rasters
Soil_wrb <- find_onedrive(dir = bigdata_repo, path = "SoilGrids250/TAXNWRB_250m_ll_UKonly.tif") %>% raster()

Soil <- stack(find_onedrive(dir = bigdata_repo, path = "SoilGrids250/SNDPPT_M_sl4_250m_ll.tif"),
              find_onedrive(dir = bigdata_repo, path = "SoilGrids250/CLYPPT_M_sl4_250m_ll.tif"),
              find_onedrive(dir = bigdata_repo, path = "SoilGrids250/BDRICM_M_250m_ll.tif"),
              find_onedrive(dir = bigdata_repo, path = "SoilGrids250/OCSTHA_M_30cm_250m_ll.tif"),
              find_onedrive(dir = bigdata_repo, path = "SoilGrids250/BLDFIE_M_sl4_250m_ll.tif"))

# adjust soil stack so extent and resolution matches postcode raster
Soil <- crop(Soil, Postcodes)

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
Master <- stack(Postcodes, Soil, Precip)
Dat_main <- as.data.frame(Master, xy = T) %>%
  as_tibble()

# name properly, sum up precipitation and calculate clay fraction
precip_temp <- Dat_main %>% select(wc2.0_30s_prec_04:wc2.0_30s_prec_09)
Dat_main <- Dat_main %>%
  rename(PC_index = UK_postcode_raster_250m,
         Sand = SNDPPT_M_sl4_250m_ll,
         Clay = CLYPPT_M_sl4_250m_ll,
         Bedrock = BDRICM_M_250m_ll,
         OC = OCSTHA_M_30cm_250m_ll,
         BD = BLDFIE_M_sl4_250m_ll) %>%
  select(-(wc2.0_30s_prec_04:wc2.0_30s_prec_09)) %>%
  mutate(Precip_mm = rowSums(precip_temp),
         Silt = 100 - (Sand + Clay))
rm(precip_temp)

# remove blank rows
Dat_main <- Dat_main %>%
  drop_na()

# assign actual postcodes
library(RSQLite)
# we need the original .gpkg data, but no need to actually read in the polygons
postcode_data <- tbl(src_sqlite(find_onedrive(dir = bigdata_repo, path = "UK postcodes/postcode_polygons.gpkg")), "postcode_district") %>%
  as_tibble() %>%
  rename(PC_index = cat, Postcode = pc_district) %>%
  select(-fid, -geom)

Dat_main <- left_join(Dat_main, postcode_data, by = "PC_index")

# assign soil type based on texture data (details here https://cran.r-project.org/web/packages/soiltexture/vignettes/soiltexture_vignette.pdf)
library(soiltexture)

Dat_soil <- Dat_main %>%
  dplyr::select(SAND = Sand, SILT = Silt, CLAY = Clay, OC) %>%
  mutate(OC = OC / 10) %>%
  as.data.frame()

Dat_main <- Dat_main %>%
  mutate(Soil_type = TT.points.in.classes(tri.data = Dat_soil,
                                          class.sys = "UK.SSEW.TT",
                                          PiC.type = "t"))

# this done to provide basis for manual soil type translations based on descriptions in RB209 Table 3.13
# now commented to allow read in of modified file

#Soil_cats <- tibble(cat = Dat_main %>% pull(Soil_type) %>% unique()) %>%
#  mutate(len = str_length(cat)) %>%
#  arrange(len) %>%
#  select(-len)
#write_csv(Soil_cats, find_onedrive(dir = projdata_repo, path = "Defra soil type translations.csv"))

# read in soil category translations (manual) and join to main data
Soil_cats <- read_csv(find_onedrive(dir = projdata_repo, path = "Defra soil type translations.csv"))
Dat_main <- Dat_main %>% left_join(Soil_cats %>% rename(Soil_type = cat, RB209_soiltype = trans), by = "Soil_type")

# function to calculate OM fraction based on C (t/ha) and BD (kg / m2)
OM_frac <- function(BD_kg_m2, C_t_ha){
  C_kg_m2 <- C_t_ha * 10^3 * 10^-4 * 1 / 0.3
  Cfrac <- C_kg_m2 / BD_kg_m2
  OMfrac <- Cfrac / 0.58
  return(OMfrac)
}

# calculate other variables not based on SSEW soil types and adjust RB209 definitions accordingly
Dat_main <- Dat_main %>%
  mutate(RB209_soiltype = ifelse(Bedrock <= 40, "Shallow soils", RB209_soiltype), # shallow soils <= 40cm
         OMfrac = OM_frac(BD, OC),
         RB209_soiltype = ifelse(OMfrac >= 0.1, "Organic soils", RB209_soiltype), # organic soils
         RB209_soiltype = ifelse(OMfrac >= 0.2, "Peat soils", RB209_soiltype)) # peat soils

# water availability class from Defra RB209
Water_availability <- Dat_main %>%
  distinct(RB209_soiltype) %>%
  arrange(RB209_soiltype) %>%
  mutate(SAW = c(2, 3, 1, 2, 3)) # soil available water class — 1 = low, 2 = medium, 3 = high

Dat_main <- Dat_main %>% left_join(Water_availability, by = "RB209_soiltype")         

# rainfall classes from Defra RB209 and final pasture growth class
# addition method is something I've inferred — works if you parameterise SAW from 1-3 and rain class from 0-2 or vice versa
# rain classes are <300, 300-400, >400
breaks <- c(min(Dat_main$Precip_mm) - 1, 300, 400, max(Dat_main$Precip_mm) + 1)

Dat_main <- Dat_main %>%
  mutate(RC = Precip_mm %>%
           cut(breaks = breaks, labels = F) %>%
           as.numeric(),
         GGC = SAW + RC - 1) # 1 = V poor, 2 = Poor, 3 = Av, 4 = Good, 5 = V good

# convert main data to raster and write out for future use as needed
GGC <- Dat_main %>% select(x, y, GGC) %>% rasterFromXYZ()
writeRaster(GGC, find_onedrive(dir = bigdata_repo, path = "Created rasters/Defra_RB209_grass_growth_class_UK_250m.tif"))
plot(GGC)

# write out main data as .rds (most efficient for reuse)
write_rds(Dat_main, find_onedrive(dir = projdata_repo, path = "RB209 grass growth GIS full data.rds"))

# checks
Dat_main %>% sample_n(30, replace = F) %>% View()
set.seed(2605)
qplot(Dat_main %>% sample_n(10^5, replace = F) %>% pull(GGC))

Dat_main %>% drop_na() %>% nrow() # no NA values there

# summarise into postcodes
Dat_summ <- Dat_main %>%
  group_by(Postcode) %>%
  summarise(GGC = mean(GGC))

Dat_summ$GGC %>% qplot() # solid trimodal distribution right there

write_csv(Dat_summ, find_onedrive(dir = projdata_repo, path = "RB209 grass growth class by postcode.csv"))

# join up to existing soil C x postcode data and write out as one file
SoilC <- read_csv(find_onedrive(dir = "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing",
                                path = "Soil C stocks by postcode index.csv"))

Both <- full_join(SoilC %>% rename(Postcode = postcode), Dat_summ, by = "Postcode")

write_csv(Both, find_onedrive(dir = "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing",
                              path = "Postcode soil C and GGC joined.csv"))
