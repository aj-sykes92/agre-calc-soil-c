library(raster)
library(tidyverse)
library(lubridate)

projdata_repo <- "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing/"
bigdata_repo <- "GIS data repository"

# read in main data from Defra grass model
Dat_main <- read_rds(find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model/RB209 grass growth GIS full data.rds"))

# manipulate data from Armstrong et al. (1997) to estimate altitude effect
Dat_alt <- read_csv(find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model modifiers/armstrong-1997-grass-alt-model.csv"))

Dat_alt <- tibble(day = dmy("01-01-2019") + 0:364,
       month = month(day)) %>%
  full_join(Dat_alt %>%
  mutate(alt_mean = (alt_min + alt_max) / 2,
         month = paste0("01-", month, "-2019") %>%
           dmy() %>%
           month()),
  by = "month")
  #group_by(alt_mean) %>%
  #summarise(adj_fac = mean(adj_fac))

Dat_alt %>%
  group_by(alt_mean, adj_fac) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  #filter(alt_mean == 955) %>%
  ggplot(aes(x = alt_mean, y = adj_fac)) +
  geom_point(aes(size = n)) +
  geom_smooth(method = "lm")

Dat_alt %>%
  mutate(day_num = (day - dmy("31-12-2018")) %>% as.numeric()) %>%
  group_by(alt_mean) %>%
    arrange(desc(adj_fac)) %>%
  View()

# each altitude has an optimal growing season i.e. where adj_fac == 1
# also varying degrees of non-productivity outside of this optimal
# assuming fraction of year @ grass from agrecalc coincides with optimal growing season,
# fit a model to predict non-optimal production drop-off with altitude and weight average in agrecalc
  

(dmy("02-03-2019") - dmy("31-12-2018")) %>% as.numeric()
ymd("May")
# read in needed rasters
# altitude
# pH