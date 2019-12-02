library(tidyverse)
library(digitize)

projdata_repo <- "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing/Defra RB209 grass model"


# code until next comment requires manual input to viewer to calibrate + digitise data from plot
cal <- ReadAndCal(find_onedrive(dir = projdata_repo, path = "Grass model screengrab.png"))

poor_lb <- DigitData()
av_lb <- DigitData()
good_lb <- DigitData()
good_ub <- DigitData()

x1 = 0
x2 = 350
y1 = 0
y2 = 16

Poor_lb <- Calibrate(poor_lb, cal, x1, x2, y1, y2) %>% as_tibble()
Av_lb <- Calibrate(av_lb, cal, x1, x2, y1, y2) %>% as_tibble()
Good_lb <- Calibrate(good_lb, cal, x1, x2, y1, y2) %>% as_tibble()
Good_ub <- Calibrate(good_ub, cal, x1, x2, y1, y2) %>% as_tibble()

# calibration done, binding and writing raw data. Test plot to check for errors
Dat_raw <- bind_rows(list(Poor_lb = Poor_lb, Av_lb = Av_lb, Good_lb = Good_lb, Good_ub = Good_ub), .id = "Model") %>%
  rename(N_rate = x, DM_ha = y)
write_csv(Dat_raw, find_onedrive(dir = projdata_repo, path = "Grass model digitisation raw.csv"))
ggplot(Dat_raw, aes(x = N_rate, y = DM_ha, colour = Model)) + geom_line()

# rounding data to sensible values and ensuring we have 0-350 ranges
Dat_main <- Dat_raw %>%
  mutate(N_rate = round(N_rate, digits = 0),
         DM_ha = round(DM_ha, digits = 2)) %>%
  group_by(Model) %>%
  mutate(N_rate = ifelse(N_rate == min(N_rate), 0, N_rate),
         N_rate = ifelse(N_rate == max(N_rate), 350, N_rate)) %>%
  ungroup()

# plot to check loess models and dial in span
ggplot(Dat_main, aes(x = N_rate, y = DM_ha, colour = Model)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.8, size = 0.5)

# we're going to do a loess model for the following classifications
# very poor, poor, average, good, very good

model <- function(x){
  y <- loess(DM_ha ~ N_rate, data = Dat_main %>% filter(Model == x), span = 0.8)
  z <- predict(y, newdata = 1:350)
  return(z)
}


Dat_preds <- tibble(N_rate = 1:350,
                    Poor_lb = model("Poor_lb"),
                    Av_lb = model("Av_lb"),
                    Good_lb = model("Good_lb"),
                    Good_ub = model("Good_ub"))

Dat_preds <- Dat_preds %>%
  mutate(Vpoor = Poor_lb + (Av_lb - Poor_lb) / 3,
         Poor = Poor_lb + (Av_lb - Poor_lb) / 3 * 2,
         Av = Av_lb + (Good_lb - Av_lb) / 2,
         Good = Good_lb + (Good_ub - Good_lb) / 3,
         Vgood = Good_lb + (Good_ub - Good_lb) / 3 * 2)

# check how models plot (inc orignal lower/upper bounds)
Dat_preds %>%
  gather(-N_rate, key = "Qual", value = "DM_ha") %>%
  ggplot(aes(x = N_rate, y = DM_ha, colour = Qual)) +
  geom_line()

# write out final models for lookup
write_csv(Dat_preds %>% select(-(Poor_lb:Good_ub)), find_onedrive(dir = projdata_repo, path = "Defra RB209 final models.csv"))
