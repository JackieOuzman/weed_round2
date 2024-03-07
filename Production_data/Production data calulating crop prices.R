library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

library(lubridate)

production_data <- read.csv("//FSSA2-ADL/clw-share1/Microlab/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/production_data_AEZ.csv")


#tidy up data frame
str(production_data)
production_data <- production_data %>% 
  select(Unit:AEZ_code, mean_19_21)


unique(production_data$Unit)
## we want $ per tonne

production_data_t_ha <- production_data %>% 
  filter(Unit == "Dollars"|
           Unit == "T")

unique(production_data_t_ha$Crop)


production_data_t_ha <- production_data_t_ha %>% 
  mutate(Crop = case_when(
    Crop == "Grain Sorghum" ~ "Sorghum",
    Crop == "Pulses" ~ "Pulse",
    .default = Crop
  ))

unique(production_data_t_ha$Crop)

production_data_t_ha_wide <- production_data_t_ha %>% pivot_wider(
  names_from = Unit,
  values_from = mean_19_21
)

str(production_data_t_ha_wide)

production_data_t_ha_wide_cal <- production_data_t_ha_wide %>% 
  mutate(crop_value = Dollars/ T)
##############################################################################



rm(list=ls()[! ls() %in% c("production_data_t_ha_wide_cal")])


##############################################################################

write.csv(production_data_t_ha_wide_cal, 
"W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/Crop_prices_AEZ.csv" ,row.names = FALSE)
