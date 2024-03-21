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

production_data_yld <- production_data %>% 
  filter(Unit == "T")

unique(production_data_yld$Crop)


production_data_yld <- production_data_yld %>% 
  mutate(Crop = case_when(
    Crop == "Grain Sorghum" ~ "Sorghum",
    Crop == "Pulses" ~ "Pulse",
    .default = Crop
  ))


##############################################################################

write.csv(production_data_yld, 
          "W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/production_data_yld_AEZ.csv" ,row.names = FALSE)
