library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)


#Production data
production_data <- read.csv("W:/Economic impact of weeds round 2/production_data/Jackie_working/summary_production_data_yrs_group.csv",)
#NVT
NVT_data <- read.csv("W:/Economic impact of weeds round 2/NVT/jaxs processing/summary_NVT_yrs_group.csv")


str(production_data)
unique(production_data$Unit)
production_data <-production_data %>%  select(Grouping_years:crop, yield)
production_data <-production_data %>% rename( production_yld = yield)
unique(production_data$crop)




str(NVT_data)

NVT_data <-NVT_data %>%  select(Grouping_years:crop, m)
NVT_data <-NVT_data %>% rename( NVT_yld = m)


### join together

weed_free_yld <- left_join(production_data, NVT_data)
unique(weed_free_yld$crop)

weed_free_yld <- weed_free_yld %>%  filter(crop != "Triticale")

### few things should have cotton....
