library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

library(lubridate)



crop_production <-  read_excel(path =
  "W:/Economic impact of weeds round 2/production_data/raw_data/Agrological region crop production data 16 8 2023.xlsx",
  sheet ="Export - Broadacre (18)")
str(crop_production)


###############################################################################
### keep only AEZ I want
crop_production <-crop_production %>%  rename(AEZ = AgroEcological)

unique(crop_production$AEZ)
crop_production <- crop_production %>% 
  filter(AEZ != "Excluded") %>% 
  filter(AEZ != "WA Ord") %>% 
  filter(AEZ != " ") %>% 
  filter(AEZ != "Qld Atherton") %>% 
  filter(AEZ != "Qld Burdekin")


###############################################################################
### keep only crops I want
unique(crop_production$Commodity)
crop_production <-crop_production %>%  rename(crop = Commodity)

str(crop_production$crop)

crop_production <- crop_production %>%  mutate(crop = case_when(
  crop == "Chickpeas" ~  "Pulses",
  crop == "Faba Beans" ~ "Pulses",
  crop == "Field Peas" ~ "Pulses",
  crop == "Lupins" ~     "Pulses",
  crop == "Lentils" ~    "Pulses",
  TRUE ~ crop
))

unique(crop_production$crop)

crop_production <- crop_production %>% filter( 
  crop == "Wheat"  |
  crop == "Barley" | 
  crop == "Canola" |
  crop == "Oats"   |
  crop == "Pulses" |
  crop == "Cotton" |
  crop == "Grain Sorghum")

###############################################################################
### keep only production values I want
str(crop_production)
unique(crop_production$Unit)
crop_production <- crop_production %>% filter( 
  Unit == "Hectares"  |
    Unit == "Tonnes" ) 


###############################################################################
### group the years

unique(crop_production$Period)

crop_production <- crop_production %>%  mutate(Grouping_years = 
                                 case_when(
                                   # Period == "2011/12" ~ "2016 Study", #note this is the years used for NVT but is not the same as production
                                   # Period == "2012/13" ~ "2016 Study",
                                   # Period == "2013/14" ~ "2016 Study",
                                   
                                   Period == "2010/11" ~ "2016 Study",
                                   Period == "2011/12" ~ "2016 Study",
                                   Period == "2012/13" ~ "2016 Study",
                                   
                                   Period == "2018/19" ~ "Current study",
                                   Period == "2019/20" ~ "Current study",
                                   Period == "2020/21" ~ "Current study",
                                   
                                   
                                   TRUE                      ~ "other"
                                 ))
unique(crop_production$Grouping_years)


crop_production_temp <- crop_production %>% mutate(Grouping_years = "All years")

crop_production_with_duplication <- rbind(crop_production, crop_production_temp)  


unique(crop_production_with_duplication$Grouping_years)

## drop other
crop_production_with_duplication <- crop_production_with_duplication %>% filter( Grouping_years != "other")





##############################################################################
## order data for display ###
# crop_production_with_duplication$crop <- factor(crop_production_with_duplication$crop, 
#                                     levels = c("Wheat", "Barley", "Oat","Canola", 
#                                                "Pulses" ,
#                                                "Grain Sorghum" ,
#                                                "Cotton"
#                                     ))
unique(crop_production_with_duplication$Grouping_years)
crop_production_with_duplication$Grouping_years <- factor(crop_production_with_duplication$Grouping_years, 
                                              levels = c( "2016 Study", "Current study","All years"
                                              ))

unique(crop_production_with_duplication$crop)

##############################################################################
## summary of data ###
str(crop_production_with_duplication)
summary_df <- crop_production_with_duplication %>% 
  filter(Grouping_years != "other") %>% 
  group_by(Grouping_years, AEZ, crop, Unit) %>% 
  summarize(mean=mean(Amount))
summary_df <- ungroup(summary_df)


###############################################################################
#Make it wide
str(summary_df)
summary_df_wide <- summary_df %>%  pivot_wider(names_from = Unit,
                                               values_from = mean)

summary_df_wide <- summary_df_wide %>% mutate(yield = Tonnes/Hectares)

summary_df_wide$yield <- round(summary_df_wide$yield ,1)


###############################################################################
write.csv(summary_df, "W:/Economic impact of weeds round 2/production_data/Jackie_working/summary_production_data_yrs_group.csv",  row.names = FALSE)
