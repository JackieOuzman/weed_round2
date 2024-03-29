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

################################################################################
### join together

weed_free_yld <- left_join(production_data, NVT_data)
unique(weed_free_yld$crop)

weed_free_yld <- weed_free_yld %>%  filter(crop != "Triticale")

################################################################################
####  create a new clm that weed free yield
str(weed_free_yld)


weed_free_yld <- weed_free_yld %>% 
  mutate(weed_free_yld = case_when(
    is.na(NVT_yld) ~ production_yld,
    TRUE     ~ (production_yld+NVT_yld)/2)
  )

################################################################################
### group crops
unique(weed_free_yld$crop)

weed_free_yld <- weed_free_yld %>% 
  mutate(crop_type  =case_when(
    crop ==  "Wheat"   ~  "Cereal",
    crop ==  "Barley"   ~ "Cereal",
    crop ==  "Oats"   ~   "Cereal",
    
    crop ==  "Canola"   ~   "Broadleaf",
    crop ==  "Pulses"   ~   "Broadleaf",
    TRUE     ~ crop))
    
################################################################################
### order years 

weed_free_yld$Grouping_years <- factor(weed_free_yld$Grouping_years, 
                                              levels = c( "2016 Study", "Current study","All years"
                                              ))



################################################################################
####  How does weed free ylds change over time?
################################################################################
str(weed_free_yld)

### "Cereal"

# weed_free_yld %>%
#   filter(crop_type == "Cereal") %>%
# ggplot(aes(x = Grouping_years, y = weed_free_yld, group =  crop)
# ) +
#   geom_line(aes(linetype = crop)) +
#   facet_wrap(. ~ AEZ) +
#   theme_bw()+
#   labs(x = "", y = "Yield t/ha", fill = "")+
#   labs(title = "Weed Free Yields: Cereal")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# ggsave(
#   device = "png",
#   filename = "Weed_free_yld_Cereal.png",
#   path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
#   width=9.5,
#   height = 6.28,
#   dpi=600
# ) 
# 
# 
# ### "Broadleaf"
# 
# weed_free_yld %>%
#   filter(crop_type == "Broadleaf") %>%
#   ggplot(aes(x = Grouping_years, y = weed_free_yld, group =  crop)
#   ) +
#   geom_line(aes(linetype = crop)) +
#   facet_wrap(. ~ AEZ) +
#   theme_bw()+
#   labs(x = "", y = "Yield t/ha", fill = "")+
#   labs(title = "Weed Free Yields: Broadleaf")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# ggsave(
#   device = "png",
#   filename = "Weed_free_yld_Broadleaf.png",
#   path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
#   width=9.5,
#   height = 6.28,
#   dpi=600
# ) 
# 
# 
# ### "Grain Sorghum"
# str(weed_free_yld$crop_type)
# 
# weed_free_yld %>%
#   filter(crop_type == "Grain Sorghum") %>%
#   ggplot(aes(x = Grouping_years, y = weed_free_yld, group =  crop)
#   ) +
#   geom_line(aes(linetype = crop)) +
#   facet_wrap(. ~ AEZ) +
#   theme_bw()+
#   labs(x = "", y = "Yield t/ha", fill = "")+
#   labs(title = "Weed Free Yields: Grain Sorghum")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# ggsave(
#   device = "png",
#   filename = "Weed_free_yld_Grain_Sorghum.png",
#   path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
#   width=9.5,
#   height = 6.28,
#   dpi=600
# ) 
# 
# 
# ### "Cotton"
# 
# weed_free_yld %>%
#   filter(crop_type == "Cotton") %>%
#   ggplot(aes(x = Grouping_years, y = weed_free_yld, group =  crop)
#   ) +
#   geom_line(aes(linetype = crop)) +
#   facet_wrap(. ~ AEZ) +
#   theme_bw()+
#   labs(x = "", y = "Yield t/ha", fill = "")+
#   labs(title = "Weed Free Yields: Cotton")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# ggsave(
#   device = "png",
#   filename = "Weed_free_yld_Grain_Cotton.png",
#   path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
#   width=9.5,
#   height = 6.28,
#   dpi=600
# ) 



###############################################################################

### arrange data for export table..
unique(weed_free_yld$Grouping_years)
names(weed_free_yld)       
weed_free_yld$weed_free_yld <- round(weed_free_yld$weed_free_yld, 2) 

     
Weed_free_yld_for_model <- weed_free_yld %>% filter(Grouping_years == "Current study")
Weed_free_yld_for_model <- Weed_free_yld_for_model %>% select(AEZ,crop, weed_free_yld)
Weed_free_yld_for_model <- Weed_free_yld_for_model %>%
  pivot_wider(
    names_from = crop,
    values_from = weed_free_yld
  )


names(Weed_free_yld_for_model)
Weed_free_yld_for_model <- Weed_free_yld_for_model %>% select(AEZ, Wheat, Barley, Oats, Canola, Pulses,  `Grain Sorghum`, Cotton)



write.csv(Weed_free_yld_for_model, 
          "W:/Economic impact of weeds round 2/NVT/jaxs processing/Weed_free_yld_for_model.csv",  row.names = FALSE)


Weed_free_yld_for_model_long <- weed_free_yld %>% filter(Grouping_years == "Current study")

### add the AEZ codes and Valley codes
unique(Weed_free_yld_for_model_long$AEZ)
rm(list=ls()[! ls() %in% c("template_for_reporting","Weed_free_yld_for_model_long")])

## average yld for WA mallee and sandplain

Weed_free_yld_for_model_long_WA_malle_sandplain <- Weed_free_yld_for_model_long %>% filter(AEZ == "WA Mallee" | AEZ == "WA Sandplain")
str(Weed_free_yld_for_model_long_WA_malle_sandplain)


Weed_free_yld_for_model_long_WA_malle_sandplain <- Weed_free_yld_for_model_long %>% group_by(crop, crop_type )   %>% 
  summarise(weed_free_yld_1 = mean(weed_free_yld, na.rm = TRUE))



Weed_free_yld_for_model_long_WA_malle_sandplain <- Weed_free_yld_for_model_long_WA_malle_sandplain %>% 
  rename(weed_free_yld = weed_free_yld_1) %>% 
  mutate(AEZ = "WA Sandplain/ Mallee")
Weed_free_yld_for_model_long_WA_malle_sandplain <- ungroup(Weed_free_yld_for_model_long_WA_malle_sandplain)
str(Weed_free_yld_for_model_long_WA_malle_sandplain)
str(Weed_free_yld_for_model_long)
Weed_free_yld_for_model_long <- Weed_free_yld_for_model_long %>% select(AEZ, crop, crop_type, weed_free_yld)


Weed_free_yld_for_model_long <- rbind(Weed_free_yld_for_model_long,Weed_free_yld_for_model_long_WA_malle_sandplain)

unique(Weed_free_yld_for_model_long$AEZ)

Weed_free_yld_for_model_long <- Weed_free_yld_for_model_long %>% 
  filter(AEZ !=  "WA Sandplain") %>% 
  filter(AEZ !=  "WA Mallee")


Weed_free_yld_for_model_long <- Weed_free_yld_for_model_long %>% 
  mutate(AEZ_code = case_when(
    `AEZ` == "NSW Central"  ~ 5,
    `AEZ` == "NSW NE/Qld SE" ~ 2, 
    `AEZ` == "NSW NW/Qld SW" ~ 3,
    `AEZ` == "NSW Vic Slopes" ~ 4,
    `AEZ` == "Qld Central" ~ 1,
    `AEZ` == "SA Midnorth-Lower Yorke Eyre"~ 6,
    `AEZ` == "SA Vic Mallee"~ 7,
    `AEZ` == "SA Vic Bordertown-Wimmera" ~ 8,
    `AEZ` == "Tas Grain" ~ 9,
    `AEZ` == "Vic High Rainfall" ~ 10, 
    `AEZ` == "WA Central" ~ 11,
    `AEZ` == "WA Eastern" ~ 12,
    `AEZ` == "WA Northern"~ 13,
    `AEZ` == "WA Sandplain/ Mallee"~ 14))


unique(Weed_free_yld_for_model_long$crop)
#test_jax <- Weed_free_yld_for_model_long %>% 
Weed_free_yld_for_model_long <- Weed_free_yld_for_model_long %>% 
  mutate(crop = case_when(
    crop == "Pulses" ~ "Pulse",
    crop == "Grain Sorghum" ~ "Sorghum",
    .default = crop))


template_for_reporting <- read.csv("W:/Economic impact of weeds round 2/model/template for reporting AEZ and valleys 2024.csv")




str(template_for_reporting)
str(Weed_free_yld_for_model_long)




##clm for joining 
template_for_reporting <- template_for_reporting %>% 
  mutate(for_join = paste0(AEZ_code, Crop))
Weed_free_yld_for_model_long <- Weed_free_yld_for_model_long %>% 
  mutate(for_join = paste0(AEZ_code, crop))





reporting_format <- full_join(template_for_reporting, Weed_free_yld_for_model_long)

str(reporting_format)

reporting_format <- reporting_format %>%  filter(!is.na(`AEZ.for.report`))


write.csv(reporting_format, 
          "W:/Economic impact of weeds round 2/NVT/jaxs processing/Weed_free_yld_for_model_long.csv",  row.names = FALSE)
