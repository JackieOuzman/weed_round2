library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

library(lubridate)


# "X:\Summer_weeds\APSIM_7\WA_sites\output_csv\Katanning_top_upextra_Clm_step2.csv"
# "X:\Summer_weeds\APSIM_7\WA_sites\output_csv\Meckering_top_upextra_Clm_step2.csv"
# "X:\Summer_weeds\APSIM_7\WA_sites\output_csv\Merredin_top_upextra_Clm_step2.csv"

################################################################################

input_path <- "X:/Summer_weeds/APSIM_7/WA_sites/output_csv/"

Katanning <- read.csv(paste0(input_path, "Katanning_top_upextra_Clm_step2.csv"))
Meckering <- read.csv(paste0(input_path, "Meckering_top_upextra_Clm_step2.csv"))
Merredin <- read.csv(paste0(input_path, "Merredin_top_upextra_Clm_step2.csv"))

################################################################################

Katanning <- Katanning %>% mutate(site = "Katanning")
Meckering <- Meckering %>% mutate(site = "Meckering")
Merredin <- Merredin %>% mutate(site = "Merredin")


WA_sites <- rbind(Katanning, Meckering, Merredin)
#rm(Katanning, Meckering, Merredin)

###############################################################################
## filter data
str(WA_sites)
unique(WA_sites$weed_type)
unique(WA_sites$soil)
unique(WA_sites$initial_water  )
unique(WA_sites$weed_sow_date)

unique(WA_sites$weed_density)


WA_sites <- WA_sites %>% 
  filter(weed_type == "winter_dicot") %>% 
  filter(soil == "Loam" ) %>% # I tried with other soils ClayLoam Duplex Loam
  filter(initial_water == 20 ) %>% 
  filter(weed_sow_date == "01-Jan" )
  
  
WA_sites <- WA_sites %>% 
  filter(weed_density == 1 | weed_density == 5 |weed_density == 12)

WA_sites <- WA_sites %>%  filter(Month_weed_emergedate == "Jan" )
###############################################################################
str(WA_sites)
WA_sites <- WA_sites %>% mutate(Wheat_yield_t_ha = Wheat_yield/1000)
  
WA_summary <- WA_sites %>% 
  group_by(weed_density, weed_kill ) %>% 
  summarise(mean_NO3 = mean(sownNO3_60, na.rm = TRUE))

WA_summary <- ungroup(WA_summary)

WA_summary_controlled_weeds <- WA_summary %>% filter(weed_kill == 10) %>% rename(NO3_controlled_weeds = mean_NO3) 
WA_summary_controlled_weeds <- WA_summary_controlled_weeds%>% select(weed_density , NO3_controlled_weeds)
WA_summary_controlled_weeds

WA_summary_step2 <- WA_summary %>% filter(weed_kill != 10) %>% rename(NO3_uncontrolled_weeds = mean_NO3)

WA_summary_step2
WA_summary_controlled_weeds

WA_yld_coef <- left_join(WA_summary_step2,WA_summary_controlled_weeds )
WA_yld_coef

WA_yld_coef <- WA_yld_coef %>% 
  mutate(NO3_used_kg_ha = NO3_controlled_weeds -NO3_uncontrolled_weeds)



WA_yld_coef_kill_40 <- WA_yld_coef %>% filter(weed_kill == 40)



WA_yld_coef_kill_40 <- WA_yld_coef_kill_40 %>%  mutate(NO3_replace_kg_ha = NO3_used_kg_ha/2)
WA_yld_coef_kill_40 <- WA_yld_coef_kill_40 %>%  mutate(NO3_replace_with_urea = NO3_replace_kg_ha/0.46)

WA_yld_coef_kill_40

write.csv(WA_yld_coef_kill_40 , "W:/Economic impact of weeds round 2/fallow_weeds_yld_loss/NO3_replacement__WA_Loam_inital_water20_sow_1stJan_kill_40days.csv" )
