library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

library(lubridate)


# "X:\Summer_weeds\APSIM_7\SA_sites\output_csv\Loxton_top_upextra_Clm_step2.csv"
   
# "X:\Summer_weeds\APSIM_7\SA_sites\output_csv\Minnipa_top_upextra_Clm_step2.csv"
# "X:\Summer_weeds\APSIM_7\SA_sites\output_csv\Roseworthy_top_upextra_Clm_step2.csv"
# 
# "X:\Summer_weeds\APSIM_7\VIC_sites\output_csv\Birchip_top_upextra_Clm_step2.csv"



################################################################################

input_path <- "X:/Summer_weeds/APSIM_7/SA_sites/output_csv/"
input_path2 <- "X:/Summer_weeds/APSIM_7/VIC_sites/output_csv/"

Loxton <- read.csv(paste0(input_path, "Loxton_top_upextra_Clm_step2.csv"))
Minnipa <- read.csv(paste0(input_path, "Minnipa_top_upextra_Clm_step2.csv"))
Roseworthy <- read.csv(paste0(input_path, "Roseworthy_top_upextra_Clm_step2.csv"))

Birchip <- read.csv(paste0(input_path2, "Birchip_top_upextra_Clm_step2.csv"))

################################################################################

Loxton <- Loxton %>% mutate(site = "Loxton")
Minnipa <- Minnipa %>% mutate(site = "Minnipa")
Roseworthy <- Roseworthy %>% mutate(site = "Roseworthy")
Birchip <- Birchip %>% mutate(site = "Birchip")


SA_sites <- rbind(Loxton, Minnipa, Roseworthy,Birchip )
#rm()

###############################################################################
## filter data
str(SA_sites)
unique(SA_sites$weed_type)
unique(SA_sites$soil)
unique(SA_sites$initial_water  )
unique(SA_sites$weed_sow_date)

unique(SA_sites$weed_density)


SA_sites <- SA_sites %>% 
  filter(weed_type == "winter_dicot") %>% 
  filter(soil == "Clay" ) %>% # I tried with other soils
  filter(initial_water == 20 ) %>% 
  filter(weed_sow_date == "01-Jan" )
  
  
SA_sites <- SA_sites %>% 
  filter(weed_density == 1 | weed_density == 5 |weed_density == 12)

SA_sites <- SA_sites %>%  filter(Month_weed_emergedate == "Jan" )
###############################################################################
str(SA_sites)
SA_sites <- SA_sites %>% mutate(Wheat_yield_t_ha = Wheat_yield/1000)
  

  

SA_summary <- SA_sites %>% 
  group_by(weed_density, weed_kill ) %>% 
  summarise(mean_NO3 = mean(sownNO3_60, na.rm = TRUE))

SA_summary <- ungroup(SA_summary)

SA_summary_controlled_weeds <- SA_summary %>% filter(weed_kill == 10) %>% rename(NO3_controlled_weeds = mean_NO3) 
SA_summary_controlled_weeds <- SA_summary_controlled_weeds%>% select(weed_density , NO3_controlled_weeds)
SA_summary_controlled_weeds

SA_summary_step2 <- SA_summary %>% filter(weed_kill != 10) %>% rename(NO3_uncontrolled_weeds = mean_NO3)

SA_summary_step2
SA_summary_controlled_weeds

SA_yld_coef <- left_join(SA_summary_step2,SA_summary_controlled_weeds )
SA_yld_coef

SA_yld_coef <- SA_yld_coef %>% 
  mutate(NO3_used_kg_ha = NO3_controlled_weeds -NO3_uncontrolled_weeds)



SA_yld_coef_kill_40 <- SA_yld_coef %>% filter(weed_kill == 40)



SA_yld_coef_kill_40 <- SA_yld_coef_kill_40 %>%  mutate(NO3_replace_kg_ha = NO3_used_kg_ha/2)
SA_yld_coef_kill_40 <- SA_yld_coef_kill_40 %>%  mutate(NO3_replace_with_urea = NO3_replace_kg_ha/0.46)

SA_yld_coef_kill_40




write.csv(SA_yld_coef_kill_40 , "W:/Economic impact of weeds round 2/fallow_weeds_yld_loss/NO3_replacement_SA_clay_inital_water20_sow_1stJan_kill_40days.csv" )
