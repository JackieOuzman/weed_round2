library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

library(lubridate)


# "X:\Summer_weeds\APSIM_7\NSW_sites\output_csv\Condobolin_top_upextra_Clm_step2.csv"
# "X:\Summer_weeds\APSIM_7\NSW_sites\output_csv\Cootamundra_top_upextra_Clm_step2.csv"
# "X:\Summer_weeds\APSIM_7\NSW_sites\output_csv\Merriwagga_top_upextra_Clm_step2.csv"





################################################################################

input_path <- "X:/Summer_weeds/APSIM_7/NSW_sites/output_csv/"


Condobolin <- read.csv(paste0(input_path, "Condobolin_top_upextra_Clm_step2.csv"))
Cootamundra <- read.csv(paste0(input_path, "Cootamundra_top_upextra_Clm_step2.csv"))
Merriwagga <- read.csv(paste0(input_path, "Merriwagga_top_upextra_Clm_step2.csv"))



################################################################################

Condobolin <- Condobolin %>% mutate(site = "Condobolin")
Cootamundra <- Cootamundra %>% mutate(site = "Cootamundra")
Merriwagga <- Merriwagga %>% mutate(site = "Merriwagga")



NSW_sites <- rbind(Condobolin, Cootamundra, Merriwagga )
#rm()

###############################################################################
## filter data
str(NSW_sites)
unique(NSW_sites$weed_type)
unique(NSW_sites$soil)
unique(NSW_sites$initial_water  )
unique(NSW_sites$weed_sow_date)

unique(NSW_sites$weed_density)


NSW_sites <- NSW_sites %>% 
  filter(weed_type == "winter_dicot") %>% 
  filter(soil == "Clay" ) %>% # I tried with other soils
  filter(initial_water == 20 ) %>% 
  filter(weed_sow_date == "01-Jan" )
  
  
NSW_sites <- NSW_sites %>% 
  filter(weed_density == 1 | weed_density == 5 |weed_density == 12)

NSW_sites <- NSW_sites %>%  filter(Month_weed_emergedate == "Jan" )
###############################################################################
str(NSW_sites)
NSW_sites <- NSW_sites %>% mutate(Wheat_yield_t_ha = Wheat_yield/1000)
  

  

NSW_summary <- NSW_sites %>% 
  group_by(weed_density, weed_kill ) %>% 
  summarise(mean_yld = mean(Wheat_yield_t_ha, na.rm = TRUE))

NSW_summary <- ungroup(NSW_summary)

NSW_summary_controlled_weeds <- NSW_summary %>% filter(weed_kill == 10) %>% rename(yld_controlled_weeds = mean_yld) 
NSW_summary_controlled_weeds <- NSW_summary_controlled_weeds%>% select(weed_density , yld_controlled_weeds)
NSW_summary_controlled_weeds

NSW_summary_step2 <- NSW_summary %>% filter(weed_kill != 10) %>% rename(yld_uncontrolled_weeds = mean_yld)

NSW_summary_step2
NSW_summary_controlled_weeds

NSW_yld_coef <- left_join(NSW_summary_step2,NSW_summary_controlled_weeds )
NSW_yld_coef

NSW_yld_coef <- NSW_yld_coef %>% 
  mutate(yld_loss_tonnes = yld_controlled_weeds -yld_uncontrolled_weeds,
         yield_loss_coef = (yld_controlled_weeds -yld_uncontrolled_weeds) /yld_controlled_weeds)




NSW_yld_coef_kill_40 <- NSW_yld_coef %>% filter(weed_kill == 40)

write.csv(NSW_yld_coef_kill_40 , "W:/Economic impact of weeds round 2/fallow_weeds_yld_loss/yld_loss_coef_NSW_clay_inital_water20_sow_1stJan_kill_40days.csv" )
