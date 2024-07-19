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
  filter(soil == "PaleSand" ) %>% # I tried with other soils Clay ClayLoam Duplex Loam PaleSan
  filter(initial_water == 20 ) %>% 
  filter(weed_sow_date == "01-Jan" )
  
  
SA_sites <- SA_sites %>% 
  filter(weed_density == 1 | weed_density == 5 |weed_density == 12)

SA_sites <- SA_sites %>%  filter(Month_weed_emergedate == "Jan" )
###############################################################################
str(WA_sites)
SA_sites <- SA_sites %>% mutate(Wheat_yield_t_ha = Wheat_yield/1000)
  

  

SA_summary <- SA_sites %>% 
  group_by(weed_density, weed_kill ) %>% 
  summarise(mean_yld = mean(Wheat_yield_t_ha, na.rm = TRUE))

SA_summary <- ungroup(SA_summary)

SA_summary_controlled_weeds <- SA_summary %>% filter(weed_kill == 10) %>% rename(yld_controlled_weeds = mean_yld) 
SA_summary_controlled_weeds <- SA_summary_controlled_weeds%>% select(weed_density , yld_controlled_weeds)
SA_summary_controlled_weeds

SA_summary_step2 <- SA_summary %>% filter(weed_kill != 10) %>% rename(yld_uncontrolled_weeds = mean_yld)

SA_summary_step2
SA_summary_controlled_weeds

SA_yld_coef <- left_join(SA_summary_step2,SA_summary_controlled_weeds )
SA_yld_coef

SA_yld_coef <- SA_yld_coef %>% 
  mutate(yld_loss_tonnes = yld_controlled_weeds -yld_uncontrolled_weeds,
         yield_loss_coef = (yld_controlled_weeds -yld_uncontrolled_weeds) /yld_controlled_weeds)




SA_yld_coef_kill_40 <- SA_yld_coef %>% filter(weed_kill == 40)

write.csv(SA_yld_coef_kill_40 , "W:/Economic impact of weeds round 2/fallow_weeds_yld_loss/yld_loss_coef_SA_PaleSand_inital_water20_sow_1stJan_kill_40days.csv" )
