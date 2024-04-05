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
  filter(soil == "Clay" ) %>% # I tried with other soils
  filter(initial_water == 20 ) %>% 
  filter(weed_sow_date == "01-Jan" )
  
  
WA_sites <- WA_sites %>% 
  filter(weed_density == 1 | weed_density == 5 |weed_density == 12)

WA_sites <- WA_sites %>%  filter(Month_weed_emergedate == "Jan" )
###############################################################################
str(WA_sites)
WA_sites <- WA_sites %>% mutate(Wheat_yield_t_ha = Wheat_yield/1000)
  
#WA_sites <- WA_sites %>% mutate(for_join = paste0(site, "_weed_density_", weed_density))
  

WA_summary <- WA_sites %>% 
  group_by(weed_density, weed_kill ) %>% 
  summarise(mean_yld = mean(Wheat_yield_t_ha, na.rm = TRUE))

WA_summary <- ungroup(WA_summary)

WA_summary_controlled_weeds <- WA_summary %>% filter(weed_kill == 10) %>% rename(yld_controlled_weeds = mean_yld) 
WA_summary_controlled_weeds <- WA_summary_controlled_weeds%>% select(weed_density , yld_controlled_weeds)
WA_summary_controlled_weeds

WA_summary_step2 <- WA_summary %>% filter(weed_kill != 10) %>% rename(yld_uncontrolled_weeds = mean_yld)

WA_summary_step2
WA_summary_controlled_weeds

WA_yld_coef <- left_join(WA_summary_step2,WA_summary_controlled_weeds )
WA_yld_coef

WA_yld_coef <- WA_yld_coef %>% 
  mutate(yld_loss_tonnes = yld_controlled_weeds -yld_uncontrolled_weeds,
         yield_loss_coef = (yld_controlled_weeds -yld_uncontrolled_weeds) /yld_controlled_weeds)




WA_yld_coef_kill_40 <- WA_yld_coef %>% filter(weed_kill == 40)

write.csv(WA_yld_coef_kill_40 , "W:/Economic impact of weeds round 2/fallow_weeds_yld_loss/yld_loss_coef_WA_clay_inital_water20_sow_1stJan_kill_40days.csv" )
