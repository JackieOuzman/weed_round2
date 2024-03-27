
library(tidyverse)
library(readxl)
## scrap paper
data_source <- "W:/Economic impact of weeds round 2/model/"
df <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "model cals residual weeds")


str(df)

df$`Weed free yield t/ha`<- as.double(df$`Weed free yield t/ha`)

# modify df to combine weed 1 and 2 outputs

# step1_Summary_weed1_2_yld_loss <- df %>% 
#   group_by(AEZ_code, `AEZ for report`, Region, Crop) %>% 
#   summarise(weed_free_yld_t_ha = mean(`Weed free yield t/ha`, na.rm = TRUE),
#             Crop_area_ha =       mean(`Crop area ha` , na.rm = TRUE),
#             
#             sum_area_of_weeds1_2_ha = sum(`Area of Weed Infestation (Ha)`, na.rm=TRUE),
#             sum_yld_loss_tonnes =     sum(`Yield loss tonnes`, na.rm=TRUE),
#             sum_rev_loss_dollars =     sum(`Revenue loss`, na.rm=TRUE)) %>% 
#   
#   mutate(weed_free_yld_per_farm_tonnes = weed_free_yld_t_ha * Crop_area_ha ) %>%  
#   mutate(precenatge_yld_loss_tonnes_per_farm = (sum_yld_loss_tonnes/ weed_free_yld_per_farm_tonnes)*100) 
# 
# step1_Summary_weed1_2_yld_loss <- ungroup(step1_Summary_weed1_2_yld_loss)


### Gross up step1_Summary_weed1_2_yld_loss
str(df)

df_GU <- df %>% 
  mutate(
   crop_area_ha_GU  =                  `Crop area ha` * `Gross up factor for AEZ and Crop`,
   area_of_Weed_Infestation_Ha_GU  =   `Area of Weed Infestation (Ha)` * `Gross up factor for AEZ and Crop`,
   yield_loss_tonnes_GU  =             `Yield loss tonnes` * `Gross up factor for AEZ and Crop`,
   revenue_loss_GU  =                  `Revenue loss` * `Gross up factor for AEZ and Crop`
  )

step1_Summary_weed1_2_yld_loss_GU <- df_GU %>% 
  group_by(AEZ_code, `AEZ for report`, Region, Crop) %>% 
  summarise(weed_free_yld_t_ha = mean(`Weed free yield t/ha`, na.rm = TRUE),
            Mean_crop_area_ha_GU =       mean(crop_area_ha_GU , na.rm = TRUE),
            
            sum_area_of_Weed_Infestation_Ha_GU = sum(area_of_Weed_Infestation_Ha_GU, na.rm=TRUE),
            sum_yield_loss_tonnes_GU =           sum(yield_loss_tonnes_GU, na.rm=TRUE),
            sum_revenue_loss_GU =               sum(revenue_loss_GU, na.rm=TRUE)) %>% 
  
  mutate(weed_free_yld_per_farm_tonnes_GU = weed_free_yld_t_ha * Mean_crop_area_ha_GU ) %>%  
  mutate(precenatge_yld_loss_tonnes_per_farm_GU = (sum_yield_loss_tonnes_GU/ weed_free_yld_per_farm_tonnes_GU)*100) 

step1_Summary_weed1_2_yld_loss_GU <- ungroup(step1_Summary_weed1_2_yld_loss_GU)




#########################################
# # summaries the data combining AEZ and crop 
# Summary_weed1_2_yld_loss_all <- step1_Summary_weed1_2_yld_loss %>% 
#   group_by() %>% 
#   summarise(
#     sum_yld_on_farm_per_AEZ =  sum(weed_free_yld_per_farm_tonnes, na.rm = TRUE),
#     sum_yld_loss_tonnes_AEZ = sum(sum_yld_loss_tonnes, na.rm=TRUE),
#     sum_area_weeds_per_AEZ = sum(sum_area_of_weeds1_2_ha, na.rm=TRUE),
#     sum_rev_loss_dollars_AEZ = sum(sum_rev_loss_dollars, na.rm=TRUE)) %>% 
#   mutate(precenatge_yld_loss_tonnes_per_AEZ = (sum_yld_loss_tonnes_AEZ/ sum_yld_on_farm_per_AEZ)*100) 
# 
# Summary_weed1_2_yld_loss_all <- ungroup(Summary_weed1_2_yld_loss_all)
# 
# #round percentage
# Summary_weed1_2_yld_loss_all$Summary_weed1_2_yld_loss_all <- 
#   round(Summary_weed1_2_yld_loss_all$precenatge_yld_loss_tonnes_per_AEZ, 2)
# Summary_weed1_2_yld_loss_all

############################################
names(step1_Summary_weed1_2_yld_loss_GU)

# summaries the data combining AEZ and crop GU
Summary_weed1_2_yld_loss_all_GU <- step1_Summary_weed1_2_yld_loss_GU %>% 
  group_by() %>% 
  summarise(
    sum_yld_on_farm_per_AEZ_GU =  sum(weed_free_yld_per_farm_tonnes_GU, na.rm = TRUE),
    sum_yld_loss_tonnes_AEZ_GU = sum(sum_yield_loss_tonnes_GU, na.rm=TRUE),
    sum_area_weeds_per_AEZ_GU = sum(sum_area_of_Weed_Infestation_Ha_GU, na.rm=TRUE),
    sum_rev_loss_dollars_AEZ_GU = sum(sum_revenue_loss_GU, na.rm=TRUE)) %>% 
  mutate(precenatge_yld_loss_tonnes_per_AEZ_GU = (sum_yld_loss_tonnes_AEZ_GU/ sum_yld_on_farm_per_AEZ_GU)*100) 

Summary_weed1_2_yld_loss_all_GU <- ungroup(Summary_weed1_2_yld_loss_all_GU)

#round percentage
Summary_weed1_2_yld_loss_all_GU$Summary_weed1_2_yld_loss_all_GU <- 
  round(Summary_weed1_2_yld_loss_all_GU$Summary_weed1_2_yld_loss_all_GU, 2)
Summary_weed1_2_yld_loss_all_GU


#########################################
# # summaries the data based on AEZ only 
# Summary_weed1_2_yld_loss_AEZ <- step1_Summary_weed1_2_yld_loss %>% 
#   group_by(AEZ_code, `AEZ for report`) %>% 
#   summarise(
#             sum_yld_on_farm_per_AEZ =  sum(weed_free_yld_per_farm_tonnes, na.rm = TRUE),
#             sum_yld_loss_tonnes_AEZ = sum(sum_yld_loss_tonnes, na.rm=TRUE),
#             sum_area_weeds_per_AEZ = sum(sum_area_of_weeds1_2_ha, na.rm=TRUE),
#             sum_rev_loss_dollars_AEZ = sum(sum_rev_loss_dollars, na.rm=TRUE)) %>% 
# mutate(precenatge_yld_loss_tonnes_per_AEZ = (sum_yld_loss_tonnes_AEZ/ sum_yld_on_farm_per_AEZ)*100) 
# 
# Summary_weed1_2_yld_loss_AEZ <- ungroup(Summary_weed1_2_yld_loss_AEZ)
# 
# #round percentage
# Summary_weed1_2_yld_loss_AEZ$precenatge_yld_loss_tonnes_per_AEZ <- 
#   round(Summary_weed1_2_yld_loss_AEZ$precenatge_yld_loss_tonnes_per_AEZ, 2)
# 
# Summary_weed1_2_yld_loss_AEZ

###############################################################################

# str(step1_Summary_weed1_2_yld_loss)
# ave_perc_yld_loss_all <-Summary_weed1_2_yld_loss_all$Summary_weed1_2_yld_loss_all
# 
# unique(step1_Summary_weed1_2_yld_loss$`AEZ for report`)
# 
# 
# step1_Summary_weed1_2_yld_loss %>% 
#   ggplot(aes(x= `AEZ for report`, y = precenatge_yld_loss_tonnes_per_farm))+
#   geom_point(aes(colour= Crop))+
#   geom_boxplot(fill = "white", alpha =0.1)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90,
#                                    hjust = 1,
#                                    vjust = 0.5))+
#   labs(
#        title = "Yield loss due to residual weeds in crops",
#        caption = "Values not grossed up. Red line is national average",
#        x = "",
#        y = "Yield loss as % of tonnes of yield", 
#        )+
#   geom_hline(yintercept=ave_perc_yld_loss_all, linetype="dashed", 
#              color = "red", size=.5)+
#   facet_wrap(.~ Region, scales = "free_x")



### add the gross up values , expressed as % of farms yield, and revenue loss -DONE
### add the average value from last study and herb loss baseline

### questions to ask...
#1) why does cotton have the same yield loss? what are the weeds that are driving this? and why is it so high?
#2) Why is yield loss so high in oats is this related to yield loss coefficient?
#3) why is SA mid north so high

#4) should we be showing yield loss per ha of crop type.
#eg  for AEZ 6 (SA Midnorth-Lower Yorke Eyre) Barley crop
## yield loss for AEZ rep farm in ha (weed 1 and 2) / ha of crop for rep farm

##################################################################################


str(step1_Summary_weed1_2_yld_loss_GU)
str(Summary_weed1_2_yld_loss_all_GU)
ave_perc_yld_loss_all_GU <-Summary_weed1_2_yld_loss_all_GU$precenatge_yld_loss_tonnes_per_AEZ_GU

unique(step1_Summary_weed1_2_yld_loss_GU$`AEZ for report`)


step1_Summary_weed1_2_yld_loss_GU %>% 
  ggplot(aes(x= `AEZ for report`, y = precenatge_yld_loss_tonnes_per_farm_GU))+
  geom_point(aes(colour= Crop))+
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "Yield loss due to residual weeds in crops",
    caption = "Values ARE grossed up. Red line is national average",
    x = "",
    y = "Yield loss as % of tonnes of yield", 
  )+
  geom_hline(yintercept=ave_perc_yld_loss_all_GU, linetype="dashed", 
             color = "red", size=.5)+
  facet_wrap(.~ Region, scales = "free_x")
