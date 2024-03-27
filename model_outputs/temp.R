## scrap paper
data_source <- "W:/Economic impact of weeds round 2/model/"
df <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "model cals residual weeds")


str(df)

df$`Weed free yield t/ha`<- as.double(df$`Weed free yield t/ha`)

# modify df to combine weed 1 and 2 outputs

step1_Summary_weed1_2_yld_loss <- df %>% 
  group_by(AEZ_code, `AEZ for report`, Region, Crop) %>% 
  summarise(weed_free_yld_t_ha = mean(`Weed free yield t/ha`, na.rm = TRUE),
            Crop_area_ha =       mean(`Crop area ha` , na.rm = TRUE),
            
            sum_area_of_weeds1_2_ha = sum(`Area of Weed Infestation (Ha)`, na.rm=TRUE),
            sum_yld_loss_tonnes =     sum(`Yield loss tonnes`, na.rm=TRUE),
            sum_rev_loss_dollars =     sum(`Revenue loss`, na.rm=TRUE)) %>% 
  
  mutate(weed_free_yld_per_farm_tonnes = weed_free_yld_t_ha * Crop_area_ha ) %>%  
  mutate(precenatge_yld_loss_tonnes_per_farm = (sum_yld_loss_tonnes/ weed_free_yld_per_farm_tonnes)*100) 

step1_Summary_weed1_2_yld_loss <- ungroup(step1_Summary_weed1_2_yld_loss)

#########################################
# summaries the data combining AEZ and crop 
Summary_weed1_2_yld_loss_all <- step1_Summary_weed1_2_yld_loss %>% 
  group_by() %>% 
  summarise(
    sum_yld_on_farm_per_AEZ =  sum(weed_free_yld_per_farm_tonnes, na.rm = TRUE),
    sum_yld_loss_tonnes_AEZ = sum(sum_yld_loss_tonnes, na.rm=TRUE),
    sum_area_weeds_per_AEZ = sum(sum_area_of_weeds1_2_ha, na.rm=TRUE),
    sum_rev_loss_dollars_AEZ = sum(sum_rev_loss_dollars, na.rm=TRUE)) %>% 
  mutate(precenatge_yld_loss_tonnes_per_AEZ = (sum_yld_loss_tonnes_AEZ/ sum_yld_on_farm_per_AEZ)*100) 

Summary_weed1_2_yld_loss_all <- ungroup(Summary_weed1_2_yld_loss_all)

#round percentage
Summary_weed1_2_yld_loss_all$Summary_weed1_2_yld_loss_all <- 
  round(Summary_weed1_2_yld_loss_all$precenatge_yld_loss_tonnes_per_AEZ, 2)
Summary_weed1_2_yld_loss_all

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

str(step1_Summary_weed1_2_yld_loss)
ave_perc_yld_loss_all <-Summary_weed1_2_yld_loss_all$Summary_weed1_2_yld_loss_all

unique(step1_Summary_weed1_2_yld_loss$`AEZ for report`)


step1_Summary_weed1_2_yld_loss %>% 
  ggplot(aes(x= `AEZ for report`, y = precenatge_yld_loss_tonnes_per_farm))+
  geom_point(aes(colour= Crop))+
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
       title = "Yield loss due to residual weeds in crops",
       caption = "Values not grossed up. Red line is national average",
       x = "",
       y = "Yield loss as % of tonnes of yield", 
       )+
  geom_hline(yintercept=ave_perc_yld_loss_all, linetype="dashed", 
             color = "red", size=.5)+
  facet_wrap(.~ Region, scales = "free_x")



### add the gross up values , expressed as % of farms yield, and revenue loss
### add the average value from last study and herb loss baseline

### questions to ask...
#1) why does cotton have the same yield loss? what are the weeds that are driving this? and why is it so high?
#2) Why is yield loss so high in oats is this related to yield loss coefficient?
#3) why is SA mid north so high

#4) should we be showing yield loss per ha of crop type.
#eg  for AEZ 6 (SA Midnorth-Lower Yorke Eyre) Barley crop
## yield loss for AEZ rep farm in ha (weed 1 and 2) / ha of crop for rep farm




  
