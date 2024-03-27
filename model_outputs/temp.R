## scrap paper

str(df)

df$`Weed free yield`<- as.double(df$`Weed free yield`)


step1_Summary_weed1_2_yld_loss <- df %>% 
  group_by(AEZ_code, `AEZ for report`, Crop) %>% 
  summarise(weed_free_yld_t_ha = mean(`Weed free yield`, na.rm = TRUE),
            Crop_area_ha = mean(`Crop area` , na.rm = TRUE),
            sum_area_of_weeds1_2_ha = sum(`Area of Weed Infestation (Ha)`, na.rm=TRUE),
            sum_yld_loss_tonnes = sum(`Yield loss tonnes`, na.rm=TRUE)) %>% 
  mutate(weed_free_yld_per_farm_tonnes = weed_free_yld_t_ha * Crop_area_ha ) %>% 
  select(AEZ_code, `AEZ for report`, Crop, sum_yld_loss_tonnes, weed_free_yld_per_farm_tonnes)
  

step2_Summary_weed1_2_yld_loss_AEZ <- step1_Summary_weed1_2_yld_loss %>% 
  group_by(AEZ_code, `AEZ for report`) %>% 
  summarise(sum_yld_loss_tonnes_AEZ = sum(sum_yld_loss_tonnes, na.rm=TRUE),
            sum_yld_loss_tonnes = sum(`Yield loss tonnes`, na.rm=TRUE))


Summary_weed1_2_yld_loss_byAEZ_crop <- df %>% 
  group_by(AEZ_code, `AEZ for report`, Crop) %>% 
  summarise(weed_free_yld_t_ha = mean(`Weed free yield`, na.rm = TRUE),
            Crop_area_ha = mean(`Crop area` , na.rm = TRUE),
            sum_area_of_weeds1_2_ha = sum(`Area of Weed Infestation (Ha)`, na.rm=TRUE),
            sum_yld_loss_tonnes = sum(`Yield loss tonnes`, na.rm=TRUE)) %>% 
  mutate(weed_free_yld_per_farm_tonnes = weed_free_yld_t_ha * Crop_area_ha ) %>% 
  mutate(precenatge_weed_free_yld_per_farm = (sum_yld_loss_tonnes/ weed_free_yld_per_farm_tonnes)*100)






  
