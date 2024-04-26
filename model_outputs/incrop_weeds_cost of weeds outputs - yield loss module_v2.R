# Cost of weed project - Yield loss module fallow weeds in crop ----
## Load and manipulate data ----

### Load library and read in data ----
library(tidyverse)
library(readxl)

data_source <- "W:/Economic impact of weeds round 2/model/"
df <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "model cals residual weeds")
#kyntec_production_data <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "1. Production data for checking")
kyntec_production_data_tonnes <- read_csv("W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/production_data_yld_AEZ.csv")

str(df)

df$`Weed free yield t/ha`<- as.double(df$`Weed free yield t/ha`)


### Gross up model outputs and  merge weed 1 and 2 ----

names(df)

df <- df %>%  select("AEZ for report",
                     "Crop"  ,
                     "Crop area ha" ,
                     
                     "Gross up factor for AEZ and Crop",
                     
                     #outputs of model
                     "Yield loss tonnes",
                     "Revenue loss" )


df_GU <- df %>% 
  mutate(
    crop_area_ha_GU  =                  `Crop area ha` * `Gross up factor for AEZ and Crop`,
    yield_loss_tonnes_GU  =             `Yield loss tonnes` * `Gross up factor for AEZ and Crop`,
    revenue_loss_GU  =                  `Revenue loss` * `Gross up factor for AEZ and Crop`
  )



### merge weed 1 and 2 (note there are some difference compred to excel cals but its very small----

weed1_2_outputs_AEZ_Crop <- df_GU %>% 
  group_by(`AEZ for report`,  `Crop`) %>% 
  summarise(sum_yield_loss_tonnes_GU =           sum(yield_loss_tonnes_GU, na.rm=TRUE),
            sum_revenue_loss_GU =                sum(revenue_loss_GU, na.rm=TRUE),
            
            crop_area_ha_GU =                    mean(crop_area_ha_GU, na.rm=TRUE)) #this is because weed 1 and 2 have the same crop area - we just want one

names(weed1_2_outputs_AEZ_Crop)


### You could remove the crop type if you wanted the plots to be simple ----


### Add production data

str(kyntec_production_data_tonnes)

#summaries the production data (note this is already grossed up)
tonnes_AEZ_Crop <- kyntec_production_data_tonnes %>% 
  group_by(AgroEcological.Zone, Crop) %>% 
  summarise(sum_production_tonnes = sum(`mean_19_21`, na.rm=TRUE))

tonnes_AEZ_Crop <- tonnes_AEZ_Crop %>%  rename(`AEZ for report` = AgroEcological.Zone)

#add to the weed1_2_outputs_AEZ df
weed1_2_outputs_AEZ_Crop <- ungroup(weed1_2_outputs_AEZ_Crop)
tonnes_AEZ_Crop <- ungroup(tonnes_AEZ_Crop)

str(weed1_2_outputs_AEZ_Crop)
str(tonnes_AEZ_Crop)

weed1_2_outputs_AEZ_Crop <- left_join(weed1_2_outputs_AEZ_Crop, tonnes_AEZ_Crop)



### make new outputs for plotting ----

str(weed1_2_outputs_AEZ_Crop)

weed1_2_outputs_AEZ_Crop <- weed1_2_outputs_AEZ_Crop %>%
  mutate(
    all_crop_yield_loss_tonnes_per_production_GU =     (sum_yield_loss_tonnes_GU /sum_production_tonnes   ),
    all_crop_yield_loss_tonnes_per_ha_GU         =     (sum_yield_loss_tonnes_GU /crop_area_ha_GU),
    all_crop_revenue_due_yldloss_per_ha_GU           = (sum_revenue_loss_GU  / crop_area_ha_GU)
  )

output_path= "W:/Economic impact of weeds round 2/model/plotting and checking results/"

write.csv(weed1_2_outputs_AEZ_Crop, paste0(output_path, "In_crop_weed1_2_outputs_AEZ_Crop_summary.csv"))
