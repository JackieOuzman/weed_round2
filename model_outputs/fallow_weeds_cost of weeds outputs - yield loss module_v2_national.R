# national average

### Load library and read in data ----
library(tidyverse)
library(readxl)

output_path= "W:/Economic impact of weeds round 2/model/plotting and checking results/"

data_source <- "W:/Economic impact of weeds round 2/model/"

data_source2 <- 

df <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "model cals fallow weeds")
kyntec_production_data <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "1. Production data for checking")
kyntec_production_data_tonnes <- read_csv("W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/production_data_yld_AEZ.csv")
str(df)

df$`Weed free yield t/ha`<- as.double(df$`Weed free yield t/ha`)


### Gross up model outputs and  merge weed 1 and 2 ----

names(df)

df <- df %>%  select("AEZ for report",
                     "Subsequent crop"  ,
                     "Crop area ha" ,
                     
                     "Gross up factor for AEZ and Crop",
                     
                     #outputs of model
                     "Yield loss tonnes",
                     "Revenue loss due to yield loss ($/t)",
                     "Revenue loss due to yield loss + extra fertiliser ($/t)"  )


df_GU <- df %>% 
  mutate(
    crop_area_ha_GU  =                  `Crop area ha` * `Gross up factor for AEZ and Crop`,
    yield_loss_tonnes_GU  =             `Yield loss tonnes` * `Gross up factor for AEZ and Crop`,
    revenue_due_yldloss_GU  =            `Revenue loss due to yield loss ($/t)` * `Gross up factor for AEZ and Crop`,
    revenue_due_yldloss_plus_N_GU  =     `Revenue loss due to yield loss + extra fertiliser ($/t)` * `Gross up factor for AEZ and Crop`
  )


### merge weed 1 and 2 (note there are some difference compred to excel cals but its very small----

weed1_2_outputs_AEZ_SubCrop <- df_GU %>% 
  group_by(`AEZ for report`,  `Subsequent crop`) %>% 
  summarise(sum_yield_loss_tonnes_GU =           sum(yield_loss_tonnes_GU, na.rm=TRUE),
            sum_revenue_due_yldloss_GU =         sum(revenue_due_yldloss_GU, na.rm=TRUE),
            sum_revenue_due_yldloss_plus_N_GU =  sum(revenue_due_yldloss_plus_N_GU, na.rm=TRUE),
            
            crop_area_ha_GU = mean(crop_area_ha_GU, na.rm=TRUE))

names(weed1_2_outputs_AEZ_SubCrop)


### remove the crop type and AEZ so we sum the outputs for all crops  (the 2016 model does not have this) ----

weed1_2_outputs <- weed1_2_outputs_AEZ_SubCrop %>% 
  group_by() %>% 
  summarise(all_crop_yield_loss_tonnes_GU =           sum(sum_yield_loss_tonnes_GU, na.rm=TRUE),
            all_crop_revenue_due_yldloss_GU =         sum(sum_revenue_due_yldloss_GU, na.rm=TRUE),
            all_crop_revenue_due_yldloss_plus_N_GU =  sum(sum_revenue_due_yldloss_plus_N_GU, na.rm=TRUE),
            all_crop_area_ha_GU =                     sum(crop_area_ha_GU, na.rm=TRUE))


### Add production data
str(kyntec_production_data_tonnes)


#summaries the production data (note this is alreday grossed up)
tonnes <- kyntec_production_data_tonnes %>% 
  group_by() %>% 
  summarise(sum_production_tonnes = mean(mean_19_21, na.rm=TRUE))



#add to the weed1_2_outputs_AEZ df
str(weed1_2_outputs)
str(tonnes)

weed1_2_outputs <- cbind(weed1_2_outputs, tonnes)


str(weed1_2_outputs)



### make new outputs for plotting ----

str(weed1_2_outputs)

weed1_2_outputs <- weed1_2_outputs %>%
  mutate(
    all_crop_yield_loss_tonnes_per_production_GU =     (all_crop_yield_loss_tonnes_GU /sum_production_tonnes),
    all_crop_yield_loss_tonnes_per_ha_GU         =     (all_crop_yield_loss_tonnes_GU /all_crop_area_ha_GU),
    all_crop_revenue_due_yldloss_per_ha_GU           = (all_crop_revenue_due_yldloss_GU / all_crop_area_ha_GU),
    all_crop_revenue_due_yldloss_plus_N_per_ha_GU    = (all_crop_revenue_due_yldloss_plus_N_GU /all_crop_area_ha_GU)
  )

write.csv(weed1_2_outputs, paste0(output_path, "fallow_weed1_2_outputs_national_summary.csv"))
