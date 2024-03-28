
# Cost of weed project - Yield loss module Residual weeds in crop ----
## Load and manipulate data ----

### Load library and read in data ----
library(tidyverse)
library(readxl)



data_source <- "W:/Economic impact of weeds round 2/model/"
df <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "model cals residual weeds")
kyntec_production_data <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "1. Production data for checking")

str(df)

df$`Weed free yield t/ha`<- as.double(df$`Weed free yield t/ha`)


### Gross up model outputs and  merge weed 1 and 2 ----

str(df)

df_GU <- df %>% 
  mutate(
   crop_area_ha_GU  =                  `Crop area ha` * `Gross up factor for AEZ and Crop`,
   area_of_Weed_Infestation_Ha_GU  =   `Area of Weed Infestation (Ha)` * `Gross up factor for AEZ and Crop`,
   yield_loss_tonnes_GU  =             `Yield loss tonnes` * `Gross up factor for AEZ and Crop`,
   revenue_loss_GU  =                  `Revenue loss` * `Gross up factor for AEZ and Crop`
  )

#### Just a quick check that my calculations for rep farm ha is the same as what Kyentec supplied ----
df_GU <- df_GU %>%  mutate(For_look_up  = paste0("AEZ_code_", AEZ_code , "_Crop_", Crop  ))
str(df_GU)
str(kyntec_production_data)

check <- left_join(df_GU, kyntec_production_data)
check <- check %>% 
  select("AEZ_code" , "AEZ for report"  , "Crop", "crop_area_ha_GU" ,
         "mean_19_21 HA" ) 
str(check)
check$`mean_19_21 HA` <- as.double(check$`mean_19_21 HA`)
str(check)
check <- check %>%  mutate(check_ha =  crop_area_ha_GU - `mean_19_21 HA`)

check$check_ha <- round(check$check_ha, 2)
sum(check$crop_area_ha_GU, na.rm = TRUE)

### merge weed 1 and 2 ----

step1_Summary_weed1_2_yld_loss_GU <- df_GU %>% 
  group_by(AEZ_code, `AEZ for report`, Region, Crop) %>% 
  summarise(weed_free_yld_t_ha = mean(`Weed free yield t/ha`, na.rm = TRUE),
            Mean_crop_area_ha_GU =       mean(crop_area_ha_GU , na.rm = TRUE),
            
            sum_area_of_Weed_Infestation_Ha_GU = sum(area_of_Weed_Infestation_Ha_GU, na.rm=TRUE),
            sum_yield_loss_tonnes_GU =           sum(yield_loss_tonnes_GU, na.rm=TRUE),
            sum_revenue_loss_GU =               sum(revenue_loss_GU, na.rm=TRUE)) %>% 
  
  mutate(weed_free_yld_per_farm_tonnes_GU = weed_free_yld_t_ha * Mean_crop_area_ha_GU ) %>%  
  mutate(precenatge_yld_loss_tonnes_per_farm_GU = (sum_yield_loss_tonnes_GU/ weed_free_yld_per_farm_tonnes_GU)*100) %>% 
  
  mutate( Yld_loss_per_ha_GU = (sum_yield_loss_tonnes_GU/Mean_crop_area_ha_GU),
          Rev_loss_per_ha_GU = (sum_revenue_loss_GU/Mean_crop_area_ha_GU)
          )

step1_Summary_weed1_2_yld_loss_GU <- ungroup(step1_Summary_weed1_2_yld_loss_GU)


### Summaries gross up output based on AEZ ----


names(step1_Summary_weed1_2_yld_loss_GU)

### summaries the data combining AEZ and crop GU
Summary_weed1_2_yld_loss_all_GU <- step1_Summary_weed1_2_yld_loss_GU %>% 
  group_by() %>% 
  summarise(
    sum_yld_on_farm_per_AEZ_GU =  sum(weed_free_yld_per_farm_tonnes_GU, na.rm = TRUE),
    sum_yld_loss_tonnes_AEZ_GU = sum(sum_yield_loss_tonnes_GU, na.rm=TRUE),
    sum_crop_area_ha_AEZ_GU = sum(Mean_crop_area_ha_GU, na.rm=TRUE),
    sum_area_weeds_per_AEZ_GU = sum(sum_area_of_Weed_Infestation_Ha_GU, na.rm=TRUE),
    sum_rev_loss_dollars_AEZ_GU = sum(sum_revenue_loss_GU, na.rm=TRUE)) %>% 
  mutate(precenatge_yld_loss_tonnes_per_AEZ_GU = (sum_yld_loss_tonnes_AEZ_GU/ sum_yld_on_farm_per_AEZ_GU)*100) %>% 
         
  mutate(Yld_loss_per_ha_AEZ_GU = sum_yld_loss_tonnes_AEZ_GU/sum_crop_area_ha_AEZ_GU) %>% 
  mutate(Rev_loss_per_ha_AEZ_GU = (sum_rev_loss_dollars_AEZ_GU/sum_crop_area_ha_AEZ_GU)) 

Summary_weed1_2_yld_loss_all_GU <- ungroup(Summary_weed1_2_yld_loss_all_GU)

####round percentage
Summary_weed1_2_yld_loss_all_GU$Summary_weed1_2_yld_loss_all_GU <- 
  round(Summary_weed1_2_yld_loss_all_GU$Summary_weed1_2_yld_loss_all_GU, 2)
Summary_weed1_2_yld_loss_all_GU










# Plot yield loss as % of yield for rep farm AEZ by crop type ----



str(step1_Summary_weed1_2_yld_loss_GU)
str(Summary_weed1_2_yld_loss_all_GU)
ave_perc_yld_loss_all_GU <-Summary_weed1_2_yld_loss_all_GU$precenatge_yld_loss_tonnes_per_AEZ_GU

unique(step1_Summary_weed1_2_yld_loss_GU$`AEZ for report`)

## 2024 results ----

step1_Summary_weed1_2_yld_loss_GU %>% 
  ggplot(aes(x= `AEZ for report`, y = precenatge_yld_loss_tonnes_per_farm_GU))+
  geom_point(aes(colour= Crop))+
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Yield loss due to residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss as % of tonnes of yield", 
  )+
   geom_hline(yintercept=ave_perc_yld_loss_all_GU, linetype="dashed", 
              color = "red", size=.5)+
   geom_hline(yintercept=2.6, linetype="dashed", 
              color = "green", size=.5)+
  facet_wrap(.~ Region, scales = "free_x")


## 2016 results ----
        
data_source_old_model <- "W:/Economic impact of weeds round 2/model/old model/"
df_2016 <- read_excel(paste0(data_source_old_model, "breakdown results for comparsion - yield loss module.xlsx"), 
                      sheet = "for_plotting yield loss as perc")

df_2016_long <- df_2016 %>% pivot_longer(
  cols =Wheat:TOTAL,
  names_to = "Crop",
  values_to = "yld_loss_tonnes_per_farm_GU"
) %>% 
  mutate(precenatge_yld_loss_tonnes_per_farm_GU = yld_loss_tonnes_per_farm_GU *100)



df_2016_long %>% 
  filter(Region!= "National") %>% 
  filter(Crop != "TOTAL") %>% 
  ggplot(aes(x= `AgEc Zone`, y = precenatge_yld_loss_tonnes_per_farm_GU))+
  geom_point(aes(colour= Crop))+
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2016 - Yield loss due to residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss as % of tonnes of yield", 
  )+
  geom_hline(yintercept=ave_perc_yld_loss_all_GU, linetype="dashed",
             color = "red", size=.5)+
  geom_hline(yintercept=2.6, linetype="dashed",
             color = "green", size=.5)+
  facet_wrap(.~ Region, scales = "free_x")




# Plot yield loss per HA of rep farm AEZ by crop type ----

## 2024 results ----
str(step1_Summary_weed1_2_yld_loss_GU)
str(Summary_weed1_2_yld_loss_all_GU)
ave_yld_loss_perHa_all_GU <-Summary_weed1_2_yld_loss_all_GU$Yld_loss_per_ha_AEZ_GU

unique(step1_Summary_weed1_2_yld_loss_GU$`AEZ for report`)


step1_Summary_weed1_2_yld_loss_GU
str(step1_Summary_weed1_2_yld_loss_GU)

step1_Summary_weed1_2_yld_loss_GU %>% 
 
  ggplot(aes(x= `AEZ for report`, y = Yld_loss_per_ha_GU ))+
  geom_point(aes(colour= Crop))+
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Yield loss per ha from residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national average for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss t/ha", 
  )+
   geom_hline(yintercept=ave_yld_loss_perHa_all_GU, linetype="dashed",
              color = "red", size=.5)+
    geom_hline(yintercept=0.048, linetype="dashed",
               color = "green", size=.5)+
  facet_wrap(.~ Region, scales = "free_x")


## 2016 results ----


df_2016_yld_t_ha <- read_excel(paste0(data_source_old_model, "breakdown results for comparsion - yield loss module.xlsx"), 
                      sheet = "for_plotting yld loss per ha")

str(df_2016_yld_t_ha)
df_2016_yld_t_ha_long <- df_2016_yld_t_ha %>% pivot_longer(
  cols =Wheat:TOTAL,
  names_to = "Crop",
  values_to = "Yld_loss_per_ha_GU")
# ) %>% 
#   mutate(precenatge_yld_loss_tonnes_per_farm_GU = yld_loss_tonnes_per_farm_GU *100)
str(df_2016_yld_t_ha_long)


df_2016_yld_t_ha_long %>%
  filter(Region!= "National") %>% 
  filter(Crop != "TOTAL") %>% 
  ggplot(aes(x= `AgEc Zone`, y = Yld_loss_per_ha_GU ))+
  geom_point(aes(colour= Crop))+
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2016 - Yield loss per ha from residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national average for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss t/ha", 
  )+
  geom_hline(yintercept=ave_yld_loss_perHa_all_GU, linetype="dashed",
             color = "red", size=.5)+
  geom_hline(yintercept=0.048, linetype="dashed",
             color = "green", size=.5)+
  facet_wrap(.~ Region, scales = "free_x")




# Plot revenue loss per HA of rep farm AEZ by crop type ----

## 2024 results ----
str(step1_Summary_weed1_2_yld_loss_GU)
str(Summary_weed1_2_yld_loss_all_GU)
ave_Rev_loss_perHa_all_GU <-Summary_weed1_2_yld_loss_all_GU$Rev_loss_per_ha_AEZ_GU



step1_Summary_weed1_2_yld_loss_GU %>% 
  ggplot(aes(x= `AEZ for report`, y =  Rev_loss_per_ha_GU  ))+
  geom_point(aes(colour= Crop))+
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Revenue loss per ha from residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national value for 2024 and 2016 respectively.",
    x = "",
    y = "Revenue loss $/ha", 
  )+
  geom_hline(yintercept=ave_Rev_loss_perHa_all_GU, linetype="dashed",
             color = "red", size=.5)+
  geom_hline(yintercept=12.21, linetype="dashed",
             color = "green", size=.5)+
  facet_wrap(.~ Region, scales = "free_x")


## 2016 results ----


df_2016_Rev_t_ha <- read_excel(paste0(data_source_old_model, "breakdown results for comparsion - yield loss module.xlsx"), 
                               sheet = "for_plotting rev loss per ha")

str(df_2016_Rev_t_ha)
df_2016_Rev_t_ha_long <- df_2016_Rev_t_ha %>% pivot_longer(
  cols =Wheat:TOTAL,
  names_to = "Crop",
  values_to = "Rev_loss_per_ha_GU")

str(df_2016_Rev_t_ha_long)


df_2016_Rev_t_ha_long %>%
  filter(Region!= "National") %>% 
  filter(Crop != "TOTAL") %>% 
  ggplot(aes(x= `AgEc Zone`, y = Rev_loss_per_ha_GU ))+
  geom_point(aes(colour= Crop))+
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2016 - Revenue loss per ha from residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national value for 2024 and 2016 respectively.",
    x = "",
    y = "Revenue loss $/ha", 
  )+
  geom_hline(yintercept=ave_Rev_loss_perHa_all_GU, linetype="dashed",
             color = "red", size=.5)+
  geom_hline(yintercept=12.21, linetype="dashed",
             color = "green", size=.5)+
  facet_wrap(.~ Region, scales = "free_x")




### add the gross up values , expressed as % of farms yield, and revenue loss -DONE
### add the average value from last study and herb loss baseline 2.6% -Done
### should we be showing yield loss per ha of crop type.
#eg  for AEZ 6 (SA Midnorth-Lower Yorke Eyre) Barley crop
## yield loss for AEZ rep farm in ha (weed 1 and 2) / ha of crop for rep farm

### questions to ask...
#1) why does cotton have the same yield loss? what are the weeds that are driving this? and why is it so high?
#2) Why is yield loss so high in oats is this related to yield loss coefficient?
#3) why is SA mid north so high

#4) 



