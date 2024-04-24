
# Cost of weed project - Yield loss module fallow weeds in crop ----
## Load and manipulate data ----

### Load library and read in data ----
library(tidyverse)
library(readxl)



data_source <- "W:/Economic impact of weeds round 2/model/"
df <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "model cals fallow weeds")
kyntec_production_data <- read_excel(paste0(data_source, "yield loss AEZ.xlsx"), sheet = "1. Production data for checking")

str(df)

df$`Weed free yield t/ha`<- as.double(df$`Weed free yield t/ha`)


### Gross up model outputs and  merge weed 1 and 2 ----

names(df)

df_GU <- df %>% 
  mutate(
   crop_area_ha_GU  =                  `Crop area ha` * `Gross up factor for AEZ and Crop`,
   area_of_Weed_Infestation_Ha_GU  =   `Area of Weed Infestation (Ha)` * `Gross up factor for AEZ and Crop`,
   yield_loss_tonnes_GU  =             `Yield loss tonnes` * `Gross up factor for AEZ and Crop`,
   revenue_due_yldloss_GU  =            `Revenue loss due to yield loss ($/t)` * `Gross up factor for AEZ and Crop`,
   revenue_due_yldloss_plus_N_GU  =     `Revenue loss due to yield loss + extra fertiliser ($/t)` * `Gross up factor for AEZ and Crop`
  )

#### Just a quick check that my calculations for rep farm ha is the same as what Kyentec supplied ----
# df_GU <- df_GU %>%  mutate(For_look_up  = paste0("AEZ_code_", AEZ_code , "_Crop_", `Subsequent crop`  ))
# str(df_GU)
# str(kyntec_production_data)
# 
# check <- left_join(df_GU, kyntec_production_data)
# check <- check %>% 
#   select("AEZ_code" , "AEZ for report"  , "Crop", "crop_area_ha_GU" ,
#          "mean_19_21 HA" ) 
# str(check)
# check$`mean_19_21 HA` <- as.double(check$`mean_19_21 HA`)
# str(check)
# check <- check %>%  mutate(check_ha =  crop_area_ha_GU - `mean_19_21 HA`)
# 
# check$check_ha <- round(check$check_ha, 2)
# sum(check$crop_area_ha_GU, na.rm = TRUE) #check by opening df called check and see if the two clm match - which they do except for a few

### merge weed 1 and 2 ----

step1_Summary_weed1_2_yld_loss_GU <- df_GU %>% 
  group_by(AEZ_code, `AEZ for report`, Region, `Subsequent crop`) %>% 
  summarise(weed_free_yld_t_ha = mean(`Weed free yield t/ha`, na.rm = TRUE),
            Mean_crop_area_ha_GU =       mean(crop_area_ha_GU , na.rm = TRUE),
            
            sum_area_of_Weed_Infestation_Ha_GU = sum(area_of_Weed_Infestation_Ha_GU, na.rm=TRUE),
            sum_yield_loss_tonnes_GU =           sum(yield_loss_tonnes_GU, na.rm=TRUE),
            sum_revenue_due_yldloss_GU =         sum(revenue_due_yldloss_GU, na.rm=TRUE),
            sum_revenue_due_yldloss_plus_N_GU =  sum(revenue_due_yldloss_plus_N_GU, na.rm=TRUE)
            ) %>% 
  
  mutate(weed_free_yld_per_farm_tonnes_GU = weed_free_yld_t_ha * Mean_crop_area_ha_GU ) %>%  
  mutate(precenatge_yld_loss_tonnes_per_farm_GU = (sum_yield_loss_tonnes_GU/ weed_free_yld_per_farm_tonnes_GU)*100) %>% 
  
  mutate( Yld_loss_per_ha_GU = (sum_yield_loss_tonnes_GU/Mean_crop_area_ha_GU),
          revenue_due_yldloss_GU = (sum_revenue_due_yldloss_GU/Mean_crop_area_ha_GU),
          revenue_due_yldloss_plus_N_GU = (sum_revenue_due_yldloss_plus_N_GU/Mean_crop_area_ha_GU)
          )

step1_Summary_weed1_2_yld_loss_GU <- ungroup(step1_Summary_weed1_2_yld_loss_GU)


### Summaries gross up output based on AEZ ----


names(step1_Summary_weed1_2_yld_loss_GU)

### summaries the data combining AEZ and NO crop GU
Summary_weed1_2_yld_loss_all_GU <- step1_Summary_weed1_2_yld_loss_GU %>% 
  group_by() %>% 
  summarise(
    sum_yld_on_farm_per_AEZ_GU =  sum(weed_free_yld_per_farm_tonnes_GU, na.rm = TRUE),
    sum_yld_loss_tonnes_AEZ_GU = sum(sum_yield_loss_tonnes_GU, na.rm=TRUE),
    sum_crop_area_ha_AEZ_GU = sum(Mean_crop_area_ha_GU, na.rm=TRUE),
    sum_area_weeds_per_AEZ_GU = sum(sum_area_of_Weed_Infestation_Ha_GU, na.rm=TRUE),
    
    sum_rev_due_yldloss_AEZ_GU = sum(sum_revenue_due_yldloss_GU, na.rm=TRUE),
    sum_rev_due_yldloss_plus_N_AEZ_GU = sum(sum_revenue_due_yldloss_plus_N_GU, na.rm=TRUE)) %>% 
  
  mutate(precenatge_yld_loss_tonnes_per_AEZ_GU = (sum_yld_loss_tonnes_AEZ_GU/ sum_yld_on_farm_per_AEZ_GU)*100) %>% 
         
  mutate(Yld_loss_per_ha_AEZ_GU = sum_yld_loss_tonnes_AEZ_GU/sum_crop_area_ha_AEZ_GU) %>% 
 
  mutate(Rev_loss_due_yld_per_ha_AEZ_GU = (sum_rev_due_yldloss_AEZ_GU/sum_crop_area_ha_AEZ_GU)) %>% 
  mutate(Rev_loss_due_yld_plus_N_per_ha_AEZ_GU = (sum_rev_due_yldloss_plus_N_AEZ_GU/sum_crop_area_ha_AEZ_GU)) 

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


names(step1_Summary_weed1_2_yld_loss_GU)
No_crop_step1_Summary_yld_loss_GU <- step1_Summary_weed1_2_yld_loss_GU %>% 
  group_by(`AEZ for report`) %>% 
  summarise(
    Mean_per_yld_loss_tonnes_per_farm_GU = mean(precenatge_yld_loss_tonnes_per_farm_GU, na.rm= TRUE))







## 2016 results ----
        
data_source_old_model <- "W:/Economic impact of weeds round 2/model/old model/"
df_2016 <- read_excel(paste0(data_source_old_model, "breakdown results for comparsion - yield loss module.xlsx"), 
                      sheet = "fallow weed")
str(df_2016$`AgEc Zone`)

df_2016 <- df_2016 %>%  filter(`AgEc Zone`!="Total")

df_2016_long <- df_2016 %>% 
  rename(precenatge_yld_loss_tonnes_per_farm_GU =`yield loss as percentage of crop production`) %>% 
  mutate(precenatge_yld_loss_tonnes_per_farm_GU = precenatge_yld_loss_tonnes_per_farm_GU *100) 

str(df_2016_long)

No_crop_step1_Summary_yld_loss_GU %>% 
  ggplot(aes(x= `AEZ for report`, y = Mean_per_yld_loss_tonnes_per_farm_GU))+
  geom_point(colour = "red", size=2.5)+
  geom_point(data = df_2016_long, aes(x = `AgEc Zone`, y = `precenatge_yld_loss_tonnes_per_farm_GU`), colour = "green4" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Yield loss due to fallow weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss as % of tonnes of yield", 
  )+
  geom_hline(yintercept=ave_perc_yld_loss_all_GU, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=3.9, linetype="dashed", 
             color = "green4", size=.5)








ggsave(
  device = "png",
  filename = "2024_plus_2016_fallowYld_loss_plot_precenatge_yld_loss_tonnes_per_farm.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)


# Plot yield loss per HA of rep farm AEZ by crop type ----

## 2024 results ----
str(step1_Summary_weed1_2_yld_loss_GU)
str(Summary_weed1_2_yld_loss_all_GU)
ave_yld_loss_perHa_all_GU <-Summary_weed1_2_yld_loss_all_GU$Yld_loss_per_ha_AEZ_GU

unique(step1_Summary_weed1_2_yld_loss_GU$`AEZ for report`)



names(step1_Summary_weed1_2_yld_loss_GU)
No_crop_step1_Summary_yld_loss_GU <- step1_Summary_weed1_2_yld_loss_GU %>% 
  group_by(`AEZ for report`) %>% 
  summarise(
    Mean_Yld_loss_per_ha_GU = mean(Yld_loss_per_ha_GU, na.rm= TRUE))



step1_Summary_weed1_2_yld_loss_GU
str(step1_Summary_weed1_2_yld_loss_GU)





## 2016 results ----


str(df_2016_long)
#`Yield loss per ha`



No_crop_step1_Summary_yld_loss_GU %>% 
  ggplot(aes(x= `AEZ for report`, y = Mean_Yld_loss_per_ha_GU ))+
  geom_point(colour= "red")+
  geom_point(data = df_2016_long, aes(x = `AgEc Zone`, y = `Yield loss per ha`), colour = "green4" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Yield loss per ha from fallow weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national average for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss t/ha", 
  )+
  geom_hline(yintercept=ave_yld_loss_perHa_all_GU, linetype="dashed",
             color = "red", size=.5)+
  geom_hline(yintercept=0.073, linetype="dashed",
             color = "green4", size=.5)

ggsave(
  device = "png",
  filename = "2024_plus_2016_fallowYld_loss_per_ha.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)








# Plot revenue loss FROM YLD ONLY per HA of rep farm AEZ by crop type ----

## 2024 results ----
str(step1_Summary_weed1_2_yld_loss_GU)
str(Summary_weed1_2_yld_loss_all_GU)
ave_Rev_due_yld_loss_perHa_all_GU <-Summary_weed1_2_yld_loss_all_GU$Rev_loss_due_yld_per_ha_AEZ_GU


names(step1_Summary_weed1_2_yld_loss_GU)
No_crop_step1_Summary_yld_loss_GU <- step1_Summary_weed1_2_yld_loss_GU %>% 
  group_by(`AEZ for report`) %>% 
  summarise(
    Mean_Rev_due_yldloss_per_ha_GU = mean(revenue_due_yldloss_GU, na.rm= TRUE))










## 2016 results ----


str(df_2016_long)

str(df_2016_Rev_t_ha)
df_2016_Rev_t_ha_long <- df_2016_Rev_t_ha %>% pivot_longer(
  cols =Wheat:TOTAL,
  names_to = "Crop",
  values_to = "Rev_loss_per_ha_GU")

str(df_2016_Rev_t_ha_long)






No_crop_step1_Summary_yld_loss_GU %>% 
  ggplot(aes(x= `AEZ for report`, y =  Mean_Rev_due_yldloss_per_ha_GU  ))+
  geom_point(colour= "red")+
  geom_point(data = df_2016_long, aes(x = `AgEc Zone`, y = `Revenue loss per ha`), colour = "green4" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Revenue loss per ha from fallow weeds in crops",
    subtitle = "Note: 2024 results are yield losses due to reduced soil water",
    caption = "Values ARE grossed up. The red and green line is national value for 2024 and 2016 respectively.",
    x = "",
    y = "Revenue loss $/ha", 
  )+
  geom_hline(yintercept=ave_Rev_due_yld_loss_perHa_all_GU, linetype="dashed",
             color = "red", size=.5)+
  geom_hline(yintercept=18.90, linetype="dashed",
             color = "green", size=.5)








ggsave(
  device = "png",
  filename = "2024_plus_2016_fallow_Rev_due_to_loss_per_ha.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)

ave_perc_yld_loss_all_GU 
ave_yld_loss_perHa_all_GU



# Plot revenue loss FROM YLD and extra N per HA of rep farm AEZ by crop type ----

## 2024 results ----
str(step1_Summary_weed1_2_yld_loss_GU)
str(Summary_weed1_2_yld_loss_all_GU)
ave_Rev_due_yld_plus_N_loss_perHa_all_GU <-Summary_weed1_2_yld_loss_all_GU$Rev_loss_due_yld_plus_N_per_ha_AEZ_GU


names(step1_Summary_weed1_2_yld_loss_GU)
No_crop_step1_Summary_yld_loss_GU <- step1_Summary_weed1_2_yld_loss_GU %>% 
  group_by(`AEZ for report`) %>% 
  summarise(
    Mean_Rev_due_yldloss_Plus_N_per_ha_GU = mean(revenue_due_yldloss_plus_N_GU, na.rm= TRUE))










## 2016 results ----


str(df_2016_long)

str(df_2016_Rev_t_ha)



No_crop_step1_Summary_yld_loss_GU %>% 
  ggplot(aes(x= `AEZ for report`, y =  Mean_Rev_due_yldloss_Plus_N_per_ha_GU  ))+
  geom_point(colour= "red")+
  geom_point(data = df_2016_long, aes(x = `AgEc Zone`, y = `Revenue loss per ha`), colour = "green4" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Revenue loss per ha from fallow weeds in crops",
    subtitle = "Note: 2024 results are yield losses due to reduced soil water Plus extra nitrogen applied",
    caption = "Values ARE grossed up. The red and green line is national value for 2024 and 2016 respectively.",
    x = "",
    y = "Revenue loss $/ha", 
  )+
  geom_hline(yintercept=ave_Rev_due_yld_plus_N_loss_perHa_all_GU, linetype="dashed",
             color = "red", size=.5)+
  geom_hline(yintercept=18.90, linetype="dashed",
             color = "green", size=.5)








ggsave(
  device = "png",
  filename = "2024_plus_2016_fallow_Rev_due_to_loss_Plus_N_per_ha.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)

