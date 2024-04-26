library(tidyverse)
library(readxl)

## 2024 summary results ----
path_2024= "W:/Economic impact of weeds round 2/model/plotting and checking results/"
incrop_2024 <- read.csv(paste0(path_2024,  "In_crop_weed1_2_outputs_AEZ_Crop_summary.csv"))



## 2016 results ----

data_source_old_model <- "W:/Economic impact of weeds round 2/model/old model/"
#2016
df_2016 <- read_excel(paste0(data_source_old_model, "breakdown results for comparsion - yield loss module.xlsx"), 
                      sheet = "Incrop_weed_all")
str(df_2016$`AgEc Zone`)
df_2016_national <- df_2016 %>%  filter(`AgEc Zone`=="Total")
df_2016 <- df_2016 %>%  filter(`AgEc Zone`!="Total")

df_2016 <- df_2016 %>% 
  rename(precenatge_yld_loss_tonnes_per_production_GU =`yield loss as percentage of crop production`) %>% 
  mutate(precenatge_yld_loss_tonnes_per_production_GU = precenatge_yld_loss_tonnes_per_production_GU *100) 

str(df_2016)


#### national averages
#2024

incrop_2024_national <- read.csv("W:/Economic impact of weeds round 2/model/plotting and checking results/incrop_weed1_2_outputs_national_summary.csv")
str(incrop_2024_national)


national_yield_loss_tonnes_per_production <-incrop_2024_national$all_crop_yield_loss_tonnes_per_production_GU
national_yield_loss_tonnes_per_ha         <-incrop_2024_national$all_crop_yield_loss_tonnes_per_ha_GU        
national_revenue_loss                     <-incrop_2024_national$all_crop_revenue_due_yldloss_per_ha_GU



#2016
str(df_2016_national)
unique(df_2016_national$variable)




national2016_yield_loss_tonnes_per_production <- df_2016_national %>% filter(variable ==  "yield loss as percentage of crop production") %>% select(TOTAL)*100
national2016_yield_loss_tonnes_per_ha <- df_2016_national %>% filter(variable ==  "yield loss per ha") %>% select(TOTAL)
national2016_revenue_loss_per_ha  <- df_2016_national %>% filter(variable ==  "rev loss per ha") %>% select(TOTAL)



incrop_2024$Crop <- factor(incrop_2024$Crop, 
                                                 levels = c("Wheat", "Barley", "Oats", "Canola", "Pulse", "Sorghum",
                                                            "Cotton" ))

df_2016$Crop <- factor(df_2016$Crop, 
                           levels = c("Wheat", "Barley", "Oats", "Canola", "Pulse", "Sorghum" ))

################################################################################
### Plots
#yield loss as % of production AEZ by crop type ----
## 2024 results ----

str(incrop_2024) $all_crop_yield_loss_tonnes_per_production_GU
national_yield_loss_tonnes_per_production
national2016_yield_loss_tonnes_per_production




incrop_2024 %>% 
  ggplot(aes(x= AEZ.for.report, y = all_crop_yield_loss_tonnes_per_production_GU*100))+
  geom_point(aes(colour= Crop))+
  scale_color_manual(values = c("Wheat" = "red",
                                "Barley"="orange",
                                "Oats"="darkviolet",
                                "Canola"="blue3",
                                "Pulse"="lightblue",
                                "Sorghum"="grey",
                                "Cotton"="black"
                                )) +
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Yield loss due to residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss as % of production (tonnes of yield)", 
  )+
   geom_hline(yintercept=national_yield_loss_tonnes_per_production, linetype="dashed", 
              color = "red", size=.5)+
   geom_hline(yintercept=national2016_yield_loss_tonnes_per_production[1,1], linetype="dashed", 
              color = "green", size=.5)


ggsave(
  device = "png",
  filename = "2024yld_loss_plot_precenatge_yld_loss_prec_production.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)

## 2016 results ----
str(df_2016)

df_2016_long <- df_2016 %>% pivot_longer(
  cols =Wheat:TOTAL,
  names_to = "Crop" ,
  values_to = "value"
)

unique(df_2016_long$variable)

df_2016_long %>% 
  filter(Region!= "National") %>% 
  filter(Crop != "TOTAL") %>% 
  filter(variable == "yield loss as percentage of crop production") %>% 
  ggplot(aes(x= `AgEc Zone`, y = value*100))+
  geom_point(aes(colour= Crop))+
  scale_color_manual(values = c("Wheat" = "red",
                                "Barley"="orange",
                                "Oats"="darkviolet",
                                "Canola"="blue3",
                                "Pulse"="lightblue",
                                "Sorghum"="grey"                                
  )) +
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
  geom_hline(yintercept=national_yield_loss_tonnes_per_production, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=national2016_yield_loss_tonnes_per_production[1,1], linetype="dashed", 
             color = "green", size=.5)

ggsave(
  device = "png",
  filename = "2016_yld_loss_plot_precenatge_yld_loss_tonnes_per_farm.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)



# Plot yield loss per HA of rep farm AEZ by crop type ----
## 2024 results ----

str(incrop_2024) #all_crop_yield_loss_tonnes_per_ha_GU
national_yield_loss_tonnes_per_ha
national2016_yield_loss_tonnes_per_ha

incrop_2024$all_crop_yield_loss_tonnes_per_ha_GU <- as.double(incrop_2024$all_crop_yield_loss_tonnes_per_ha_GU )


incrop_2024 %>% 
  ggplot(aes(x= AEZ.for.report, y = all_crop_yield_loss_tonnes_per_ha_GU))+
  geom_point(aes(colour= Crop))+
  scale_color_manual(values = c("Wheat" = "red",
                                "Barley"="orange",
                                "Oats"="darkviolet",
                                "Canola"="blue3",
                                "Pulse"="lightblue",
                                "Sorghum"="grey",
                                "Cotton"="black"
  )) +
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Yield loss due to residual weeds in crops (t/ha)",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss t/ha", 
  )+
  geom_hline(yintercept=national_yield_loss_tonnes_per_ha, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=as.double(national2016_yield_loss_tonnes_per_ha[1,1]), linetype="dashed", 
             color = "green", size=.5)


ggsave(
  device = "png",
  filename = "2024yld_loss_plot_t_ha.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)

str(df_2016_long)
unique(df_2016_long$variable)

df_2016_long %>% 
  filter(Region!= "National") %>% 
  filter(Crop != "TOTAL") %>% 
  filter(variable == "yield loss per ha") %>% 
  ggplot(aes(x= `AgEc Zone`, y = value))+
  geom_point(aes(colour= Crop))+
  scale_color_manual(values = c("Wheat" = "red",
                                "Barley"="orange",
                                "Oats"="darkviolet",
                                "Canola"="blue3",
                                "Pulse"="lightblue",
                                "Sorghum"="grey"                                
  )) +
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2016 - Yield loss due to residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss t/ha", 
  )+
  geom_hline(yintercept=national_yield_loss_tonnes_per_ha, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=as.double(national2016_yield_loss_tonnes_per_ha[1,1]), linetype="dashed", 
             color = "green", size=.5)


ggsave(
  device = "png",
  filename = "2016_yld_loss_plot_t_ha.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)



# Plot Rev loss per HA of rep farm AEZ by crop type ----
## 2024 results ----

str(incrop_2024) #all_crop_revenue_due_yldloss_per_ha_GU
national_revenue_loss
national2016_revenue_loss_per_ha

incrop_2024$all_crop_revenue_due_yldloss_per_ha_GU <- as.double(incrop_2024$all_crop_revenue_due_yldloss_per_ha_GU )


incrop_2024 %>% 
  ggplot(aes(x= AEZ.for.report, y = all_crop_revenue_due_yldloss_per_ha_GU))+
  geom_point(aes(colour= Crop))+
  scale_color_manual(values = c("Wheat" = "red",
                                "Barley"="orange",
                                "Oats"="darkviolet",
                                "Canola"="blue3",
                                "Pulse"="lightblue",
                                "Sorghum"="grey",
                                "Cotton"="black"
  )) +
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 - Revenue loss due to residual weeds in crops ($/ha)",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Revenue loss $/ha", 
  )+
  geom_hline(yintercept=national_revenue_loss, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=as.double(national2016_revenue_loss_per_ha[1,1]), linetype="dashed", 
             color = "green", size=.5)


ggsave(
  device = "png",
  filename = "2024Rev_loss_plot_doll_ha.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)


str(df_2016_long)
unique(df_2016_long$variable)

df_2016_long %>% 
  filter(Region!= "National") %>% 
  filter(Crop != "TOTAL") %>% 
  filter(variable == "rev loss per ha") %>% 
  ggplot(aes(x= `AgEc Zone`, y = value))+
  geom_point(aes(colour= Crop))+
  scale_color_manual(values = c("Wheat" = "red",
                                "Barley"="orange",
                                "Oats"="darkviolet",
                                "Canola"="blue3",
                                "Pulse"="lightblue",
                                "Sorghum"="grey"                                
  )) +
  geom_boxplot(fill = "white", alpha =0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2016 - Reveue loss due to residual weeds in crops",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Reveue loss $/ha", 
  )+
  geom_hline(yintercept=national_revenue_loss, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=as.double(national2016_revenue_loss_per_ha[1,1]), linetype="dashed", 
             color = "green", size=.5)


ggsave(
  device = "png",
  filename = "2016_Rev_loss_plot_doll_ha.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)

