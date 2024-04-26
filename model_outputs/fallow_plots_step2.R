#Fallow plots

### Load library and read in data ----
library(tidyverse)
library(readxl)

## 2024 summary results ----
path_2024= "W:/Economic impact of weeds round 2/model/plotting and checking results/"
fallow_2024 <- read.csv(paste0(path_2024,  "fallow_weed1_2_outputs_AEZ_summary.csv"))



## 2016 results ----

data_source_old_model <- "W:/Economic impact of weeds round 2/model/old model/"
df_2016 <- read_excel(paste0(data_source_old_model, "breakdown results for comparsion - yield loss module.xlsx"), 
                      sheet = "fallow weed")
str(df_2016$`AgEc Zone`)
df_2016_national <- df_2016 %>%  filter(`AgEc Zone`=="Total")
df_2016 <- df_2016 %>%  filter(`AgEc Zone`!="Total")

df_2016 <- df_2016 %>% 
  rename(precenatge_yld_loss_tonnes_per_production_GU =`yield loss as percentage of crop production`) %>% 
  mutate(precenatge_yld_loss_tonnes_per_production_GU = precenatge_yld_loss_tonnes_per_production_GU *100) 

str(df_2016)


#### national averages
#2024

fallow_2024_national <- read.csv("W:/Economic impact of weeds round 2/model/plotting and checking results/fallow_weed1_2_outputs_national_summary.csv")
str(fallow_2024_national)


national_yield_loss_tonnes_per_production <-fallow_2024_national$all_crop_yield_loss_tonnes_per_production_GU
national_yield_loss_tonnes_per_ha         <-fallow_2024_national$all_crop_yield_loss_tonnes_per_ha_GU        
national_revenue_due_yldloss              <-fallow_2024_national$all_crop_revenue_due_yldloss_per_ha_GU
national_revenue_due_yldlossplus_N        <-fallow_2024_national$all_crop_revenue_due_yldloss_plus_N_per_ha_GU   



#2016
str(df_2016_national)

national2016_yield_loss_tonnes_per_production <-df_2016_national$`yield loss as percentage of crop production`*100
national2016_yield_loss_tonnes_per_ha         <-df_2016_national$`Yield loss per ha`         
national2016_revenue_due_yld_per_ha           <-df_2016_national$`Revenue loss per ha`



#################  Plots

#yld loss as % of production

str(fallow_2024)
str(df_2016)

national_yield_loss_tonnes_per_production
national2016_yield_loss_tonnes_per_production


fallow_2024 %>% 
  ggplot(aes(x= AEZ.for.report, y = all_crop_yield_loss_tonnes_per_production_GU*100))+
  geom_point(colour = "red", size=2.5)+
  geom_point(data = df_2016, aes(x = `AgEc Zone`, y = `precenatge_yld_loss_tonnes_per_production_GU`), colour = "green4" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 and 2016 - % of Production loss for subsquent crop due to fallow weeds",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss as % of production (tonnes of yield)", 
  )+
  geom_hline(yintercept=national_yield_loss_tonnes_per_production, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=national2016_yield_loss_tonnes_per_production, linetype="dashed", 
             color = "green4", size=.5)



ggsave(
  device = "png",
  filename = "2024_plus_2016_fallowYld_loss_plot_precenatge_yld_loss_production_t.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)



#yld loss as t/ha

str(fallow_2024) # all_crop_yield_loss_tonnes_per_ha_GU
str(df_2016) # `Yield Loss per t/ Ha`

df_2016$`Yield Loss per t/ Ha` <- as.double(df_2016$`Yield Loss per t/ Ha`)

national_yield_loss_tonnes_per_ha
national2016_yield_loss_tonnes_per_ha



fallow_2024 %>% 
  ggplot(aes(x= AEZ.for.report, y = all_crop_yield_loss_tonnes_per_ha_GU))+
  geom_point(colour = "red", size=2.5)+
  geom_point(data = df_2016, aes(x = `AgEc Zone`, y = `Yield Loss per t/ Ha`), colour = "green4" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 and 2016 - Yield loss (t/ha) for subsquent crop due to fallow weeds",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Yield loss t/ha", 
  )+
  geom_hline(yintercept=national_yield_loss_tonnes_per_ha, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=national2016_yield_loss_tonnes_per_ha, linetype="dashed", 
             color = "green4", size=.5)



ggsave(
  device = "png",
  filename = "2024_plus_2016_fallowYld_loss_t_ha_plot.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)



#Rev loss as $/ha only yield

str(fallow_2024) # all_crop_revenue_due_yldloss_per_ha_GU 
names(df_2016) # `Costs per $/aHa`

df_2016$`Costs per $/aHa` <- as.double(df_2016$`Costs per $/aHa`)

national_revenue_due_yldloss
national2016_revenue_due_yld_per_ha

fallow_2024 %>% 
  ggplot(aes(x= AEZ.for.report, y = all_crop_revenue_due_yldloss_per_ha_GU ))+
  geom_point(colour = "red", size=2.5)+
  geom_point(data = df_2016, aes(x = `AgEc Zone`, y = `Costs per $/aHa`), colour = "green4" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 and 2016 - Revenue loss ($/ha) for subsquent crop due to fallow weeds",
    subtitle = "2024: yield losses no extra nitrogen",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Revenue loss ($/ha)", 
  )+
  geom_hline(yintercept=national_revenue_due_yldloss, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=national2016_revenue_due_yld_per_ha, linetype="dashed", 
             color = "green4", size=.5)



ggsave(
  device = "png",
  filename = "2024_plus_2016_fallowRev_loss_2024yld_only_plot.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)


#Rev loss as $/ha only yield

str(fallow_2024) # all_crop_revenue_due_yldloss_per_ha_GU 
names(df_2016) # `Costs per $/aHa`

df_2016$`Costs per $/aHa` <- as.double(df_2016$`Costs per $/aHa`)

national_revenue_due_yldlossplus_N
national2016_revenue_due_yld_per_ha

fallow_2024 %>% 
  ggplot(aes(x= AEZ.for.report, y = all_crop_revenue_due_yldloss_plus_N_per_ha_GU ))+
  geom_point(colour = "red", size=2.5)+
  geom_point(data = df_2016, aes(x = `AgEc Zone`, y = `Costs per $/aHa`), colour = "green4" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))+
  labs(
    title = "2024 and 2016 - Revenue loss ($/ha) for subsquent crop due to fallow weeds",
    subtitle = "2024: yield losses plus extra nitrogen",
    caption = "Values ARE grossed up. The red and green line is national for 2024 and 2016 respectively.",
    x = "",
    y = "Revenue loss ($/ha)", 
  )+
  geom_hline(yintercept=national_revenue_due_yldlossplus_N, linetype="dashed", 
             color = "red", size=.5)+
  geom_hline(yintercept=national2016_revenue_due_yld_per_ha, linetype="dashed", 
             color = "green4", size=.5)



ggsave(
  device = "png",
  filename = "2024_plus_2016_fallowRev_loss_2024yld_Plus_N_plot.png",
  path= "W:/Economic impact of weeds round 2/model/plotting and checking results/",
  width=9.5,
  height = 6.28,
  dpi=600
)
