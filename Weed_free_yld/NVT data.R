#### NVT data ####

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

library(lubridate)



NVT <-             read.csv("W:/Economic impact of weeds round 2/NVT/jaxs processing/NVT_with_AEZ.csv")
NVT_old_project <- read.csv("W:/Economic impact of weeds round 2/NVT/jaxs processing/NVT_Old_project_with_AEZ.csv")

str(NVT)
### filter data that does not fall in zone we want.
unique(NVT$AEZ)
 
NVT <- NVT %>% 
  filter(AEZ != "Excluded") %>% 
  filter(AEZ != "WA Ord") %>% 
  filter(AEZ != " ") 


str(NVT_old_project)
### filter data that does not fall in zone we want.
unique(NVT_old_project$AEZ)

NVT_old_project <- NVT_old_project %>% 
  filter(AEZ != "Excluded") %>% 
  filter(AEZ != "WA Ord") %>% 
  filter(AEZ != " ") 


################################################################################

#Years of coverage

unique(NVT_old_project$Year)
unique(NVT$Year)

names(NVT_old_project)

NVT_old_project <- NVT_old_project %>% select(Year, Name, SowingDate, HarvestDat,Trial_Mean, AEZ)
NVT_old_project <- NVT_old_project %>% rename(
  crop = Name,
  yield = Trial_Mean,
  Sowing_date = SowingDate,
  harvest_date = HarvestDat
)
   
names(NVT)

NVT <- NVT %>% select(Year, Name, SowingDate, HarvestDat,Trial_Mean, AEZ)
NVT <- NVT %>% rename(
  crop = Name,
  yield = Trial_Mean,
  Sowing_date = SowingDate,
  harvest_date = HarvestDat
)
        
 
NVT_all <- rbind(NVT_old_project, NVT)


rm(NVT_old_project, NVT)


### stuff around getting new crop type pulses - for the summary data
NVT_all$crop <- as.character(NVT_all$crop)
str(NVT_all$crop)

NVT_all <- NVT_all %>%  mutate(crop = case_when(
  crop == "Chickpea" ~  "Pulses",
  crop == "Faba Bean" ~ "Pulses",
  crop == "Field Pea" ~ "Pulses",
  crop == "Lupin" ~     "Pulses",
  crop == "Lentil" ~    "Pulses",
  TRUE ~ crop
))

unique(NVT_all$crop)

NVT_all$crop <- factor(NVT_all$crop, 
                          levels = c("Wheat", "Barley", "Oat","Canola", 
                                     "Pulses" ,
                                     "Sorghum" ,
                                     "Triticale"
                          ))




################################################################################
## the first year range means it was sown in that year and then either harvested that year or early the following year

NVT_all <- NVT_all %>%  mutate(Year_season = case_when(
  Year == 2011 ~ "2011-2012",
  Year == 2012 ~ "2012-2013",
  Year == 2013 ~ "2013-2014",
  
  Year == 2014 ~ "2014-2015",
  Year == 2015 ~ "2015-2016",
  Year == 2016 ~ "2016-2017",
  Year == 2017 ~ "2017-2018",
  
  Year == 2018 ~ "2018-2019", 
  Year == 2019 ~ "2019-2020", 
  Year == 2020 ~ "2020-2021", 
  
  Year == 2021 ~ "2021-2022",  
  Year == 2022 ~ "2022-2023"))




### Make a grouping long_term_trend, 2016_study, This_study



### Grouping long_term_trend, 2016_study, This_study

NVT_all <- NVT_all %>%  mutate(Grouping_years = 
                         case_when(
  Year_season == "2011-2012" ~ "2016 Study",
  Year_season == "2012-2013" ~ "2016 Study",
  Year_season == "2013-2014" ~ "2016 Study",
  
  Year_season == "2018-2019" ~ "Current study",
  Year_season == "2019-2020" ~ "Current study",
  Year_season == "2020-2021" ~ "Current study",
  
  
  TRUE                      ~ "other"
  ))

NVT_all_temp <- NVT_all %>% mutate(Grouping_years = "All years")

NVT_with_duplication <- rbind(NVT_all, NVT_all_temp)  

str(NVT_with_duplication)
unique(NVT_with_duplication$crop)


## order the data crop type

NVT_with_duplication$crop <- factor(NVT_with_duplication$crop, 
                                    levels = c("Wheat", "Barley", "Oat","Canola", 
                                               "Pulses" ,
                                               "Sorghum" ,
                                               "Triticale"
                                               ))
unique(NVT_with_duplication$Grouping_years)
NVT_with_duplication$Grouping_years <- factor(NVT_with_duplication$Grouping_years, 
                                    levels = c( "2016 Study", "Current study","All years","Most recent years"
                                    ))

unique(NVT_with_duplication$AEZ)

summary_df <- NVT_with_duplication %>% 
  filter(Grouping_years != "other") %>% 
  group_by(Grouping_years, AEZ, crop) %>% 
  summarize(m=mean(yield))
summary_df <- ungroup(summary_df)

################################################################################
### wheat #####################################################################
crop_type <- "wheat"
str(crop_type)
summary_df_crop_type <- summary_df %>%  filter(crop == "Wheat")

plot1<-
  NVT_with_duplication %>%  
  filter(crop=="Wheat") %>% 
  filter(Grouping_years != "other") %>% 
  filter(crop != "Triticale") %>% 
  ggplot( mapping = aes(x = Grouping_years, 
                        y = yield ,
                        #group = name,
                        fill=Grouping_years)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  ) +
  ylim(1, 12)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  labs(x = "", y = "Yield t/ha", fill = "")+
  labs(title = paste0("NVT yield for: ",  crop_type))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(.~AEZ)

plot1 + geom_text(
  data    = summary_df_crop_type,
  mapping = aes(x = Grouping_years, y = 12, label = round(m, 2)),size=3)

ggsave(
  device = "png",
  filename = paste0("NVT_",   crop_type,".png"),
  path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
  width=9.5,
  height = 6.28,
  dpi=600
) 
################################################################################
### Oat #####################################################################
crop_type <- "Oat"
unique(summary_df$crop)
summary_df_crop_type <- summary_df %>%  filter(crop == "Oat")

plot1<-
  NVT_with_duplication %>%  
  filter(crop=="Oat") %>% 
  filter(Grouping_years != "other") %>% 
  filter(crop != "Triticale") %>% 
  ggplot( mapping = aes(x = Grouping_years, 
                        y = yield ,
                        #group = name,
                        fill=Grouping_years)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  ) +
  ylim(1, 12)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  labs(x = "", y = "Yield t/ha", fill = "")+
  labs(title = paste0("NVT yield for: ",   crop_type))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(.~AEZ)

plot1 + geom_text(
  data    = summary_df_crop_type,
  mapping = aes(x = Grouping_years, y = 12, label = round(m, 2)),size=3)

ggsave(
  device = "png",
  filename = paste0("NVT_",   crop_type,".png"),
  path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
  width=9.5,
  height = 6.28,
  dpi=600
)   

################################################################################
### Barley #####################################################################

crop_type <- "Barley"
unique(summary_df$crop)
summary_df_crop_type <- summary_df %>%  filter(crop == "Barley")

plot1<-
  NVT_with_duplication %>%  
  filter(crop=="Barley") %>% 
  filter(Grouping_years != "other") %>% 
  filter(crop != "Triticale") %>% 
  ggplot( mapping = aes(x = Grouping_years, 
                        y = yield ,
                        #group = name,
                        fill=Grouping_years)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  ) +
  ylim(1, 12)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  labs(x = "", y = "Yield t/ha", fill = "")+
  labs(title = paste0("NVT yield for: ",   crop_type))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(.~AEZ)

plot1 + geom_text(
  data    = summary_df_crop_type,
  mapping = aes(x = Grouping_years, y = 12, label = round(m, 2)),size=3)

ggsave(
  device = "png",
  filename = paste0("NVT_",   crop_type,".png"),
  path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
  width=9.5,
  height = 6.28,
  dpi=600
)   

################################################################################
### Canola #####################################################################

crop_type <- "Canola"
unique(summary_df$crop)
summary_df_crop_type <- summary_df %>%  filter(crop == "Canola")

plot1<-
  NVT_with_duplication %>%  
  filter(crop=="Canola") %>% 
  filter(Grouping_years != "other") %>% 
  filter(crop != "Triticale") %>% 
  ggplot( mapping = aes(x = Grouping_years, 
                        y = yield ,
                        #group = name,
                        fill=Grouping_years)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  ) +
  ylim(1, 12)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  labs(x = "", y = "Yield t/ha", fill = "")+
  labs(title = paste0("NVT yield for: ",   crop_type))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(.~AEZ)

plot1 + geom_text(
  data    = summary_df_crop_type,
  mapping = aes(x = Grouping_years, y = 12, label = round(m, 2)),size=3)

ggsave(
  device = "png",
  filename = paste0("NVT_",   crop_type,".png"),
  path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
  width=9.5,
  height = 6.28,
  dpi=600
)   


################################################################################
### Pulses (Chickpea,  Faba Bean, Field Pea, Lupin,    Lentil  #################


crop_type <- "Pulses"
unique(summary_df$crop)
summary_df_crop_type <- summary_df %>%  filter(crop == "Pulses")





###### something wrong with below plot with the summary file


plot1<-
  NVT_with_duplication %>%  
  filter(crop=="Pulses") %>% 
  filter(Grouping_years != "other") %>% 
  filter(crop != "Triticale") %>% 
  ggplot( mapping = aes(x = Grouping_years, 
                        y = yield ,
                        #group = name,
                        fill=Grouping_years)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  ) +
  ylim(1, 12)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  labs(x = "", y = "Yield t/ha", fill = "")+
  labs(title = paste0("NVT yield for: ",   crop_type))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(.~AEZ)

plot1 + geom_text(
  data    = summary_df_crop_type,
  mapping = aes(x = Grouping_years, y = 12, label = round(m, 2)),size=3)

ggsave(
  device = "png",
  filename = paste0("NVT_",   crop_type,".png"),
  path= "W:/Economic impact of weeds round 2/NVT/jaxs processing/plots/",
  width=9.5,
  height = 6.28,
  dpi=600
)   
#################################################################################

write.csv(summary_df, "W:/Economic impact of weeds round 2/NVT/jaxs processing/summary_NVT_yrs_group.csv",  row.names = FALSE)
