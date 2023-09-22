
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)


################################################################################
###    Bring in the data ########
################################################################################
#"W:\Economic impact of weeds round 2\HR\raw_data\HR Survey database All no lat long.xlsx"

HR_weed<-  read_excel("W:/Economic impact of weeds round 2/HR/raw_data/HR Survey database All no lat long.xlsx", 
                                                     sheet = "All species")




## Add ID to dataset so each row has a unique ID
HR_weed <- HR_weed %>% 
  mutate(ID = row_number()) %>% 
  mutate(ID_Jaxs = paste0(ID,"_HR"))

names(HR_weed)
HR_weed <- HR_weed %>% select(Number:  Gp_27_Gp_6, ID_Jaxs)
HR_weed <- HR_weed %>% select(ID_Jaxs, everything())
## make it long 

HR_weed <- HR_weed%>%  dplyr::select(ID_Jaxs:Gp_27_Gp_6)
str(HR_weed)
dim(HR_weed)
### some formatting and recoding of RESIST , or DR or SUSC

HR_weed <- HR_weed %>% mutate(across(13:33, toupper))
HR_weed <- HR_weed %>% mutate(across(13:33, str_trim))


coding_for_weeds <-HR_weed %>% distinct() %>%  select(Gp_1_fop_1:Gp_27_Gp_6)
str(coding_for_weeds)
coding_for_weeds_long <- coding_for_weeds %>% pivot_longer(cols = Gp_1_fop_1 :Gp_27_Gp_6,
                                                           values_to = "text_coding")
coding_for_weeds_v1 <-coding_for_weeds_long %>% distinct(text_coding)
coding_for_weeds_v1

HR_weed <- HR_weed %>% mutate_at(c(13:33),funs(str_replace(., "D", "DR")))
HR_weed <- HR_weed %>% mutate_at(c(13:33),funs(str_replace(., "S", "SUSC")))
## this makes some junk that needs cleaning up
HR_weed <- HR_weed %>% mutate_at(c(13:33),funs(str_replace(., "RESUSCIST", "RESIST")))
HR_weed <- HR_weed %>% mutate_at(c(13:33),funs(str_replace(., "DRR", "DR")))
HR_weed <- HR_weed %>% mutate_at(c(13:33),funs(str_replace(., "SUSCUSC", "SUSC")))

coding_for_weeds <-HR_weed %>% distinct() %>%  select(Gp_1_fop_1:Gp_27_Gp_6)
str(coding_for_weeds)
coding_for_weeds_long <- coding_for_weeds %>% pivot_longer(cols = Gp_1_fop_1 :Gp_27_Gp_6,
                                                           values_to = "text_coding")
coding_for_weeds_v1 <-coding_for_weeds_long %>% distinct(text_coding)
coding_for_weeds_v1

rm(coding_for_weeds_long, coding_for_weeds_v1, coding_for_weeds)


################################################################################

## create a new columns that are HR weed to non selective group 9 etc..

str(HR_weed)
unique(HR_weed$Species)
HR_weed$Species <- toupper(HR_weed$Species)



unique(HR_weed$Gp_9)



#for RYEGRASS
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_RYEGRASS = case_when(
    Gp_9 == "DR" & Species == "RYEGRASS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "Ryvegrass"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))


#for Wild oats
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_WILD_OATS = case_when(
    Gp_9 == "DR" & Species == "WILD OATS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "WILD OATS"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))


#for BARLEY GRASS
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_BARLEY_GRASS = case_when(
    Gp_9 == "DR" & Species == "BARLEY GRASS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "BARLEY GRASS"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))


#for BROME GRASS
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_BROME_GRASS = case_when(
    Gp_9 == "DR" & Species == "BROME GRASS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "BROME GRASS"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))

#for PHALARIS
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_PHALARIS = case_when(
    Gp_9 == "DR" & Species == "PHALARIS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "PHALARIS"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))


#for WILD RADISH
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_WILD_RADISH = case_when(
    Gp_9 == "DR" & Species == "WILD RADISH"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "WILD RADISH"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))

#for INDIAN HEDGE MUSTARD
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_INDIAN_HEDGE_MUSTARD = case_when(
    Gp_9 == "DR" & Species == "INDIAN HEDGE MUSTARD"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "INDIAN HEDGE MUSTARD"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))


#for SOWTHISTLE
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_SOWTHISTLE = case_when(
    Gp_9 == "DR" & Species == "SOWTHISTLE"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "SOWTHISTLE"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))

#for WILD TURNIP
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_WILD_TURNIP = case_when(
    Gp_9 == "DR" & Species == "WILD TURNIP"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "WILD TURNIP"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))

#for FEATHERTOP RHODES GRASS
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_FEATHERTOP_RHODES_GRASS = case_when(
    Gp_9 == "DR" & Species == "FEATHERTOP RHODES GRASS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "FEATHERTOP RHODES GRASS"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))

#for BARNYARD GRASS
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_BARNYARD_GRASS = case_when(
    Gp_9 == "DR" & Species == "BARNYARD GRASS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "BARNYARD GRASS"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))


#for WINDMILL GRASS
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_WINDMILL_GRASS = case_when(
    Gp_9 == "DR" & Species == "WINDMILL GRASS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "WINDMILL GRASS"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))

#for LIVERSEED GRASS
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_LIVERSEED_GRASS = case_when(
    Gp_9 == "DR" & Species == "LIVERSEED GRASS"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "LIVERSEED GRASS"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))


#for FLEABANE
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_FLEABANE = case_when(
    Gp_9 == "DR" & Species == "FLEABANE"~ "RESIST" ,
    Gp_9 == "RESIST" & Species == "FLEABANE"~ "RESIST" ,
    TRUE ~ "OTHER"
  ))





## create a new columns that are HR weed to selective group 1, 2, 3, 4, 5 ,12 etc..
names(HR_weed)
unique(HR_weed$Species)

#for RYEGRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_RYEGRASS = case_when(
      Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
        
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
        
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
        
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
        
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "RYEGRASS" 
        
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))

#for WILD OATS
HR_weed <- HR_weed %>% 
  mutate(Selective_WILD_OATS = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "WILD OATS" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))


#for BARLEY GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_BARLEY_GRASS = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "BARLEY GRASS" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))



#for BROME  GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_BROME_GRASS = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "BROME GRASS" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))

#PHALARIS
HR_weed <- HR_weed %>% 
  mutate(Selective_PHALARIS = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "PHALARIS" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))



#WILD RADISH
HR_weed <- HR_weed %>% 
  mutate(Selective_WILD_RADISH = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "WILD RADISH" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))

#INDIAN HEDGE MUSTARD
HR_weed <- HR_weed %>% 
  mutate(Selective_INDIAN_HEDGE_MUSTARD = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "INDIAN HEDGE MUSTARD" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))



#SOWTHISTLE
HR_weed <- HR_weed %>% 
  mutate(Selective_SOWTHISTLE = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "SOWTHISTLE" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))
#WILD TURNIP
HR_weed <- HR_weed %>% 
  mutate(Selective_WILD_TURNIP = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "WILD TURNIP" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))



#FEATHERTOP RHODES GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_FEATHERTOP_RHODES_GRASS = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "FEATHERTOP RHODES GRASS" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))


#BARNYARD GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_BARNYARD_GRASS = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "BARNYARD GRASS" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))


#WINDMILL GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_WINDMILL_GRASS = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "WINDMILL GRASS" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))


#LIVERSEED GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_LIVERSEED_GRASS = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "LIVERSEED GRASS" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))

#FLEABANE
HR_weed <- HR_weed %>% 
  mutate(Selective_FLEABANE = case_when(
    Gp_1_fop_1 == "DR"|Gp_1_fop_1 == "RESIST"| 
      Gp_1_fop_2 == "DR"|Gp_1_fop_2 == "RESIST" | 
      Gp_1_fop_3 == "DR"|Gp_1_fop_3 == "RESIST" | 
      Gp_1_clethodim == "DR"|Gp_1_clethodim == "RESIST" | 
      Gp_1_dim == "DR"|Gp_1_dim == "RESIST" | 
      Gp_1_den == "DR"|Gp_1_den == "RESIST" | 
      
      Gp_2_Sulfonylureas == "DR"|Gp_2_Sulfonylureas == "RESIST" |
      Gp_2_Imidazolinones == "DR"|Gp_2_Imidazolinones == "RESIST" |
      Gp_2_Triazolopyramidines == "DR"|Gp_2_Triazolopyramidines == "RESIST" |
      
      Gp_3_Dinitroanilines == "DR"|Gp_3_Dinitroanilines == "RESIST" |
      Gp_3_Benzamides == "DR"|Gp_3_Benzamides == "RESIST" |
      
      Gp_4 == "DR"|Gp_4 == "RESIST" |
      
      Gp_5 == "DR"|Gp_5 == "RESIST" |
      
      Gp_12 == "DR"|Gp_12 == "RESIST" & Species == "FLEABANE" 
    
    ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))






## create a new columns that are HR weed to selective and non selective
names(HR_weed)

unique(HR_weed$Species)

#RYEGRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_RYEGRASS = case_when(
    Non_Selective_RYEGRASS ==  "RESIST"& Selective_RYEGRASS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))

#WILD OATS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_RYEGRASS = case_when(
    Non_Selective_WILD_OATS ==  "RESIST"& Selective_WILD_OATS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))


#BARLEY GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_BARLEY_GRASS = case_when(
    Non_Selective_BARLEY_GRASS ==  "RESIST"& Selective_BARLEY_GRASS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))

# BROME GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_BROME_GRASS = case_when(
    Non_Selective_BROME_GRASS ==  "RESIST"& Selective_BROME_GRASS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))

# PHALARIS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_PHALARIS = case_when(
    Non_Selective_PHALARIS ==  "RESIST"& Selective_PHALARIS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))

# WILD RADISH
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_WILD_RADISH = case_when(
    Non_Selective_WILD_RADISH ==  "RESIST"& Selective_WILD_RADISH ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))
# INDIAN HEDGE MUSTARD
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_INDIAN_HEDGE_MUSTARD = case_when(
    Non_Selective_INDIAN_HEDGE_MUSTARD ==  "RESIST"& Selective_INDIAN_HEDGE_MUSTARD ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))


# SOWTHISTLE
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_SOWTHISTLE = case_when(
    Non_Selective_SOWTHISTLE ==  "RESIST"& Selective_SOWTHISTLE ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))


# "WILD TURNIP
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_WILD_TURNIP = case_when(
    Non_Selective_WILD_TURNIP ==  "RESIST"& Selective_WILD_TURNIP ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))
# "FEATHERTOP RHODES GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_FEATHERTOP_RHODES_GRASS = case_when(
    Non_Selective_FEATHERTOP_RHODES_GRASS ==  "RESIST"& Selective_FEATHERTOP_RHODES_GRASS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))


# BARNYARD GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_BARNYARD_GRASS = case_when(
    Non_Selective_BARNYARD_GRASS ==  "RESIST"& Selective_BARNYARD_GRASS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))
# WINDMILL GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_WINDMILL_GRASS = case_when(
    Non_Selective_WINDMILL_GRASS ==  "RESIST"& Selective_WINDMILL_GRASS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))

# LIVERSEED GRASS
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_LIVERSEED_GRASS = case_when(
    Non_Selective_LIVERSEED_GRASS ==  "RESIST"& Selective_LIVERSEED_GRASS ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))


# FLEABANE
HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective_FLEABANE = case_when(
    Non_Selective_FLEABANE ==  "RESIST"& Selective_FLEABANE ==  "RESIST"  ~ "RESIST",
    TRUE ~ "OTHER"
  ))











###############################################################################
names(HR_weed)



#### drop AEZ that we wont use ###
unique(HR_weed$GRDC_AEZ)
HR_weed <-HR_weed %>% rename(AEZ = GRDC_AEZ)

HR_weed <- HR_weed %>% filter(
  AEZ ==  "WA Central" |
    AEZ ==  "WA Northern" |
    AEZ ==  "WA Eastern" |
    AEZ ==  "WA Sandplain" |
    AEZ == "SA Vic Bordertown Wimmera"  |
    AEZ == "SA Vic Mallee"   |
    AEZ == "NSW Vic Slopes" |
    AEZ ==  "Vic High Rainfall" |
    AEZ ==  "NSW NE Qld SE" |
    AEZ == "SA Mid North Lower Yorke Eyre" |
    AEZ == "Tas Grain"      |
    AEZ ==   "NSW Central"  |
    AEZ ==  "NSW NW Qld SW" |   
    AEZ == "Qld Central"     )



################################################################################
### Recode crop into groups and drop hort crops

list_crops <- as.data.frame(unique(HR_weed$Crop) )
list_crops <- arrange(list_crops,HR_weed$Crop)

names(list_crops)
HR_weed$Crop <- str_trim(HR_weed$Crop)

HR_weed <- HR_weed %>% 
  mutate(crop_grouping = case_when(
     Crop == "Wheat" |
       Crop == 	"US Wheat" |
      Crop == "wheat."|
      Crop == "oat"|
      Crop == "barley"|
      Crop == "baley"|
       Crop == "Barley"|
       Crop == "US Barley"|
       
      Crop == "oats/barley"|
      Crop == "oats"|
      Crop == "barley/oat (undersown medic0"|
       Crop == "Triticale"|
       Crop == "Oats"|
       
       Crop == "oats/Barley"|
       Crop == "Cereal crop"|
       
      Crop == "wheta" ~ "Cereals",
     
     
       Crop == "Chickpeas" |
       Crop == "Chickpea" |
       Crop == "Freezer Peas" |
       Crop == "Lupin" |
       Crop == "Field pea" |
       Crop == "Canola"|
       
       Crop == "Field peas" |
       Crop == "Chick Peas" |
       Crop == "Field peas" |
       Crop == "Field pea" |
       Crop == "Beans" |
       Crop == "Chick peas" |
       Crop == "Faba beans" |
       Crop == "Lentils" |
     
       Crop == "Linseed" |
       Crop == "lentils" |
       Crop == "Field Peas" |
       Crop == "Peas" |
       Crop == "Mung bean" | 
       Crop == "Pigeon pea" | 
       
      Crop == "canola" |
      Crop == "CHICK pea" | 
      Crop == "pea" | 
      Crop == "field pea" |
      Crop == "peas" |
      Crop == "chickpea" |
      Crop == "lupin" |
       Crop == "Lupins" |
       Crop == "Albus lupins" |
      Crop == "lupins"  ~ "Broadleaf",
    
    Crop == "pasture"  ~ "Pasture", 
    Crop == "Pasture"  ~ "Pasture", 
    Crop == "Annual pasture"  ~ "Pasture", 
    Crop == "Perennial pasture"  ~ "Pasture", 
    Crop == "Perennial Pasture"  ~ "Pasture", 
    
    Crop == "Sorghum"  ~ "Sorghum", 
    Crop == "Fallow"  ~ "Fallow",
    
    
    TRUE                      ~ "other"
  ))


test <- HR_weed %>% filter(crop_grouping == "other") %>% distinct(Crop)

test









## need to check that some of these crops are coded correctly


unique(HR_weed$crop_grouping)
check_what_coded_other<- HR_weed %>% filter(crop_grouping == "other")
check_what_coded_Broadleaf<- HR_weed %>% filter(crop_grouping == "Broadleaf")

rm(check_what_coded_other, check_what_coded_Broadleaf, list_crops, test)

#################################################################################
############  Summary #########################################################


write.csv(HR_weed, "C:/Users/ouz001/working_from_home_post_Sep2022/weed_round2_offline/HR_weed_test.csv")



################################################################################
### make a list of weeds per zone and crop
str(HR_weed)


################################################################################
## how many samples tested per zone


samples_per_AEZ_crop_group_species <- HR_weed %>%  
  filter(crop_grouping != "other") %>% 
  count(AEZ, crop_grouping,  Species) #

samples_per_AEZ_crop_group_species <- samples_per_AEZ_crop_group_species %>%  rename(`count of samples tested` = n)

samples_per_AEZ_crop_group_species



################################################################################
#count the number of HR weed occurrence per AEZ, weed   
names(HR_weed)
test_1 <- HR_weed %>% select(AEZ, crop_grouping, Species,Non_Selective_RYEGRASS:Selective_and_non_Selective_FLEABANE )
str(test_1)

unique(test_1$Non_Selective_RYEGRASS)
dim(test_1)

test_1 <- test_1 %>% mutate_at(c(3:44),funs(str_replace(., "OTHER", "0")))
test_1 <- test_1 %>% mutate_at(c(3:44),funs(str_replace(., "RESIST", "1")))
test_1 <- test_1 %>% mutate_at(c(3:44),funs(as.double())) 
test_1 <- test_1 %>% mutate_at(vars("Non_Selective_RYEGRASS": "Selective_and_non_Selective_FLEABANE"), as.numeric)

str(test_1)

summary_HR_SP_Type <- test_1 %>% 
  group_by(AEZ, crop_grouping, Species ) %>% 
  summarise(
    NS_RYEGRASS = sum(Non_Selective_RYEGRASS, na.rm = TRUE),
    S_RYEGRASS =  sum(Selective_RYEGRASS, na.rm = TRUE),
    BOTH_RYEGRASS =  sum(Selective_and_non_Selective_RYEGRASS, na.rm = TRUE),
    
    NS_BARLEY_GRASS = sum(Non_Selective_BARLEY_GRASS, na.rm = TRUE),
    S_BARLEY_GRASS =  sum(Selective_BARLEY_GRASS, na.rm = TRUE),
    BOTH_RYEGRASS =  sum(Selective_and_non_Selective_BARLEY_GRASS, na.rm = TRUE),
    
    NS_BROME_GRASS = sum(Non_Selective_BROME_GRASS, na.rm = TRUE),
    S_BROME_GRASS =  sum(Selective_BROME_GRASS, na.rm = TRUE),
    BOTH_RYEGRASS =  sum(Selective_and_non_Selective_BROME_GRASS, na.rm = TRUE),
    
    NS_PHALARIS = sum(Non_Selective_PHALARIS, na.rm = TRUE),
    S_PHALARIS =  sum(Non_Selective_PHALARIS, na.rm = TRUE),
    BOTH_PHALARIS =  sum(Selective_and_non_Selective_PHALARIS, na.rm = TRUE),
    
    NS_WILD_RADISH = sum(Non_Selective_WILD_RADISH, na.rm = TRUE),
    S_WILD_RADISH =  sum(Non_Selective_WILD_RADISH, na.rm = TRUE),
    BOTH_WILD_RADISH =  sum(Selective_and_non_Selective_WILD_RADISH, na.rm = TRUE),
    
    NS_INDIAN_HEDGE_MUSTARD = sum(Non_Selective_INDIAN_HEDGE_MUSTARD, na.rm = TRUE),
    S_INDIAN_HEDGE_MUSTARD =  sum(Non_Selective_INDIAN_HEDGE_MUSTARD, na.rm = TRUE),
    BOTH_INDIAN_HEDGE_MUSTARD =  sum(Selective_and_non_Selective_INDIAN_HEDGE_MUSTARD, na.rm = TRUE),
    
    NS_SOWTHISTLE = sum(Non_Selective_SOWTHISTLE, na.rm = TRUE),
    S_SOWTHISTLE =  sum(Non_Selective_SOWTHISTLE, na.rm = TRUE),
    BOTH_SOWTHISTLE =  sum(Selective_and_non_Selective_SOWTHISTLE, na.rm = TRUE),
    
    NS_WILD_TURNIP = sum(Non_Selective_WILD_TURNIP, na.rm = TRUE),
    S_WILD_TURNIP =  sum(Non_Selective_WILD_TURNIP, na.rm = TRUE),
    BOTH_WILD_TURNIP =  sum(Selective_and_non_Selective_WILD_TURNIP, na.rm = TRUE),
    
    NS_FEATHERTOP_RHODES_GRASS = sum(Non_Selective_FEATHERTOP_RHODES_GRASS, na.rm = TRUE),
    S_FEATHERTOP_RHODES_GRASS =  sum(Non_Selective_FEATHERTOP_RHODES_GRASS, na.rm = TRUE),
    BOTH_FEATHERTOP_RHODES_GRASS =  sum(Selective_and_non_Selective_FEATHERTOP_RHODES_GRASS, na.rm = TRUE),
    
    NS_BARNYARD_GRASS = sum(Non_Selective_BARNYARD_GRASS, na.rm = TRUE),
    S_BARNYARD_GRASS =  sum(Non_Selective_BARNYARD_GRASS, na.rm = TRUE),
    BOTH_BARNYARD_GRASS =  sum(Selective_and_non_Selective_BARNYARD_GRASS, na.rm = TRUE),
    
    NS_WINDMILL_GRASS = sum(Non_Selective_WINDMILL_GRASS, na.rm = TRUE),
    S_WINDMILL_GRASS =  sum(Non_Selective_WINDMILL_GRASS, na.rm = TRUE),
    BOTH_WINDMILL_GRASS =  sum(Selective_and_non_Selective_WINDMILL_GRASS, na.rm = TRUE),
    
    NS_LIVERSEED_GRASS = sum(Non_Selective_LIVERSEED_GRASS, na.rm = TRUE),
    S_LIVERSEED_GRASS =  sum(Non_Selective_LIVERSEED_GRASS, na.rm = TRUE),
    BOTH_LIVERSEED_GRASS =  sum(Selective_and_non_Selective_LIVERSEED_GRASS, na.rm = TRUE),
    
    NS_FLEABANE = sum(Non_Selective_FLEABANE, na.rm = TRUE),
    S_FLEABANE =  sum(Non_Selective_FLEABANE, na.rm = TRUE),
    BOTH_FLEABANE =  sum(Selective_and_non_Selective_FLEABANE, na.rm = TRUE)
  )


##################################################################################

### make it long
summary_HR_SP_Type <- ungroup(summary_HR_SP_Type)
str(summary_HR_SP_Type)


summary_HR_SP_Type_long <- summary_HR_SP_Type %>% pivot_longer(
  cols= c(NS_RYEGRASS: BOTH_FLEABANE),
  names_to = "HR_Type",
  values_to = "count_HR_Type")
  
### Add a col for species

str(summary_HR_SP_Type_long)

summary_HR_SP_Type_long <- summary_HR_SP_Type_long %>% mutate(Species = HR_Type) 

summary_HR_SP_Type_long <- summary_HR_SP_Type_long %>% mutate_at(c(5),funs(str_replace(., "NS_", "")))
summary_HR_SP_Type_long <- summary_HR_SP_Type_long %>% mutate_at(c(5),funs(str_replace(., "S_", "")))
summary_HR_SP_Type_long <- summary_HR_SP_Type_long %>% mutate_at(c(5),funs(str_replace(., "BOTH_", "")))

#### go get the weed counts of each species
str(samples_per_AEZ_crop_group_species)


test2 <- left_join(summary_HR_SP_Type_long, samples_per_AEZ_crop_group_species)


###################################################################################


AEZ_crop_Non_selective_HR_weeds_count <- HR_weed %>% count(AEZ, Non_Selective_Ryegrass, crop_grouping, sort = TRUE)    
AEZ_crop_Non_selective_HR_weeds_count <- AEZ_crop_Non_selective_HR_weeds_count %>%  arrange(AEZ, Non_Selective_Herb)
str(AEZ_crop_Non_selective_HR_weeds_count) 
AEZ_crop_Non_selective_HR_weeds_count <- AEZ_crop_Non_selective_HR_weeds_count %>%  rename(count_Non_selective = n)
AEZ_crop_Non_selective_HR_weeds_count <- AEZ_crop_Non_selective_HR_weeds_count %>% filter(Non_Selective_Herb=="RESIST" )

################################################################################
#count the number of HR weed occurrence per AEZ, weed   
names(HR_weed)

AEZ_crop_selective_HR_weeds_count <- HR_weed %>% count(AEZ, Selective_Herb, crop_grouping, sort = TRUE)    
AEZ_crop_selective_HR_weeds_count <- AEZ_crop_selective_HR_weeds_count %>%  arrange(AEZ, Selective_Herb)
str(AEZ_crop_selective_HR_weeds_count) 
AEZ_crop_selective_HR_weeds_count <- AEZ_crop_selective_HR_weeds_count %>%  rename(count_selective = n)

AEZ_crop_selective_HR_weeds_count <- AEZ_crop_selective_HR_weeds_count %>% filter(Selective_Herb=="RESIST" )
AEZ_crop_selective_HR_weeds_count
################################################################################
#count the number of HR weed occurrence per AEZ, weed   
names(HR_weed)

AEZ_crop_Both_HR_weeds_count <- HR_weed %>% count(AEZ, Selective_and_non_Selective, crop_grouping, sort = TRUE)    
AEZ_crop_Both_HR_weeds_count <- AEZ_crop_Both_HR_weeds_count %>%  arrange(AEZ, Selective_and_non_Selective)
str(AEZ_crop_Both_HR_weeds_count) 
AEZ_crop_Both_HR_weeds_count <- AEZ_crop_Both_HR_weeds_count %>%  rename(count_Both = n)


AEZ_crop_Both_HR_weeds_count <- AEZ_crop_Both_HR_weeds_count %>% filter(Selective_and_non_Selective=="RESIST" )
AEZ_crop_Both_HR_weeds_count
################################################################################
#### join the summary data together
names(AEZ_crop_selective_HR_weeds_count)
names(AEZ_crop_Both_HR_weeds_count)
names(AEZ_crop_Non_selective_HR_weeds_count)
names(samples_per_AEZ_crop_group)

### clm for joining

AEZ_crop_selective_HR_weeds_count <- AEZ_crop_selective_HR_weeds_count %>% mutate(ID_join = paste0(AEZ, crop_grouping))
AEZ_crop_Non_selective_HR_weeds_count <- AEZ_crop_Non_selective_HR_weeds_count %>% mutate(ID_join = paste0(AEZ, crop_grouping))
AEZ_crop_Both_HR_weeds_count <- AEZ_crop_Both_HR_weeds_count %>% mutate(ID_join = paste0(AEZ, crop_grouping))
samples_per_AEZ_crop_group <- samples_per_AEZ_crop_group %>% mutate(ID_join = paste0(AEZ, crop_grouping))


AEZ_crop_HR_weeds <- left_join(AEZ_crop_selective_HR_weeds_count,AEZ_crop_Non_selective_HR_weeds_count)
AEZ_crop_HR_weeds <- left_join(AEZ_crop_HR_weeds,AEZ_crop_Both_HR_weeds_count)
AEZ_crop_HR_weeds <- left_join(AEZ_crop_HR_weeds,samples_per_AEZ_crop_group)

names(AEZ_crop_HR_weeds)
AEZ_crop_HR_weeds <- AEZ_crop_HR_weeds %>% 
  select(AEZ, crop_grouping,"count of samples tested", count_Non_selective, count_selective, count_Both )



## add a percent 

str(AEZ_crop_HR_weeds)
AEZ_crop_HR_weeds <- AEZ_crop_HR_weeds %>%
  mutate(percent_occurance_NON_Selective = (count_Non_selective/ `count of samples tested`)*100) %>% 
  mutate(percent_occurance_selective = (count_selective/`count of samples tested`)*100 )%>% 
  mutate(percent_occurance_BOTH = (count_Both/ `count of samples tested` )*100)

AEZ_crop_HR_weeds$percent_occurance_NON_Selective <- round(AEZ_crop_HR_weeds$percent_occurance_NON_Selective, 2)
AEZ_crop_HR_weeds$percent_occurance_selective <- round(AEZ_crop_HR_weeds$percent_occurance_selective, 2)
AEZ_crop_HR_weeds$percent_occurance_BOTH <- round(AEZ_crop_HR_weeds$percent_occurance_BOTH, 2)



rm(AEZ_crop_Both_HR_weeds_count, AEZ_crop_Non_selective_HR_weeds_count, AEZ_crop_selective_HR_weeds_count, samples_per_AEZ_crop_group)



write.csv(AEZ_crop_HR_weeds, "W:/Economic impact of weeds round 2/HR/Jackie_working/HR_weeds/HR_class_occurance_attempt_BY_AEZ_CROP.csv")








##################################################################################

###summaries the weed class   
str(rank_crop)

rank_crop$weed_class_code <- as.double(rank_crop$weed_class_code)
rank_crop <- rank_crop %>% filter( !is.na(weed_class_code))


#I need a function to cal mode!
mode <- function(codes){
  which.max(tabulate(codes))
}


### recode mode.

## recode weed class with an number
rank_crop_density_mode_display_weed1 <-rank_crop_density_mode_display_weed1 %>% 
  mutate(
    weed_class_Mode = case_when(
      mode ==1 ~ "VL",
      mode ==2 ~ "L",
      mode ==3 ~ "M",
      mode ==4~ "H",
      mode ==5 ~ "VH",
      TRUE ~ "check"
    )
  )

rank_crop_density_mode_display_weed2 <-rank_crop_density_mode_display_weed2 %>% 
  mutate(
    weed_class_Mode = case_when(
      mode ==1 ~ "VL",
      mode ==2 ~ "L",
      mode ==3 ~ "M",
      mode ==4~ "H",
      mode ==5 ~ "VH",
      TRUE ~ "check"
    )
  )

  


 







