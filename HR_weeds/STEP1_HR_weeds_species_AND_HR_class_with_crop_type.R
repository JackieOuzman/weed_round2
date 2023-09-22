
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

## more formatting

str(HR_weed)
unique(HR_weed$Species)
HR_weed$Species <- toupper(HR_weed$Species)


################################################################################

## new clm for weed type - grouping

str(HR_weed)
unique(HR_weed$Species)
HR_weed <- HR_weed %>% 
  mutate(Weed_grouping = case_when(
    Species == "FLEABANE" |
      Species == "INDIAN HEDGE MUSTARD" |
      Species == "SOWTHISTLE" |
      Species == "WILD RADISH" |
      Species == "WILD TURNIP"  ~ "broadleaf",
      TRUE ~ "grass"
        ))



### Recode crop into groups and drop hort crops
HR_weed$Crop <- str_trim(HR_weed$Crop)
unique(HR_weed$Crop)

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
#################################################################################
############  Summary #########################################################

str(HR_weed)
## recode the NA to not tested (This is what John said - not convinced tho)

HR_weed <- HR_weed %>%  mutate_at(vars("Gp_1_fop_1":"Gp_27_Gp_6"), ~replace_na(.,"not tested"))

write.csv(HR_weed, "W:/Economic impact of weeds round 2/HR/Jackie_working/HR_weeds/HR_weed_step1a.csv")



###############################################################################
### break up data into Herbicide grouping
### non selective group 9 or M
names(HR_weed)
HR_weed_Non_Selective <- HR_weed %>% select(ID_Jaxs:"Weed density",Gp_9, Weed_grouping,crop_grouping)

names(HR_weed_Non_Selective)

################################################################################
## create a new columns that are HR weed to non selective group 9 etc..
unique(HR_weed$Gp_9)
#for All
HR_weed_Non_Selective <- HR_weed_Non_Selective %>% 
  mutate(All_Species = case_when(
    Gp_9 == "DR" ~ "RESIST" ,
    Gp_9 == "RESIST" ~ "RESIST" ,
    Gp_9 == "SUSC" ~ "SUSC" ,
    TRUE ~ "NOT_TESTED"
  ))





## create a new columns that are HR weed to selective group 1, 2, 3, 4, 5 ,12 etc..
names(HR_weed)
unique(HR_weed$Species)
names(HR_weed)
HR_weed_Selective <- HR_weed %>% select(ID_Jaxs:"Weed density",Gp_1_fop_1:Gp_2_Triazolopyramidines,
                                        Gp_3_Dinitroanilines:Gp_3_Benzamides,
                                        Gp_4,
                                        Gp_5,
                                        Gp_12,Weed_grouping,crop_grouping)

names(HR_weed_Selective)
#for all
HR_weed_Selective_step1 <- HR_weed_Selective %>% 
  mutate(All_Species_Resist = case_when(
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
      Gp_12 == "DR"|Gp_12 == "RESIST" 
    ~ "RESIST",
    TRUE ~ "OTHER"
  ))
    
HR_weed_Selective_step1 <- HR_weed_Selective_step1 %>% 
  mutate(All_Species_SUSC = case_when(
    Gp_1_fop_1 == "SUSC"|
      Gp_1_fop_2 == "SUSC"|
      Gp_1_fop_3 == "SUSC" |
      Gp_1_clethodim == "SUSC" | 
      Gp_1_dim == "SUSC" |
      Gp_1_den == "SUSC" |
      Gp_2_Sulfonylureas == "SUSC"|
      Gp_2_Imidazolinones == "SUSC"|
      Gp_2_Triazolopyramidines == "SUSC"|
      Gp_3_Dinitroanilines == "SUSC"|
      Gp_3_Benzamides == "SUSC"|
      Gp_4 == "SUSC"|
      Gp_5 == "SUSC"|
      Gp_12 == "SUSC"
    ~ "SUSC",
        TRUE ~ "OTHER"
  ))


str(HR_weed_Selective_step1)
HR_weed_Selective_step2 <- HR_weed_Selective_step1 %>% select(ID_Jaxs:"Weed density",Weed_grouping:All_Species_SUSC  )

HR_weed_Selective_step2 <- HR_weed_Selective_step2 %>% 
  mutate(Not_tested_any_selective = case_when(
    All_Species_Resist == "OTHER"& All_Species_SUSC == "OTHER" ~ "NOT_TESTED",
    TRUE ~ paste(All_Species_Resist,"_", All_Species_SUSC)
  ))
unique(HR_weed_Selective_step2$Not_tested_any_selective)


HR_weed_Selective_step2 <- HR_weed_Selective_step2 %>% 
  mutate(All_Species = case_when(
    Not_tested_any_selective =="RESIST _ OTHER" ~ "RESIST",
    Not_tested_any_selective =="RESIST _ SUSC" ~ "RESIST",
    Not_tested_any_selective =="OTHER _ SUSC" ~ "SUSC",
    Not_tested_any_selective =="NOT_TESTED" ~ "NOT_TESTED",
    TRUE ~ Not_tested_any_selective))

names(HR_weed_Selective_step2)

HR_weed_Selective_step2 <- HR_weed_Selective_step2 %>% select(ID_Jaxs:"Weed density",Weed_grouping:crop_grouping,  All_Species )




#################### TIDY UP

rm(HR_weed_Selective, HR_weed_Selective_step1, test)

str(HR_weed_Selective_step2)
str(HR_weed_Non_Selective)
HR_weed_Non_Selective <- HR_weed_Non_Selective %>% select(ID_Jaxs, All_Species)
HR_weed_Non_Selective <- HR_weed_Non_Selective %>% rename(All_Species_non_selective = All_Species)
HR_weed_Selective_step2 <- HR_weed_Selective_step2 %>% rename(All_Species_selective = All_Species)


HR_Weed_HR_Class <- left_join(HR_weed_Selective_step2,HR_weed_Non_Selective)






## create a new columns that are HR weed to selective and non selective
names(HR_weed)

unique(HR_Weed_HR_Class$All_Species_selective)
unique(HR_Weed_HR_Class$All_Species_non_selective)

#All species
HR_Weed_HR_Class <- HR_Weed_HR_Class %>% 
  mutate(BOTH = case_when(
    All_Species_selective ==  "RESIST"& All_Species_non_selective ==  "RESIST"  ~ "RESIST",
    TRUE ~ paste0(All_Species_selective, "_",All_Species_non_selective)
  ))


unique(HR_Weed_HR_Class$BOTH)

#"RESIST"    #keep this        
#"NOT_TESTED_NOT_TESTED"
# "RESIST_NOT_TESTED"    
# "SUSC_NOT_TESTED"     
# "RESIST_SUSC"      
# "SUSC_SUSC"            
# "SUSC_RESIST"   
# "NOT_TESTED_SUSC"  
# "NOT_TESTED_RESIST"    

HR_Weed_HR_Class <- HR_Weed_HR_Class %>% 
  mutate(BOTH = case_when(
    BOTH ==  "NOT_TESTED_NOT_TESTED" ~ "NOT_TESTED",
    BOTH ==  "RESIST" ~ "RESIST",
    TRUE ~ "OTHER")
  )
unique(HR_Weed_HR_Class$BOTH)


write.csv(HR_Weed_HR_Class, "W:/Economic impact of weeds round 2/HR/Jackie_working/HR_weeds/HR_Weed_HR_Class_step1b.csv")


