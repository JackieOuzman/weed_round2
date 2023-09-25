
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)


################################################################################
###    Bring in the data from step 1########
################################################################################
#
HR_weed<-  read.csv("W:/Economic impact of weeds round 2/HR/Jackie_working/HR_weeds/HR_Weed_HR_Class_step1b.csv")



str(HR_weed)

###############################################################################
#SELECTIVE
SELECTIVE_count_of_test_results_AEZ_CROP_SPEC <- HR_weed %>% 
  group_by(AEZ, crop_grouping, Species, All_Species_selective) %>% 
  summarise(count_HR_TEST= n())
SELECTIVE_count_of_test_results_AEZ_CROP_SPEC


SELECTIVEcount_of_seeds_Tested_AEZ_CROP_SPEC <- HR_weed %>% 
  filter(All_Species_selective != "NOT_TESTED") %>% 
  group_by(AEZ, crop_grouping, Species ) %>% 
  summarise(count_Seeds_TEST= n())
SELECTIVEcount_of_seeds_Tested_AEZ_CROP_SPEC

SELECTIVE <- left_join(SELECTIVE_count_of_test_results_AEZ_CROP_SPEC, SELECTIVEcount_of_seeds_Tested_AEZ_CROP_SPEC)

###############################################################################
#NON_SELECTIVE
NON_SELECTIVE_count_of_test_results_AEZ_CROP_SPEC <- HR_weed %>% 
  group_by(AEZ, crop_grouping, Species, All_Species_non_selective) %>% 
  summarise(count_HR_TEST= n())
NON_SELECTIVE_count_of_test_results_AEZ_CROP_SPEC


NON_SELECTIVEcount_of_seeds_Tested_AEZ_CROP_SPEC <- HR_weed %>% 
  filter(All_Species_non_selective != "NOT_TESTED") %>% 
  group_by(AEZ, crop_grouping, Species ) %>% 
  summarise(count_Seeds_TEST= n())
NON_SELECTIVEcount_of_seeds_Tested_AEZ_CROP_SPEC

NON_SELECTIVE <- left_join(NON_SELECTIVE_count_of_test_results_AEZ_CROP_SPEC, NON_SELECTIVEcount_of_seeds_Tested_AEZ_CROP_SPEC)
  

###############################################################################
#BOTH
BOTH_count_of_test_results_AEZ_CROP_SPEC <- HR_weed %>% 
  group_by(AEZ, crop_grouping, Species, BOTH) %>% 
  summarise(count_HR_TEST= n())
BOTH_count_of_test_results_AEZ_CROP_SPEC


BOTHcount_of_seeds_Tested_AEZ_CROP_SPEC <- HR_weed %>% 
  filter(BOTH != "NOT_TESTED") %>% 
  group_by(AEZ, crop_grouping, Species ) %>% 
  summarise(count_Seeds_TEST= n())
BOTHcount_of_seeds_Tested_AEZ_CROP_SPEC

BOTH <- left_join(BOTH_count_of_test_results_AEZ_CROP_SPEC, BOTHcount_of_seeds_Tested_AEZ_CROP_SPEC)


################################################################################

rm(BOTHcount_of_seeds_Tested_AEZ_CROP_SPEC, BOTH_count_of_test_results_AEZ_CROP_SPEC,
   NON_SELECTIVE_count_of_test_results_AEZ_CROP_SPEC, NON_SELECTIVEcount_of_seeds_Tested_AEZ_CROP_SPEC,
   SELECTIVE_count_of_test_results_AEZ_CROP_SPEC, SELECTIVEcount_of_seeds_Tested_AEZ_CROP_SPEC)
 



###############################################################################
## remove the not tested and 	SUSC

BOTH <- ungroup(BOTH)
str(BOTH)
BOTH <- BOTH %>% filter(BOTH == "RESIST")

NON_SELECTIVE <- ungroup(NON_SELECTIVE)
str(NON_SELECTIVE)
NON_SELECTIVE <- NON_SELECTIVE %>% filter(All_Species_non_selective == "RESIST")

SELECTIVE <- ungroup(SELECTIVE)
str(SELECTIVE)
SELECTIVE <- SELECTIVE %>% filter(All_Species_selective == "RESIST")



################################################################################
## cal percentage values #####

str(SELECTIVE)
str(NON_SELECTIVE)
str(BOTH)

SELECTIVE <- SELECTIVE %>% rename(All_Species = All_Species_selective)
SELECTIVE <- SELECTIVE %>% mutate(percenatge_weed_seed_HR = count_HR_TEST/count_Seeds_TEST*100)
SELECTIVE$percenatge_weed_seed_HR <-round(SELECTIVE$percenatge_weed_seed_HR, 0)

NON_SELECTIVE <- NON_SELECTIVE %>% rename(All_Species = All_Species_non_selective)
NON_SELECTIVE <- NON_SELECTIVE %>% mutate(percenatge_weed_seed_HR = count_HR_TEST/count_Seeds_TEST*100)
NON_SELECTIVE$percenatge_weed_seed_HR <-round(NON_SELECTIVE$percenatge_weed_seed_HR, 0)


BOTH <- BOTH %>% mutate(percenatge_weed_seed_HR = count_HR_TEST/count_Seeds_TEST*100)
BOTH$percenatge_weed_seed_HR <-round(BOTH$percenatge_weed_seed_HR, 0)
###############################################################################

#How to join it all together and not loose any data

###############################################################################

crop_type <- c ("Cereals",
           "Broadleaf",
           "Pasture",
           "other",
           "Fallow",
           "Sorghum" )
Weeds <- c (
  "Barley grass",
  "Barnyard grass",
  "Brome grass",
  "Feathertop Rhodes grass",
  "Fleabane",
  "Indian hedge mustard",
  "Liverseed grass",
  "Phalaris",
  "Ryegrass",
  "Sowthistle",
  "Wild oats",
  "Wild radis",
  "Wild turnip",
  "Windmill grass"
)

Weeds <- toupper(Weeds)

AEZ <- c ("NSW Central",
              "NSW NE Qld SE",
              "NSW NW Qld SW",
              "NSW Vic Slopes",
              "Qld Central",
              "SA Mid North Lower Yorke Eyre",
              "SA Vic Bordertown Wimmera",
              "SA Vic Mallee",
              "Tas Grain",
              "Vic High Rainfall",
              "WA Central",
              "WA Eastern",
              "WA Northern",
              "WA Sandplain")


template <- expand.grid(AEZ, crop_type, Weeds)
#name the clms
template <- template %>% 
  rename(AEZ =Var1,
         crop_grouping =Var2,
         Species =Var3)

rm(AEZ, crop_type, Weeds)
##############################################################################
template <- as.data.frame(template)

str(template)
str(NON_SELECTIVE)
str(SELECTIVE)


ALL_WEEDS <- left_join(template, NON_SELECTIVE)
str(NON_SELECTIVE_ALL_WEEDS)

ALL_WEEDS <- ALL_WEEDS %>% 
  select(AEZ, crop_grouping, Species, percenatge_weed_seed_HR) %>% 
  rename(perc_seed_HR_NON_SELECTIVE = percenatge_weed_seed_HR)

ALL_WEEDS <- left_join(ALL_WEEDS, SELECTIVE)

ALL_WEEDS <- ALL_WEEDS %>% 
  select(AEZ, crop_grouping, Species, perc_seed_HR_NON_SELECTIVE,percenatge_weed_seed_HR) %>% 
  rename(perc_seed_HR_SELECTIVE = percenatge_weed_seed_HR)
str(ALL_WEEDS)

## DO BOTH NOW
ALL_WEEDS <- left_join(ALL_WEEDS, BOTH)
str(ALL_WEEDS)
ALL_WEEDS <- ALL_WEEDS %>% 
  select(AEZ, crop_grouping, Species, 
         perc_seed_HR_NON_SELECTIVE,
         perc_seed_HR_SELECTIVE, 
         percenatge_weed_seed_HR) %>% 
  rename(perc_seed_HR_BOTH = percenatge_weed_seed_HR)
str(ALL_WEEDS)


write.csv(ALL_WEEDS, "W:/Economic impact of weeds round 2/HR/Jackie_working/HR_weeds/HR_Weed_HR_Class_Crop_type_weed_prec.csv")
