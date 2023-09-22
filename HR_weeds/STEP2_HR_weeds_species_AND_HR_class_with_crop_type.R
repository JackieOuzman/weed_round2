
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


###############################################################################

#How to join it all togther and not loose any data