
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
"W:\Economic impact of weeds round 2\HR\raw_data\HR Survey database All no lat long.xlsx"

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
unique(HR_weed$Gp_9)
HR_weed <- HR_weed %>% 
  mutate(Non_Selective_Herb = case_when(
         Gp_9 == "DR"~ "RESIST",
         Gp_9 == "RESIST"~ "RESIST",
         TRUE ~ "OTHER"
         ))


## create a new columns that are HR weed to selective group 1, 2, 3, 4, 5 ,12 etc..
names(HR_weed)
HR_weed <- HR_weed %>% 
  mutate(Selective_Herb = case_when(
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


## create a new columns that are HR weed to selective and non selective
names(HR_weed)
unique(HR_weed$Non_Selective_Herb)
unique(HR_weed$Selective_Herb)

HR_weed <- HR_weed %>% 
  mutate(Selective_and_non_Selective = case_when(
    Non_Selective_Herb ==  "RESIST"& Selective_Herb ==  "RESIST"  ~ "RESIST",
    
    TRUE ~ "OTHER"
  ))



###############################################################################
names(HR_weed)

HR_weed <- HR_weed %>% select(ID_Jaxs:`Weed density`,Non_Selective_Herb:Selective_and_non_Selective)

unique(HR_weed$`Weed density`)


## recode weed class with an number
HR_weed <-HR_weed %>% 
  mutate(
    weed_class_code = case_when(
      `Weed density` =="V Low" ~ "1",
      `Weed density` =="Low" ~ "2",
      `Weed density` =="Medium" ~ "3",
      `Weed density` =="High" ~ "4",
      `Weed density` =="V High" ~ "5",
      `Weed density` =="4" ~ "4",
      `Weed density` == "N" | `Weed density` == "na" | `Weed density` == "Yes" | `Weed density` == "Fence" |
      `Weed density` == "Unknown" | `Weed density` == "0"      ~ NA_character_,
      TRUE ~ `Weed density`
    )
  )



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
       Crop == "Canola"|
      Crop == "wheta" ~ "Cereals",
     
     
       Crop == "Chickpeas" |
       Crop == "Chickpea" |
       Crop == "Freezer Peas" |
       Crop == "Lupin" |
       Crop == "Field pea" |
       
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



################################################################################
### make a list of weeds per zone and crop
str(HR_weed)


################################################################################
## how many samples tested per zone


samples_per_AEZ_crop_group <- HR_weed %>%  
  filter(crop_grouping != "other") %>% 
  count(AEZ, crop_grouping) #

samples_per_AEZ_crop_group <- samples_per_AEZ_crop_group %>%  rename(`count of samples tested` = n)

samples_per_AEZ_crop_group



################################################################################
#count the number of HR weed occurrence per AEZ, weed   
names(HR_weed)

AEZ_crop_Non_selective_HR_weeds_count <- HR_weed %>% count(AEZ, Non_Selective_Herb, crop_grouping, sort = TRUE)    
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

  


 







