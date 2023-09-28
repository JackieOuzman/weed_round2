
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



#####################################################################################

## remove all the rows with missing weeds
all_states_with_data_long_remove_na <- all_states_with_data_long %>% filter(!is.na(weed_class))



### make a list of weeds per zone and crop
str(HR_weed_list_long_remove_na)

list_of_weed_AEZ_crop_grouping <- HR_weed_list_long_remove_na %>% 
  group_by(AEZ, crop_grouping) %>% 
  distinct(weed, .keep_all = TRUE) %>% 
  select(AEZ, weed, crop_grouping)

list_of_weed_AEZ_crop_grouping <- ungroup(list_of_weed_AEZ_crop_grouping)
str(list_of_weed_AEZ_crop_grouping)

write.csv(list_of_weed_AEZ_crop_grouping, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/list_of_weed_AEZ_crop_grouping.csv", row.names = FALSE)

################################################################################
## how many paddocks per zone
str(HR_weed_list_long_remove_na)

paddock_per_AEZ_year_crop_group <- HR_weed_list_long %>%  count(ID_Jaxs, AEZ, Year, crop_grouping) #


paddock_per_AEZ_year_crop_group <- paddock_per_AEZ_year_crop_group %>% 
  group_by(AEZ, Year, crop_grouping) %>% 
  count(AEZ)
paddock_per_AEZ_year_crop_group <- ungroup(paddock_per_AEZ_year_crop_group)
str(paddock_per_AEZ_year_crop_group)
paddock_per_AEZ_year_crop_group <- paddock_per_AEZ_year_crop_group %>%  rename(`count of paddocks` = n)

paddock_per_AEZ_year_crop_group



################################################################################
#count the number of weed occurrence per AEZ, weed and year  
AEZ_crop_weeds_count <- HR_weed_list_long_remove_na %>% count(AEZ, weed, Year,crop_grouping, sort = TRUE)    
AEZ_crop_weeds_count <- AEZ_crop_weeds_count %>%  arrange(AEZ, weed, Year)
str(AEZ_crop_weeds_count) 
AEZ_crop_weeds_count <- AEZ_crop_weeds_count %>%  rename(count = n)

#count the number of paddocks sampled per AEZ and year (from above)
paddock_per_AEZ_year_crop_group

################################################################################
#### join the summary data together
AEZ_crop_weeds <- left_join(AEZ_crop_weeds_count,paddock_per_AEZ_year_crop_group)
## add a percent 

str(AEZ_crop_weeds)
AEZ_crop_weeds <- AEZ_crop_weeds %>% mutate(percent_occurance = (count/`count of paddocks`)*100) 
AEZ_crop_weeds$percent_occurance <- round(AEZ_crop_weeds$percent_occurance, 2)

AEZ_crop_weeds
#rm( AEZ_weeds_count)#paddock_per_AEZ_year

################################################################################
## add a rank ## ranks without averaging # first occurrence wins
str(AEZ_crop_weeds)

AEZ_crop_weeds <- AEZ_crop_weeds %>% arrange(AEZ, Year, crop_grouping) %>%
  group_by(AEZ, Year) %>%
  mutate(rank = rank(-percent_occurance, ties.method= "first"))



################################################################################
# keep the top 4 weeds in each AEZ and year

top4weeds_crop <- AEZ_crop_weeds %>% 
  group_by(AEZ, Year, crop_grouping) %>%
  top_n(-4, rank)

str(top4weeds_crop)
top4weeds_crop <- ungroup(top4weeds_crop)

top4weeds_crop <- top4weeds_crop %>%  select(AEZ, Year, weed, crop_grouping, percent_occurance, rank)


top4weeds_crop$percent_occurance <- round(top4weeds_crop$percent_occurance, 0)
top4weeds_crop <- top4weeds_crop %>% arrange(AEZ, crop_grouping, Year, rank)


write.csv(top4weeds_crop, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/top4weeds_crop_group.csv")



################################################################################
#Addd ranking to the long list of weeds

rank1_2_crop <- top4weeds_crop %>% filter(rank == 1 | rank==2) 
rank_crop <- left_join(HR_weed_list_long_remove_na, rank1_2_crop)
## remove all the rows with missing weeds
rank_crop <- rank_crop %>% filter(!is.na(rank))
str(rank_crop)
unique(rank_crop$weed_class)

rank_crop$weed_class <- as.factor(rank_crop$weed_class)
levels(rank_crop$weed_class)
rank_crop$weed_class <- factor(rank_crop$weed_class, levels=c("VL", "L", "M", "H", "VH"))

label_weed1 <- rank_crop %>% 
  filter(rank == 1) %>% 
  distinct(Year,AEZ, crop_grouping, .keep_all = TRUE)

label_weed2 <- rank %>% 
  filter(rank == 2) %>% 
  distinct(Year,AEZ, crop_grouping, .keep_all = TRUE)

rm(top4weeds_crop)


##################################################################################

###summaries the weed class   
str(rank_crop)

rank_crop$weed_class_code <- as.double(rank_crop$weed_class_code)
rank_crop <- rank_crop %>% filter( !is.na(weed_class_code))


#I need a function to cal mode!
mode <- function(codes){
  which.max(tabulate(codes))
}

rank_crop_AEZ_weed_densities <- rank_crop %>% 
  group_by(weed, crop_grouping, AEZ, Year, rank) %>% 
  summarise(
    mode =mode(weed_class_code),
    median = median(weed_class_code, na.rm = TRUE)
  ) %>% 
  arrange(AEZ, rank, crop_grouping, Year )


################################################################################
str(rank_crop_AEZ_weed_densities)
rank_crop_AEZ_weed_densities <- ungroup(rank_crop_AEZ_weed_densities)

### add the mode and median back into the rank data 
rank_crop_density_mode <- left_join(rank_crop, rank_crop_AEZ_weed_densities)

rank_crop_density_mode_display_weed1 <- rank_crop_density_mode %>% 
  filter(rank == 1) %>% 
  distinct(Year,AEZ, crop_grouping, .keep_all = TRUE) %>% select(Year,AEZ, mode)
rank_crop_density_mode_display_weed2 <- rank_crop_density_mode %>% 
  filter(rank == 2) %>% 
  distinct(Year,AEZ, crop_grouping,.keep_all = TRUE) %>% select(Year,AEZ, mode)

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








rm(AEZ_crop_weeds, label_weed1, label_weed2,rank, rank_crop_density_mode_display_weed1, rank_crop_density_mode_display_weed2)


################################################################################
### Final list of weeds 

str(rank_crop_density_mode)

Final_list_AEZ_crop_Year <-      rank_crop_density_mode %>% 
  distinct(Year, AEZ, crop_grouping, .keep_all = TRUE) %>% 
  select(Year, AEZ, crop_grouping, rank, weed, percent_occurance, mode ) %>% 
  arrange(AEZ, Year, crop_grouping, rank)

Final_list_AEZ_crop_Year <-Final_list_AEZ_crop_Year %>% 
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


write.csv(Final_list_AEZ_crop_Year, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/Final_Rank1_2_weed_AEZ_year_CROP.csv")

rm(rank_density_mode, rank1_2)







################################################################################
################################################################################
## All years to get weed list ranking ###
################################################################################
################################################################################
rm(list=ls()[! ls() %in% c("HR_weed_list_long","HR_weed_list_long_remove_na")])

str(HR_weed_list_long)
str(HR_weed_list_long_remove_na)

### make a new sample ID that includes the year.

HR_weed_list_long <- HR_weed_list_long %>% mutate(Sampl_ID_Yr = paste0(Sample,"_", Year))
HR_weed_list_long_remove_na <- HR_weed_list_long_remove_na %>% mutate(Sampl_ID_Yr = paste0(Sample,"_", Year))

paddock_per_AEZ_crop <- HR_weed_list_long %>%  count(ID_Jaxs, AEZ, crop_grouping) %>%  select (ID_Jaxs, AEZ, crop_grouping) #This is just a list of paddock
paddock_per_AEZ_1_crop <- paddock_per_AEZ_crop %>% count(AEZ, crop_grouping)

paddock_per_AEZ_1_crop <-paddock_per_AEZ_1_crop %>%  rename(`count of paddocks` = n)
################################################################################
#count the number of weed occurrence per AEZ, weed   
AEZ_weeds_count_crop <- HR_weed_list_long_remove_na %>% count(AEZ, crop_grouping, weed, sort = TRUE)    
AEZ_weeds_count_crop <- AEZ_weeds_count_crop %>%  arrange(AEZ, crop_grouping, weed)
str(AEZ_weeds_count_crop) 
AEZ_weeds_count_crop <- AEZ_weeds_count_crop %>%  rename(count = n)

#count the number of paddocks sampled per AEZ (from above)
paddock_per_AEZ_1_crop

################################################################################
#### join the summary data together
AEZ_weeds_crop <- left_join(AEZ_weeds_count_crop,paddock_per_AEZ_1_crop)
## add a percent 

str(AEZ_weeds_crop)
AEZ_weeds_crop <- AEZ_weeds_crop %>% mutate(percent_occurance = (count/`count of paddocks`)*100) 
AEZ_weeds_crop$percent_occurance <- round(AEZ_weeds_crop$percent_occurance, 2)

AEZ_weeds_crop
rm( AEZ_weeds_count_crop)#paddock_per_AEZ_year

################################################################################
## add a rank ## ranks without averaging # first occurrence wins
str(AEZ_weeds_crop)

AEZ_weeds_crop <- AEZ_weeds_crop %>% arrange(AEZ, crop_grouping) %>%
  group_by(AEZ, crop_grouping) %>%
  mutate(rank = rank(-percent_occurance, ties.method= "first"))



################################################################################
# keep the top 4 weeds in each AEZ and year

top4weeds_crop <- AEZ_weeds_crop %>% 
  group_by(AEZ, crop_grouping) %>%
  top_n(-4, rank)

str(top4weeds_crop)
top4weeds_crop <- ungroup(top4weeds_crop)

top4weeds_crop <- top4weeds_crop %>%  select(AEZ, crop_grouping,weed, percent_occurance, rank)


top4weeds_crop$percent_occurance <- round(top4weeds_crop$percent_occurance, 0)
top4weeds_crop <- top4weeds_crop %>% arrange(AEZ, crop_grouping, rank)


write.csv(top4weeds_crop, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/top4weeds_noYear_CROP.csv")



################################################################################
#Addd ranking to the long list of weeds

rank1_2_crop <- top4weeds_crop %>% filter(rank == 1 | rank==2) 
rank_crop <- left_join(HR_weed_list_long_remove_na, rank1_2_crop)
## remove all the rows with missing weeds
rank_crop <- rank_crop %>% filter(!is.na(rank))
str(rank_crop)
unique(rank_crop$weed_class)



###summaries the weed class   
str(rank_crop)

rank_crop$weed_class_code <- as.double(rank_crop$weed_class_code)
#I need a function to cal mode!
mode <- function(codes){
  which.max(tabulate(codes))
}

rank_AEZ_weed_densities_crop <- rank_crop %>% 
  group_by(weed, crop_grouping,AEZ, rank) %>% 
  summarise(
    mode =mode(weed_class_code),
    median = median(weed_class_code, na.rm = TRUE)
  ) %>% 
  arrange(AEZ, crop_grouping,rank )


################################################################################
str(rank_AEZ_weed_densities_crop)
rank_AEZ_weed_densities_crop <- ungroup(rank_AEZ_weed_densities_crop)

### add the mode and median back into the rank data 
rank_density_mode_crop <- left_join(rank_crop, rank_AEZ_weed_densities_crop)



rank_density_mode_display_weed1_crop <- rank_density_mode_crop %>% 
  filter(rank == 1) %>% 
  distinct(Year,AEZ, crop_grouping, .keep_all = TRUE) %>% select(AEZ, crop_grouping, mode)
rank_density_mode_display_weed2_crop <- rank_density_mode_crop %>% 
  filter(rank == 2) %>% 
  distinct(Year,AEZ,crop_grouping, .keep_all = TRUE) %>% select(AEZ, crop_grouping,mode)

### recode mode.

## recode weed class with an number
rank_density_mode_display_weed1_crop <-rank_density_mode_display_weed1_crop %>% 
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

rank_density_mode_display_weed2_crop <-rank_density_mode_display_weed2_crop %>% 
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


################################################################################
### Final list of weeds 

str(rank_density_mode_crop)

Final_list_AEZ_crop <-      rank_density_mode_crop %>% 
  distinct(AEZ, rank, crop_grouping, .keep_all = TRUE) %>% 
  select(AEZ, rank,crop_grouping, weed, percent_occurance, mode ) %>% 
  arrange(AEZ, crop_grouping, rank)

Final_list_AEZ_crop <-Final_list_AEZ_crop %>% 
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


write.csv(Final_list_AEZ_crop, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/Final_Rank1_2_weed_AEZ_CROP.csv")






