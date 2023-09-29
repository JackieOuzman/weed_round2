
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

all_states_with_data_long <- read.csv(file =  "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/all_states_with_data_long_step1.csv")
#all_states_with_data_long <- read.csv(file ="C:/Users/ouz001/working_from_home_post_Sep2022/weed_round2_offline/all_states_with_data_long_step1.csv")
#####################################################################################

## remove all the rows with missing weeds
all_states_with_data_long_remove_na <- all_states_with_data_long %>% filter(!is.na(weed_class))



################################################################################
## how many paddocks per zone

colnames(all_states_with_data_long_remove_na)
paddock_per_AEZ <- all_states_with_data_long %>%  count(Sample, AEZ) #
paddock_per_AEZ

paddock_per_AEZ_summary <- paddock_per_AEZ %>% 
  group_by(AEZ) %>% 
  count(AEZ)
paddock_per_AEZ_summary <- ungroup(paddock_per_AEZ_summary)
str(paddock_per_AEZ_summary)
paddock_per_AEZ_summary <- paddock_per_AEZ_summary %>%  rename(`count of paddocks` = n)

paddock_per_AEZ_summary



################################################################################
#count the number of weed occurrence per AEZ, weed and year  
AEZ_weeds_count <- all_states_with_data_long_remove_na %>% count(AEZ, weed, sort = TRUE)    
AEZ_weeds_count <- AEZ_weeds_count %>%  arrange(AEZ, weed)
str(AEZ_weeds_count) 
AEZ_weeds_count <- AEZ_weeds_count %>%  rename(count = n)
AEZ_weeds_count

#count the number of paddocks sampled per AEZ (from above)
paddock_per_AEZ_summary

################################################################################
#### join the summary data together
AEZ_weeds <- left_join(AEZ_weeds_count,paddock_per_AEZ_summary)
## add a percent 

str(AEZ_weeds)
AEZ_weeds <- AEZ_weeds %>% mutate(percent_occurance = (count/`count of paddocks`)*100) 
AEZ_weeds$percent_occurance <- round(AEZ_weeds$percent_occurance, 2)

AEZ_weeds
rm( AEZ_weeds_count)#paddock_per_AEZ_year

################################################################################
## add a rank ## ranks without averaging # first occurrence wins
str(AEZ_weeds)

AEZ_weeds <- AEZ_weeds %>% arrange(AEZ) %>%
  group_by(AEZ) %>%
  mutate(rank = rank(-percent_occurance, ties.method= "first"))



################################################################################
# keep the top 4 weeds in each AEZ and year

top4weeds <- AEZ_weeds %>% 
  group_by(AEZ) %>%
  top_n(-4, rank)

str(top4weeds)
top4weeds <- ungroup(top4weeds)

top4weeds <- top4weeds %>%  select(AEZ, weed, percent_occurance, rank)


top4weeds$percent_occurance <- round(top4weeds$percent_occurance, 0)
top4weeds <- top4weeds %>% arrange(AEZ, rank)


write.csv(top4weeds, 
          "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/top4weeds_2020.csv")



################################################################################
#Addd ranking to the long list of weeds

rank1_2 <- top4weeds %>% filter(rank == 1 | rank==2) 
rank <- left_join(all_states_with_data_long_remove_na, rank1_2)
## remove all the rows with missing weeds
rank <- rank %>% filter(!is.na(rank))
str(rank)
unique(rank$weed_class)

rank$weed_class <- as.factor(rank$weed_class)
levels(rank$weed_class)
rank$weed_class <- factor(rank$weed_class, levels=c("VL", "L", "M", "H", "VH"))




##################################################################################

###summaries the weed class   
str(rank)

rank$weed_class_code <- as.double(rank$weed_class_code)
rank <- rank %>% filter( !is.na(weed_class_code))


#I need a function to cal mode!
mode <- function(codes){
  which.max(tabulate(codes))
}

rank_AEZ_weed_densities <- rank %>% 
  group_by(weed, AEZ, rank) %>% 
  summarise(
    mode =mode(weed_class_code),
    median = median(weed_class_code, na.rm = TRUE)
  ) %>% 
  arrange(AEZ, rank )


################################################################################
str(rank_AEZ_weed_densities)
rank_AEZ_weed_densities <- ungroup(rank_AEZ_weed_densities)

### add the mode and median back into the rank data 
rank_density_mode <- left_join(rank, rank_AEZ_weed_densities)

rank_density_mode_display_weed1 <- rank_density_mode %>% 
  filter(rank == 1) %>% 
  distinct(AEZ,  .keep_all = TRUE) %>% select(AEZ, mode)
rank_density_mode_display_weed2 <- rank_density_mode %>% 
  filter(rank == 2) %>% 
  distinct(AEZ,.keep_all = TRUE) %>% select(AEZ, mode)

### recode mode.

## recode weed class with an number
rank_density_mode_display_weed1 <-rank_density_mode_display_weed1 %>% 
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

rank_density_mode_display_weed2 <-rank_density_mode_display_weed2 %>% 
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








rm(AEZ_weeds, label_weed1, label_weed2,rank, rank_density_mode_display_weed1, rank_density_mode_display_weed2)


################################################################################
### Final list of weeds 

str(rank_density_mode)

Final_list_AEZ <-      rank_density_mode %>% 
  distinct(rank, AEZ,  .keep_all = TRUE) %>% 
  select(AEZ, rank, weed, percent_occurance, mode ) %>% 
  arrange(AEZ,  rank)

Final_list_AEZ <-Final_list_AEZ %>% 
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


write.csv(Final_list_AEZ, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/Final_Rank1_2_weed_AEZ_2020.csv")

rm(rank_density_mode, rank1_2)



