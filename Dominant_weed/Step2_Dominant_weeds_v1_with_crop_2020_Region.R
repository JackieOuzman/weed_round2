
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)


##----------------------------------------------------------------------------##
#### Bring in the data ####
##----------------------------------------------------------------------------##

all_states_with_data_long <- read.csv(file =  "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/all_states_with_data_long_step1.csv")
#all_states_with_data_long <- read.csv(file ="C:/Users/ouz001/working_from_home_post_Sep2022/weed_round2_offline/all_states_with_data_long_step1.csv")

##----------------------------------------------------------------------------##
#### Remove paddock not sampled #### 
##----------------------------------------------------------------------------##
# note - the data has been updated it now contains no data when the paddock was not sampled

str(all_states_with_data_long)
unique(all_states_with_data_long$crop_grouping)
unique(all_states_with_data_long$Crop) # "No data"  

# remove all the rows that the paddocks were not sampled coded by JB as No data
all_states_with_data_long_remove_no_data <- all_states_with_data_long %>% filter(Crop != "No data") %>% 
  filter(Crop != "0")
unique(all_states_with_data_long_remove_no_data$Crop) %>% sort()

##----------------------------------------------------------------------------##
#### only keep crop data ####

all_states_with_data_long_remove_no_data <- all_states_with_data_long_remove_no_data %>% filter(Crop != "No data") %>% 
  filter(Crop != "Fallow")


##----------------------------------------------------------------------------##
#### How many paddocks per zone region ####

colnames(all_states_with_data_long_remove_no_data)

all_states_with_data_long_remove_no_data <- all_states_with_data_long_remove_no_data %>% 
  mutate(
    region = case_when(
      AEZ == "NSW NE / Qld SE" ~ "Northern",
      AEZ == "NSW NW / Qld SW" ~ "Northern",
      AEZ == "Qld Central" ~ "Northern",
      
      AEZ == "NSW Central" ~ "Northern",
      AEZ == "NSW Vic Slopes" ~ "Northern",
      
      AEZ == "SA Mid N Lower YP EP" ~ "Southern",
      AEZ == "SA Vic Mallee" ~ "Southern",
      AEZ == "SA Vic Bordertown Wimmera" ~ "Southern",
      
      AEZ == "WA Northern"  ~ "Western",
      AEZ == "WA Central"  ~ "Western",
      AEZ == "WA Eastern"  ~ "Western",
      AEZ == "WA Sandplain"  ~ "Western"))


Unique_paddock_ID <- all_states_with_data_long_remove_no_data %>% 
  mutate(sample_AEz =   paste0(Sample,"_", region))

Unique_paddock_ID <- Unique_paddock_ID %>% distinct(sample_AEz, .keep_all = TRUE)

paddock_per_AEZ <- Unique_paddock_ID %>%  count(Sample, region) #
paddock_per_AEZ

paddock_per_AEZ_summary <- paddock_per_AEZ %>% 
  group_by(region) %>% 
  count(region)
paddock_per_AEZ_summary <- ungroup(paddock_per_AEZ_summary)
str(paddock_per_AEZ_summary)
paddock_per_AEZ_summary <- paddock_per_AEZ_summary %>%  rename(`count of paddocks` = n)

paddock_per_AEZ_summary



##----------------------------------------------------------------------------##
#### count the number of weed spec per region #### 
str(all_states_with_data_long_remove_no_data)
unique(all_states_with_data_long_remove_no_data$AEZ)


all_states_with_data_long_remove_no_data <- all_states_with_data_long_remove_no_data %>% 
  mutate(
    region = case_when(
      AEZ == "NSW NE / Qld SE" ~ "Northern",
      AEZ == "NSW NW / Qld SW" ~ "Northern",
      AEZ == "Qld Central" ~ "Northern",
      
      AEZ == "NSW Central" ~ "Northern",
      AEZ == "NSW Vic Slopes" ~ "Northern",
      
      AEZ == "SA Mid N Lower YP EP" ~ "Southern",
      AEZ == "SA Vic Mallee" ~ "Southern",
      AEZ == "SA Vic Bordertown Wimmera" ~ "Southern",
      
      AEZ == "WA Northern"  ~ "Western",
      AEZ == "WA Central"  ~ "Western",
      AEZ == "WA Eastern"  ~ "Western",
      AEZ == "WA Sandplain"  ~ "Western"))


all_states_with_data_long_remove_no_weeds <- all_states_with_data_long_remove_no_data %>% 
  filter(!is.na(weed_class)) 

AEZ_weeds_count <- all_states_with_data_long_remove_no_weeds %>% 
  count(region, weed, sort = TRUE)    
AEZ_weeds_count <- AEZ_weeds_count %>%  arrange(region, weed)
str(AEZ_weeds_count) 
AEZ_weeds_count <- AEZ_weeds_count %>%  rename(count = n)
AEZ_weeds_count

#count the number of paddocks sampled per AEZ (from above)
paddock_per_AEZ_summary

##----------------------------------------------------------------------------##
#### Join data - numb paddocks tested + count of weeds spec per AEZ ####

AEZ_weeds <- left_join(AEZ_weeds_count,paddock_per_AEZ_summary)

##----------------------------------------------------------------------------##
#### Cal percent occurrence #### 

str(AEZ_weeds)
AEZ_weeds <- AEZ_weeds %>% mutate(percent_occurance = (count/`count of paddocks`)*100) 
AEZ_weeds$percent_occurance <- round(AEZ_weeds$percent_occurance, 2)

AEZ_weeds
rm( AEZ_weeds_count)#paddock_per_AEZ_year

##----------------------------------------------------------------------------##
#### Rank percent occurance per AEZ ####
#note - ranks without averaging the first occurrence wins

str(AEZ_weeds)

AEZ_weeds <- AEZ_weeds %>% arrange(region) %>%
  group_by(region) %>%
  mutate(rank = rank(-percent_occurance, ties.method= "first"))



##----------------------------------------------------------------------------##
#### keep the top 4 weeds in each AEZ and year ####

top4weeds <- AEZ_weeds %>% 
  group_by(region) %>%
  top_n(-10, rank)

str(top4weeds)
top4weeds <- ungroup(top4weeds)

# subset clms
top4weeds <- top4weeds %>%  select(region, weed, percent_occurance, rank)


top4weeds$percent_occurance <- round(top4weeds$percent_occurance, 0)
top4weeds <- top4weeds %>% arrange(region, rank)

# save output 
# this is top 4 weeds in crops (no fallow) per AEZ
write.csv(top4weeds, 
          "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/top10weeds_2020REGION.csv")



##----------------------------------------------------------------------------##
#### ADD THE DENSITY DATA ####

# just keep the top 4 weeds (this was old code for 2 top weeds)
rank1_2_3_4 <- top4weeds %>% filter(rank == 1 | rank==2 | rank==3 | rank==4) 

# join the rank data with data that contains the density of weeds
rank <- left_join(all_states_with_data_long_remove_no_weeds, rank1_2_3_4)

## remove all the rows with missing weeds
rank <- rank %>% filter(!is.na(rank))
str(rank)
unique(rank$weed_class)

# format the weed desnisty data - called weed class
rank$weed_class <- as.factor(rank$weed_class)
levels(rank$weed_class)
rank$weed_class <- factor(rank$weed_class, levels=c("VL", "L", "M", "H", "VH"))




##----------------------------------------------------------------------------##
#### Cal the mode value for weed density ####

## note weed density information is in weed class clm, 
## The weed class code in the same data but coded with numbers
 
str(rank)

rank$weed_class_code <- as.double(rank$weed_class_code)
rank <- rank %>% filter( !is.na(weed_class_code))


#I need a function to cal mode!
mode <- function(codes){
  which.max(tabulate(codes))
}

# group data by weed AEZ and rank then cal the mode of weed density
rank_AEZ_weed_densities <- rank %>% 
  group_by(weed, AEZ, rank) %>% 
  summarise(
    mode =mode(weed_class_code),
    median = median(weed_class_code, na.rm = TRUE)
  ) %>% 
  arrange(AEZ, rank )


##----------------------------------------------------------------------------##
#### Join mode data t ####

str(rank_AEZ_weed_densities)
rank_AEZ_weed_densities <- ungroup(rank_AEZ_weed_densities)

### add the mode and median back into the rank data
rank_density_mode <- left_join(rank, rank_AEZ_weed_densities)

rank_density_mode$rank <- as.double(rank_density_mode$rank)

rank_density_mode %>% distinct(AEZ, rank) %>% arrange(AEZ, rank)

# rm(AEZ_weeds, label_weed1, label_weed2,rank, rank_density_mode_display_weed1, rank_density_mode_display_weed2)


##----------------------------------------------------------------------------##
### Final list of weeds ####

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


write.csv(Final_list_AEZ, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/Final_rank1_2_3_4_weed_AEZ_2020.csv")

rm(rank_density_mode, rank1_2_3_4)



