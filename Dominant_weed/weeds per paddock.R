






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

#all_states_with_data_long <- read.csv(file =  "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/all_states_with_data_long_step1.csv")
all_states_with_data_long <- read.csv(file ="C:/Users/ouz001/working_from_home_post_Sep2022/weed_round2_offline/all_states_with_data_long_step1.csv")
#####################################################################################

## remove all the rows with missing weeds
all_states_with_data_long_remove_na <- all_states_with_data_long %>% filter(!is.na(weed_class))




###############################################################################
### How many weeds per paddock per year

str(all_states_with_data_long)
str(all_states_with_data_long_remove_na)

Number_weeds_per_paddock_na <- all_states_with_data_long_remove_na %>% 
  group_by(Sample) %>% 
  summarise(n = n())
Number_weeds_per_paddock_na <- ungroup(Number_weeds_per_paddock_na)

## what about the paddocks with no weeds?

list_of_paddock_sample_ID <- all_states_with_data_long %>% distinct(Sample) %>% mutate(check="1")
str(list_of_paddock_sample_ID)
str(Number_weeds_per_paddock_na)

Number_weeds_per_paddock <- left_join(list_of_paddock_sample_ID, Number_weeds_per_paddock_na)
str(Number_weeds_per_paddock)

### modify the clms so that the when there are no weeds I have zero value!

Number_weeds_per_paddock <- Number_weeds_per_paddock %>% 
  mutate(n = ifelse(is.na(n), 0, n))

Number_weeds_per_paddock <-Number_weeds_per_paddock %>% rename(count_weeds = n )
Number_weeds_per_paddock$count_weeds <- as.factor(Number_weeds_per_paddock$count_weeds)

ggplot(data = Number_weeds_per_paddock, aes(x = count_weeds)) +
  geom_histogram(aes(
    x = count_weeds,
    y = (..count.. / sum(..count..)) * 100),
    
    stat = 'count',
    show.legend = FALSE) +
  scale_x_discrete(drop = FALSE) +
  labs(x = 'Number of weed species per paddock', 
       y = 'Percent', 
       title = "Percentage of fields containing multiple weed species",
       subtitle = "2020 data supplied Sep 2023 mising some AEZ") +
  
  theme(panel.grid.minor = element_blank()) 
  


ggsave(
  device = "png",
  filename = "Percentage of fields containing multiple weed species 2020_data.png",
  path= "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/",
  width=9.5,
  height = 6.28,
  dpi=600
) 


################################################################################
###############################################################################
### How many weeds per paddock per AEZ

str(all_states_with_data_long)
str(all_states_with_data_long_remove_na)

Number_weeds_per_paddock_na_AEZ <- all_states_with_data_long_remove_na %>% 
  group_by(Sample, AEZ) %>% 
  summarise(n = n())
Number_weeds_per_paddock_na_AEZ <- ungroup(Number_weeds_per_paddock_na_AEZ)

## what about the paddocks with no weeds?

list_of_paddock_sample_ID_AEZ <- all_states_with_data_long %>% distinct(Sample, AEZ) %>% mutate(check="1")
str(list_of_paddock_sample_ID_AEZ)
str(Number_weeds_per_paddock_na_AEZ)

Number_weeds_per_paddock_AEZ <- left_join(list_of_paddock_sample_ID_AEZ, Number_weeds_per_paddock_na_AEZ)
str(Number_weeds_per_paddock_AEZ)

### modify the clms so that the when there are no weeds I have zero value!

Number_weeds_per_paddock_AEZ <- Number_weeds_per_paddock_AEZ %>% 
  mutate(n = ifelse(is.na(n), 0, n))

Number_weeds_per_paddock_AEZ <-Number_weeds_per_paddock_AEZ %>% rename(count_weeds = n )
Number_weeds_per_paddock_AEZ$count_weeds <- as.factor(Number_weeds_per_paddock_AEZ$count_weeds)

ggplot(data = Number_weeds_per_paddock_AEZ, aes(x = count_weeds)) +
  geom_histogram(aes(
    x = count_weeds,
    y = (..count.. / sum(..count..)) * 100),
    
    stat = 'count',
    show.legend = FALSE) +
  scale_x_discrete(drop = FALSE) +
  labs(x = 'Number of weed species per paddock', 
       y = 'Percent', 
       title = "Percentage of fields containing multiple weed species",
       subtitle = "2020 data supplied Sep 2023 mising some AEZ") +
  
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(.~AEZ)



ggsave(
  device = "png",
  filename = "Percentage of fields containing multiple weed species 2020_data_by_AEZ.png",
  path= "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/",
  width=9.5,
  height = 6.28,
  dpi=600
) 


################################################################################
# number of weeds ID per AEZ
str(all_states_with_data_long_remove_na)

ID_WEEDS_AEZ <- all_states_with_data_long_remove_na %>% 
  group_by(AEZ, weed) %>% 
  count(AEZ)



ID_WEEDS_AEZ_step1 <- all_states_with_data_long_remove_na %>%  count(weed, AEZ) #
ID_WEEDS_AEZ_step1

ID_WEEDS_AEZ_step2 <- ID_WEEDS_AEZ_step1 %>% 
  group_by(AEZ) %>% 
  count(AEZ)
ID_WEEDS_AEZ_step2
