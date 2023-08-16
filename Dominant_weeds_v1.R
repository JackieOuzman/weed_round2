
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)


################################################################################
###    Bring in the data ########
################################################################################


HR_weed_list <-  read_excel("W:/Economic impact of weeds round 2/HR/raw_data/Weed Species locations no co-ordinates.xlsx", 
                                                     sheet = "NSW")
## make it long
str(HR_weed_list)
HR_weed_list <- HR_weed_list%>%  dplyr::select(Sample:Wireweed)
str(HR_weed_list)
HR_weed_list <- HR_weed_list %>%  rename(AEZ = `GRDC AEZ`)

HR_weed_list_long <- pivot_longer(HR_weed_list,
                                  cols = c(`3 corner Jack`:`Wireweed`),
                                  names_to = "weed",
                                  values_to="weed_class"
                                  )

unique(HR_weed_list_long$weed_class)

## recode weed class with an number
HR_weed_list_long <-HR_weed_list_long %>% 
  mutate(
    weed_class_code = case_when(
      weed_class =="VL" ~ "1",
      weed_class =="L" ~ "2",
      weed_class =="M" ~ "3",
      weed_class =="H" ~ "4",
      weed_class =="VH" ~ "5",
      TRUE ~ weed_class
    )
  )


## remove all the rows with missing weeds
HR_weed_list_long_remove_na <- HR_weed_list_long %>% filter(!is.na(weed_class))

################################################################################
### make a list of weeds per zone
str(HR_weed_list_long_remove_na)

list_of_weed_AEZ <- HR_weed_list_long_remove_na %>% 
  group_by(AEZ) %>% 
  distinct(weed, .keep_all = TRUE) %>% 
  select(AEZ, weed)
  
list_of_weed_AEZ <- ungroup(list_of_weed_AEZ)
str(list_of_weed_AEZ)

write.csv(list_of_weed_AEZ, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/list_of_weed_AEZ.csv", row.names = FALSE)

################################################################################
## how many weeds per zone
list_of_weed_AEZ_count <- list_of_weed_AEZ %>% count(AEZ)
str(list_of_weed_AEZ_count)

list_of_weed_AEZ_count <- list_of_weed_AEZ_count %>% rename(`Number of weeds ID` = n)
write.csv(list_of_weed_AEZ_count, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/list_of_weed_AEZ_count.csv", row.names = FALSE)


################################################################################
## how many paddocks per zone
str(HR_weed_list_long_remove_na)
paddock_per_AEZ_year <- HR_weed_list_long_remove_na %>%  count(Sample, AEZ, Year) #nope this is not what I want

paddock_per_AEZ_year <- paddock_per_AEZ_year %>% 
  group_by(AEZ, Year) %>% 
  count(AEZ)
paddock_per_AEZ_year <- ungroup(paddock_per_AEZ_year)
str(paddock_per_AEZ_year)
paddock_per_AEZ_year <- paddock_per_AEZ_year %>%  rename(`count of paddocks` = n)

paddock_per_AEZ_year

################################################################################
#count the number of weed occurrence per AEZ, weed and year  
AEZ_weeds_count <- HR_weed_list_long_remove_na %>% count(AEZ, weed, Year, sort = TRUE)    
AEZ_weeds_count <- AEZ_weeds_count %>%  arrange(AEZ, weed, Year)
str(AEZ_weeds_count) 
AEZ_weeds_count <- AEZ_weeds_count %>%  rename(count = n)

#count the number of paddocks sampled per AEZ and year (from above)
paddock_per_AEZ_year

################################################################################
#### join the summary data together
AEZ_weeds <- left_join(AEZ_weeds_count,paddock_per_AEZ_year)
## add a percent 

str(AEZ_weeds)
AEZ_weeds <- AEZ_weeds %>% mutate(percent_occurance = (count/`count of paddocks`)*100) 
AEZ_weeds$percent_occurance <- round(AEZ_weeds$percent_occurance, 2)

AEZ_weeds

################################################################################
## add a rank ## ranks without averaging # first occurrence wins
str(AEZ_weeds)

AEZ_weeds <- AEZ_weeds %>% arrange(AEZ, Year) %>%
  group_by(AEZ, Year) %>%
  mutate(rank = rank(-percent_occurance, ties.method= "first"))

test <- AEZ_weeds %>% filter(Year == 2013) %>% 
  filter(AEZ == "NSW NE Qld SE") %>% 
  arrange(AEZ, Year) %>%
  mutate(rank = rank(-percent_occurance, ties.method= "first"))

################################################################################
# keep the top 4 weeds in each AEZ and year

top4weeds <- AEZ_weeds %>% 
  group_by(AEZ, Year) %>%
  top_n(-4, rank)

str(top4weeds)
top4weeds <- ungroup(top4weeds)

top4weeds <- top4weeds %>%  select(AEZ, Year, weed, percent_occurance, rank)


top4weeds$percent_occurance <- round(top4weeds$percent_occurance, 0)




top4weeds <- top4weeds %>% arrange(AEZ, Year, rank)






#### Up to here I have list of weed and rank and % of paddocks

### now add in the densities and number of species per paddock plots
## Think about how I can display this which is not a table?


































################################################################################
### higher level look - sum of weeds for each zone

## remove all the rows with missing weeds
HR_weed_list_long_remove_na <- HR_weed_list_long %>% filter(!is.na(weed_class))

#count the number of occurrence per AEZ, weed and year  
AEZ_weeds_count <- HR_weed_list_long_remove_na %>% count(AEZ, weed, Year, sort = TRUE)    
AEZ_weeds_count <- AEZ_weeds_count %>%  arrange(AEZ, weed, Year)
str(AEZ_weeds_count) 
AEZ_weeds_count <- AEZ_weeds_count %>%  rename(count = n)

#count the number of occurrence per AEZ and year
AEZ_weeds_tally <- HR_weed_list_long_remove_na %>% count(AEZ, Year, sort = TRUE) 
AEZ_weeds_tally <- AEZ_weeds_tally %>%  arrange(AEZ, Year)
AEZ_weeds_tally <- AEZ_weeds_tally %>%  rename(tally = n)


#### join the summary data together
AEZ_weeds <- left_join(AEZ_weeds_count,AEZ_weeds_tally)
## add a percent 
AEZ_weeds <- AEZ_weeds %>% mutate(percent_occurance = (count/tally)*100) 
AEZ_weeds$percent_occurance <- round(AEZ_weeds$percent_occurance, 2)

## add a rank
str(AEZ_weeds)

AEZ_weeds <- AEZ_weeds %>% arrange(AEZ, Year) %>%
  group_by(AEZ, Year) %>%
  mutate(rank = rank(percent_occurance))
 
#https://www.r-bloggers.com/2022/07/how-to-rank-by-group-in-r/
# This has some tip on dealing with ties - I have not implemented this yet.

AEZ_weed1 <- AEZ_weeds %>%
  group_by(AEZ, Year) %>%
  filter(percent_occurance == max(percent_occurance, na.rm=TRUE))
AEZ_weed1 <- ungroup(AEZ_weed1)

AEZ_weed1 <-AEZ_weed1 %>% rename (weed1 = weed,
                                  count_weed1 = count,
                                  perecent_occurance_weed1 = percent_occurance,
                                  rank_weed1 = rank) %>% 
  select (-tally)

### I am returning more than one because of ties
str(AEZ_weed1)
AEZ_weed1 <- AEZ_weed1 %>% mutate(tie_breaker = paste0(AEZ,"_", Year, "_", rank_weed1 )) 

AEZ_weed1 <- AEZ_weed1 %>% distinct(tie_breaker, .keep_all = TRUE) 
AEZ_weed1 <- AEZ_weed1 %>% select(-tie_breaker)

str(AEZ_weed1)

AEZ_weed2 <- AEZ_weeds %>%
  group_by(AEZ, Year) %>%
  filter(percent_occurance == max(percent_occurance[percent_occurance != max(percent_occurance)]))

AEZ_weed2 <- ungroup(AEZ_weed2)
str(AEZ_weed2)
AEZ_weed2 <-AEZ_weed2 %>% rename (weed2 = weed,
                                  count_weed2 = count,
                                  perecent_occurance_weed2 = percent_occurance,
                                  rank_weed2 = rank) %>% 
  select (-tally)




### I am returning more than one because of ties
str(AEZ_weed2)
AEZ_weed2 <- AEZ_weed2 %>% mutate(tie_breaker = paste0(AEZ,"_", Year, "_", rank_weed2 )) 
  
AEZ_weed2 <- AEZ_weed2 %>% distinct(tie_breaker, .keep_all = TRUE) 
AEZ_weed2 <- AEZ_weed2 %>% select(-tie_breaker)

#### weed 1 and 2 list

AEZ_weed1_2 <- left_join(AEZ_weed1, AEZ_weed2, by=c("AEZ" , "Year" )) 
str(AEZ_weed1_2)
AEZ_weed1_2 <- AEZ_weed1_2 %>% select(AEZ, Year, weed1, weed2, perecent_occurance_weed1, rank_weed1, perecent_occurance_weed2, rank_weed2)




AEZ_weeds <- ungroup(AEZ_weeds)
str(AEZ_weeds)
str(AEZ_weed1_2)

ggplot(data=AEZ_weeds, aes(as.factor(Year),perecent_occurance))+
  geom_point(aes(colour = weed), size = 4)+
  facet_wrap(.~AEZ)+
  geom_text(data=AEZ_weed1_2, aes(label=weed1,y=90,x=as.factor(Year)), angle = 90, size =2.0, colour= "black")+
  geom_text(data=AEZ_weed1_2, aes(label=weed2,y=60,x=as.factor(Year)), angle = 90, size =2.0, colour= "darkblue")+
  theme_bw()+ 
  labs(title = "Most common weed by AEZ and Year",
        subtitle = "Weed1 = black, weed2 = blue",
        caption = "Using: Weed Species locations no co-ordinates.xlsx",
        x = "Year",  
        y ="Percentage occurrence (weed / all weeds recorded in AEZ by year)")+
  theme(legend.key.size = unit(0.01, 'cm'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(
  device = "png",
  filename = "Common_weed_draft1.png",
  path= "W:/Economic impact of weeds round 2/HR/Jackie_working/",
  width=9.5,
  height = 6.28,
  dpi=600
) 




###############################################################################
### not sure I need the follow code






################################################################################
### sum of weeds for each zone



AEZ1 <- HR_weed_list_long %>% 
  filter(AEZ == "NSW NW Qld SW") %>% 
  filter(!is.na(weed_class)) %>% 
  dplyr::select(AEZ, Year, weed, weed_class, weed_class_code) %>% 
  arrange(weed,Year, weed_class ) 
str(AEZ1)  

AEZ1_weeds_count <-AEZ1 %>% count(weed, AEZ, Year, sort = TRUE)    
str(AEZ1_weeds_count) 

AEZ1_weeds_count <- AEZ1_weeds_count %>%  rename(count = n)

ggplot(data=AEZ1_weeds_count, aes(Year,count ))+
  #geom_point()+
  geom_point(aes(colour = weed), size = 4)


### what was the weed class?

AEZ1$weed_class_code <- as.double(AEZ1$weed_class_code)
#I need a function to cal mode!
mode <- function(codes){
  which.max(tabulate(codes))
}

AEZ1_weeds_code <- AEZ1 %>% 
  group_by(weed, AEZ, Year) %>% 
  summarise(
    mode =mode(weed_class_code),
    median = median(weed_class_code, na.rm = TRUE)
            )



       
