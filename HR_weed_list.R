library(MESS)
library(tidyverse)
library(readxl)
library(dplyr)
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library("Hmisc")
library(ggplot2)

library(plotly)


library(hrbrthemes)
library(viridis)
library(forcats)
library(hrbrthemes)
library(stringr)
#install.packages("hrbrthemes")

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
      TRUE ~ weed_class
    )
  )

################################################################################
###    what have the top weeds for each AEZ ########
################################################################################
unique(HR_weed_list$AEZ)
str(HR_weed_list_long)


################################################################################
### make a list of weeds per zone

list_of_weed_AEZ1 <- HR_weed_list_long %>% 
  filter(AEZ == "NSW NW Qld SW") %>% 
  filter(!is.na(weed_class)) %>% 
  distinct(weed, .keep_all = TRUE) %>% 
  dplyr::select(AEZ, weed) %>% 
  arrange(weed )


################################################################################
### 37 weeds in zone 1 ### this is probably not what I want

#3 corner Jack


AEZ1_3corner_Jack <- HR_weed_list_long %>% 
  filter(AEZ == "NSW NW Qld SW") %>% 
  filter(weed  == "3 corner Jack") %>% 
  filter(!is.na(weed_class)) %>% 
  distinct(weed, .keep_all = TRUE) %>% 
  arrange(Year, weed_class )


AEZ1_African_turnip_weed <- HR_weed_list_long %>% 
  filter(AEZ == "NSW NW Qld SW") %>% 
  filter(weed  == "African turnip weed") %>% 
  filter(!is.na(weed_class)) %>% 
  distinct(weed, .keep_all = TRUE) %>% 
  arrange(Year, weed_class )


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
  mutate(rank = rank(perecent_occurance))
 
#https://www.r-bloggers.com/2022/07/how-to-rank-by-group-in-r/
# This has some tip on dealing with ties - I have not implemented this yet.

AEZ_weed1 <- AEZ_weeds %>%
  group_by(AEZ, Year) %>%
  filter(perecent_occurance == max(perecent_occurance, na.rm=TRUE))
AEZ_weed1 <- ungroup(AEZ_weed1)

AEZ_weed1 <-AEZ_weed1 %>% rename (weed1 = weed,
                                  count_weed1 = count,
                                  perecent_occurance_weed1 = perecent_occurance,
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
  filter(perecent_occurance == max(perecent_occurance[perecent_occurance != max(perecent_occurance)]))

AEZ_weed2 <- ungroup(AEZ_weed2)
str(AEZ_weed2)
AEZ_weed2 <-AEZ_weed2 %>% rename (weed2 = weed,
                                  count_weed2 = count,
                                  perecent_occurance_weed2 = perecent_occurance,
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



       
