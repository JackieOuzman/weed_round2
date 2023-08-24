
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


HR_weed_list_NSW <-  read_excel("W:/Economic impact of weeds round 2/HR/raw_data/Weed Species locations no co-ordinates.xlsx", 
                                                     sheet = "NSW")

HR_weed_list_Tas <-  read_excel("W:/Economic impact of weeds round 2/HR/raw_data/Weed Species locations no co-ordinates.xlsx", 
                                sheet = "Tas")


## Add ID to dataset so each row has a unique ID
HR_weed_list_NSW <- HR_weed_list_NSW %>% 
  mutate(ID = row_number()) %>% 
  mutate(ID_Jaxs = paste0(ID,"_NSW"))

HR_weed_list_NSW <- HR_weed_list_NSW %>% select(-ID)
HR_weed_list_NSW <- HR_weed_list_NSW %>% select(ID_Jaxs, everything())
## make it long NSW
str(HR_weed_list_NSW)
HR_weed_list_NSW <- HR_weed_list_NSW%>%  dplyr::select(ID_Jaxs:Wireweed)
str(HR_weed_list_NSW)
HR_weed_list_NSW <- HR_weed_list_NSW %>%  rename(AEZ = `GRDC AEZ`)

HR_weed_list_NSW_long <- pivot_longer(HR_weed_list_NSW,
                                  cols = c(`3 corner Jack`:`Wireweed`),
                                  names_to = "weed",
                                  values_to="weed_class"
                                  )


str(HR_weed_list_NSW_long)


## make it long Tas
str(HR_weed_list_Tas)
HR_weed_list_Tas <- HR_weed_list_Tas%>%  dplyr::select(Sample:Wireweed)
str(HR_weed_list_Tas)
## Add ID to dataset so each row has a unique ID
HR_weed_list_Tas <- HR_weed_list_Tas %>% 
  mutate(ID = row_number()) %>% 
  mutate(ID_Jaxs = paste0(ID,"_Tas"))

HR_weed_list_Tas <- HR_weed_list_Tas %>% select(-ID)
HR_weed_list_Tas <- HR_weed_list_Tas %>% select(ID_Jaxs, everything())



HR_weed_list_Tas <- HR_weed_list_Tas %>%  rename(AEZ = `GRDC AEZ`)

HR_weed_list_Tas_long <- pivot_longer(HR_weed_list_Tas,
                                      cols = c(`3 corner Jack`:`Wireweed`),
                                      names_to = "weed",
                                      values_to="weed_class"
)


str(HR_weed_list_Tas_long)


HR_weed_list_long <- rbind(HR_weed_list_NSW_long, HR_weed_list_Tas_long)
rm(HR_weed_list_NSW, HR_weed_list_Tas, HR_weed_list_NSW_long,HR_weed_list_Tas_long,  )

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



#### drop AEZ that we wont use ###
unique(HR_weed_list_long$AEZ)

HR_weed_list_long <- HR_weed_list_long %>% filter(  AEZ ==  "NSW NW Qld SW" | 
                                                    AEZ ==  "NSW NE Qld SE" |
                                                    AEZ ==  "NSW Vic Slopes"|
                                                    AEZ ==  "Vic High Rainfall"|
                                                    AEZ ==  "NSW Central"|
                                                    AEZ ==  "Tas Grain"
                                                      )


## remove all the rows with missing weeds
HR_weed_list_long_remove_na <- HR_weed_list_long %>% filter(!is.na(weed_class))
#rm(HR_weed_list_long, HR_weed_list)
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

rm(list_of_weed_AEZ,list_of_weed_AEZ_count )


################################################################################
## how many paddocks per zone
str(HR_weed_list_long_remove_na)

paddock_per_AEZ_year <- HR_weed_list_long %>%  count(ID_Jaxs, AEZ, Year) #


paddock_per_AEZ_year <- paddock_per_AEZ_year %>% 
  group_by(AEZ, Year) %>% 
  count(AEZ)
paddock_per_AEZ_year <- ungroup(paddock_per_AEZ_year)
str(paddock_per_AEZ_year)
paddock_per_AEZ_year <- paddock_per_AEZ_year %>%  rename(`count of paddocks` = n)

paddock_per_AEZ_year


paddock_per_AEZ_year %>% 
  ggplot(aes(as.factor(Year),  `count of paddocks`, group=AEZ, color = AEZ))+
  geom_point()+
   theme_bw()+ 
   labs(title = "Number of paddock by AEZ and Year",
        x = "Year",  
        y ="Number of paddocks smapled")

ggsave(
  device = "png",
  filename = "Number of paddock.png",
  path= "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/",
  width=9.5,
  height = 6.28,
  dpi=600
) 



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
rm( AEZ_weeds_count)#paddock_per_AEZ_year

################################################################################
## add a rank ## ranks without averaging # first occurrence wins
str(AEZ_weeds)

AEZ_weeds <- AEZ_weeds %>% arrange(AEZ, Year) %>%
  group_by(AEZ, Year) %>%
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


write.csv(top4weeds, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/top4weeds.csv")



################################################################################
#Addd ranking to the long list of weeds

rank1_2 <- top4weeds %>% filter(rank == 1 | rank==2) 
rank <- left_join(HR_weed_list_long_remove_na, rank1_2)
## remove all the rows with missing weeds
rank <- rank %>% filter(!is.na(rank))
str(rank)
unique(rank$weed_class)

rank$weed_class <- as.factor(rank$weed_class)
levels(rank$weed_class)
rank$weed_class <- factor(rank$weed_class, levels=c("VL", "L", "M", "H", "VH"))

label_weed1 <- rank %>% 
  filter(rank == 1) %>% 
  distinct(Year,AEZ, .keep_all = TRUE)

label_weed2 <- rank %>% 
  filter(rank == 2) %>% 
  distinct(Year,AEZ, .keep_all = TRUE)

rm(top4weeds)

### Plot of weed 1 and 2
rank %>% 
 filter(rank == 1) %>% 
ggplot(aes(as.factor(Year),  weed_class))+
  geom_jitter(aes(colour = weed_class), position=position_jitter(0.2))+
   facet_wrap(.~AEZ)+
   geom_text(data=label_weed1, mapping = aes(label=stringr::str_wrap(weed,4),y="VH",x=as.factor(Year)), angle = 90, size =3, colour= "black")+
   theme_bw()+ 
   labs(title = "Most common weed by AEZ and Year",
        subtitle = "Rank weed 1",
        caption = "Using: Weed Species locations no co-ordinates.xlsx",
        x = "Year",  
       y ="Weed Class")+
   theme(legend.position = "none")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))


rank %>% 
  filter(rank == 2) %>% 
  ggplot(aes(as.factor(Year),  weed_class))+
  geom_jitter(aes(colour = weed_class), position=position_jitter(0.2))+
  facet_wrap(.~AEZ)+
  geom_text(data=label_weed2, mapping = aes(label=stringr::str_wrap(weed,4),y="VH",x=as.factor(Year)), angle = 90, size =3, colour= "black")+
  theme_bw()+ 
  labs(title = "Most common weed by AEZ and Year",
       subtitle = "Rank weed 2",
       caption = "Using: Weed Species locations no co-ordinates.xlsx",
       x = "Year",  
       y ="Weed Class")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  group_by(weed, AEZ, Year, rank) %>% 
  summarise(
    mode =mode(weed_class_code),
    median = median(weed_class_code, na.rm = TRUE)
  ) %>% 
  arrange(AEZ, rank, Year )


################################################################################
str(rank_AEZ_weed_densities)
rank_AEZ_weed_densities <- ungroup(rank_AEZ_weed_densities)

### add the mode and median back into the rank data 
rank_density_mode <- left_join(rank, rank_AEZ_weed_densities)

rank_density_mode_display_weed1 <- rank_density_mode %>% 
  filter(rank == 1) %>% 
  distinct(Year,AEZ, .keep_all = TRUE) %>% select(Year,AEZ, mode)
rank_density_mode_display_weed2 <- rank_density_mode %>% 
  filter(rank == 2) %>% 
  distinct(Year,AEZ, .keep_all = TRUE) %>% select(Year,AEZ, mode)

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
#turn it into a factor so it displays better
rank_density_mode_display_weed1$weed_class_Mode <- as.factor(rank_density_mode_display_weed1$weed_class_Mode)
levels(rank_density_mode_display_weed1$weed_class_Mode)
rank_density_mode_display_weed1$weed_class_Mode <- factor(rank_density_mode_display_weed1$weed_class_Mode, levels=c("VL", "L", "M", "H", "VH"))

rank_density_mode_display_weed2$weed_class_Mode <- as.factor(rank_density_mode_display_weed2$weed_class_Mode)
levels(rank_density_mode_display_weed2$weed_class_Mode)
rank_density_mode_display_weed2$weed_class_Mode <- factor(rank_density_mode_display_weed2$weed_class_Mode, levels=c("VL", "L", "M", "H", "VH"))

### Plot of weed 1 and 2
rank1_plot <- rank_density_mode %>% 
  filter(rank == 1) %>% 
  ggplot(aes(as.factor(Year),  weed_class))+
  #geom_point(data = rank_density_mode_display_weed1, aes(as.factor(Year),  weed_class_Mode))+
  geom_jitter(aes(colour = weed_class), position=position_jitter(0.2))+
  facet_wrap(.~AEZ)+
  geom_text(data=label_weed1, mapping = aes(label=stringr::str_wrap(weed,4),y="VH",x=as.factor(Year)), angle = 90, size =3, colour= "black")+
  theme_bw()+ 
  labs(title = "Most common weed by AEZ and Year",
       subtitle = "Rank weed 1, mode of densisty in black",
       caption = "Using: Weed Species locations no co-ordinates.xlsx",
       x = "Year",  
       y ="Weed Class")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
### Add the mode data
rank1_plot + geom_point(data = rank_density_mode_display_weed1, aes(as.factor(Year),  weed_class_Mode))

ggsave(
  device = "png",
  filename = "Rank1.png",
  path= "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/",
  width=9.5,
  height = 6.28,
  dpi=600
) 


 

### Plot of weed 1 and 2
rank2_plot <- rank_density_mode %>% 
  filter(rank == 2) %>% 
  ggplot(aes(as.factor(Year),  weed_class))+
  #geom_point(data = rank_density_mode_display_weed1, aes(as.factor(Year),  weed_class_Mode))+
  geom_jitter(aes(colour = weed_class), position=position_jitter(0.2))+
  facet_wrap(.~AEZ)+
  geom_text(data=label_weed2, mapping = aes(label=stringr::str_wrap(weed,4),y="VH",x=as.factor(Year)), angle = 90, size =3, colour= "black")+
  theme_bw()+ 
  labs(title = "Most common weed by AEZ and Year",
       subtitle = "Rank weed 2, mode of densisty in black",
       caption = "Using: Weed Species locations no co-ordinates.xlsx",
       x = "Year",  
       y ="Weed Class")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Add the mode data
rank2_plot + geom_point(data = rank_density_mode_display_weed2, aes(as.factor(Year),  weed_class_Mode))

ggsave(
  device = "png",
  filename = "Rank2.png",
  path= "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/",
  width=9.5,
  height = 6.28,
  dpi=600
) 

rm(AEZ_weeds, label_weed1, label_weed2,rank, rank_density_mode_display_weed1, rank_density_mode_display_weed2)


################################################################################
### Final list of weeds 

str(rank_density_mode)

Final_list_AEZ_Year <-      rank_density_mode %>% 
  distinct(Year, AEZ, rank, .keep_all = TRUE) %>% 
  select(Year, AEZ, rank, weed, percent_occurance, mode ) %>% 
  arrange(AEZ, Year, rank)

Final_list_AEZ_Year <-Final_list_AEZ_Year %>% 
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


write.csv(Final_list_AEZ_Year, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/Final_Rank1_2_weed_AEZ_year.csv")

rm(rank_density_mode, rank1_2)
################################################################################
### Turn into a table

DT_Rank1 <- Final_list_AEZ_Year %>% filter(rank == 1) %>% 
  select(-mode, - weed_class_Mode, -rank) %>% 
  pivot_wider(names_from = Year,
              values_from =percent_occurance) 
 
DT_Rank1 <- DT_Rank1 %>% rename(
  OCC_2011 = `2011`,
  OCC_2012 = `2012`,
  OCC_2013 = `2013`,
  OCC_2014 = `2014`,
  OCC_2015 = `2015`,
  OCC_2016 = `2016`,
  OCC_2017 = `2017`,
  OCC_2018 = `2018`,
  OCC_2019 = `2019`
)


DT_Rank1_a <- Final_list_AEZ_Year %>% filter(rank == 1) %>% 
  select(-mode,  -rank, -percent_occurance) %>% 
  pivot_wider(names_from = Year,
              values_from =weed_class_Mode) 
  

DT_Rank1_a <- DT_Rank1_a %>% rename(
  den_2011 = `2011`,
  den_2012 = `2012`,
  den_2013 = `2013`,
  den_2014 = `2014`,
  den_2015 = `2015`,
  den_2016 = `2016`,
  den_2017 = `2017`,
  den_2018 = `2018`,
  den_2019 = `2019`
)


DT_Rank1_test <- left_join(DT_Rank1, DT_Rank1_a)

DT_Rank1_test <- DT_Rank1_test %>% 
  mutate(Year2011 = paste0("Percentage = " ,OCC_2011, " Density = ", den_2011),
         Year2012 = paste0("Percentage = " ,OCC_2012, " Density = ", den_2012),
         Year2013 = paste0("Percentage = " ,OCC_2013, " Density = ", den_2013),
         Year2014 = paste0("Percentage = " ,OCC_2014, " Density = ", den_2014),
         Year2015 = paste0("Percentage = " ,OCC_2015, " Density = ", den_2015),
         Year2016 = paste0("Percentage = " ,OCC_2016, " Density = ", den_2016),
         Year2017 = paste0("Percentage = " ,OCC_2017, " Density = ", den_2017),
         Year2018 = paste0("Percentage = " ,OCC_2018, " Density = ", den_2018),
         Year2019 = paste0("Percentage = " ,OCC_2019, " Density = ", den_2019)
         ) %>% 
  select(AEZ:weed, Year2011:Year2019)

DT_Rank1_test[DT_Rank1_test == "Percentage = NA Density = NA"] <- NA
datatable(DT_Rank1_test,  caption = 'Rank 1: Most common weed per AEZ and year.')






DT_Rank2 <- Final_list_AEZ_Year %>% filter(rank == 2) %>% 
  select(-mode, - weed_class_Mode, -rank) %>% 
  pivot_wider(names_from = Year,
              values_from =percent_occurance) 

DT_Rank2 <- DT_Rank2 %>% rename(
  OCC_2011 = `2011`,
  OCC_2012 = `2012`,
  OCC_2013 = `2013`,
  OCC_2014 = `2014`,
  OCC_2015 = `2015`,
  OCC_2016 = `2016`,
  OCC_2017 = `2017`,
  OCC_2018 = `2018`,
  OCC_2019 = `2019`
)


DT_Rank2_a <- Final_list_AEZ_Year %>% filter(rank == 2) %>% 
  select(-mode,  -rank, -percent_occurance) %>% 
  pivot_wider(names_from = Year,
              values_from =weed_class_Mode) 


DT_Rank2_a <- DT_Rank2_a %>% rename(
  den_2011 = `2011`,
  den_2012 = `2012`,
  den_2013 = `2013`,
  den_2014 = `2014`,
  den_2015 = `2015`,
  den_2016 = `2016`,
  den_2017 = `2017`,
  den_2018 = `2018`,
  den_2019 = `2019`
)


DT_Rank2_test <- left_join(DT_Rank2, DT_Rank2_a)


DT_Rank2_test <- DT_Rank2_test %>% 
  mutate(Year2011 = paste0("Percentage = " ,OCC_2011, " Density = ", den_2011),
         Year2012 = paste0("Percentage = " ,OCC_2012, " Density = ", den_2012),
         Year2013 = paste0("Percentage = " ,OCC_2013, " Density = ", den_2013),
         Year2014 = paste0("Percentage = " ,OCC_2014, " Density = ", den_2014),
         Year2015 = paste0("Percentage = " ,OCC_2015, " Density = ", den_2015),
         Year2016 = paste0("Percentage = " ,OCC_2016, " Density = ", den_2016),
         Year2017 = paste0("Percentage = " ,OCC_2017, " Density = ", den_2017),
         Year2018 = paste0("Percentage = " ,OCC_2018, " Density = ", den_2018),
         Year2019 = paste0("Percentage = " ,OCC_2019, " Density = ", den_2019)
  ) %>% 
  select(AEZ:weed, Year2011:Year2019)

DT_Rank2_test[DT_Rank2_test == "Percentage = NA Density = NA"] <- NA
datatable(DT_Rank2_test,  caption = 'Rank 2: 2nd Most common weed per AEZ and year.')


rm(DT_Rank1_a, DT_Rank2_a, DT_Rank1, DT_Rank2)

write.csv(DT_Rank1_test, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/Final_Rank1_weed_AEZ_year_format.csv")
write.csv(DT_Rank2_test, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/Final_Rank2_weed_AEZ_year_format.csv")



###############################################################################
### How many weeds per paddock per year

str(HR_weed_list_long)
str(HR_weed_list_long_remove_na)

Number_weeds_per_paddock_na <- HR_weed_list_long_remove_na %>% 
  group_by(ID_Jaxs, Year) %>% 
  summarise(n = n())
Number_weeds_per_paddock_na <- ungroup(Number_weeds_per_paddock_na)

## what about the paddocks with no weeds?

list_of_paddock_sample_ID_Yr <- HR_weed_list_long %>% distinct(ID_Jaxs, Year) %>% mutate(check="1")
str(list_of_paddock_sample_ID_Yr)
str(Number_weeds_per_paddock_na)

Number_weeds_per_paddock <- left_join(list_of_paddock_sample_ID_Yr, Number_weeds_per_paddock_na)
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
    #fill = ..x..  ),
  stat = 'count',
  show.legend = FALSE) +
  scale_x_discrete(drop = FALSE) +
  labs(x = 'Number of weed species per paddock', 
       y = 'Percent', 
       title = "Percentage of fields containing multiple weed species",
       subtitle = "NSW and Tas AEZ zones, years 2011-2019") +
  #theme_minimal() +
  theme(panel.grid.minor = element_blank())







ggsave(
  device = "png",
  filename = "Percentage of fields containing multiple weed species VIC.png",
  path= "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/",
  width=9.5,
  height = 6.28,
  dpi=600
) 




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

paddock_per_AEZ <- HR_weed_list_long %>%  count(ID_Jaxs, AEZ) %>%  select (ID_Jaxs, AEZ) #This is just a list of paddock
paddock_per_AEZ_1 <- paddock_per_AEZ %>% count(AEZ)
  
paddock_per_AEZ_1 <-paddock_per_AEZ_1 %>%  rename(`count of paddocks` = n)
################################################################################
#count the number of weed occurrence per AEZ, weed   
AEZ_weeds_count <- HR_weed_list_long_remove_na %>% count(AEZ, weed, sort = TRUE)    
AEZ_weeds_count <- AEZ_weeds_count %>%  arrange(AEZ, weed)
str(AEZ_weeds_count) 
AEZ_weeds_count <- AEZ_weeds_count %>%  rename(count = n)

#count the number of paddocks sampled per AEZ (from above)
paddock_per_AEZ_1

################################################################################
#### join the summary data together
AEZ_weeds <- left_join(AEZ_weeds_count,paddock_per_AEZ_1)
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


write.csv(top4weeds, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/top4weeds_noYear.csv")



################################################################################
#Addd ranking to the long list of weeds

rank1_2 <- top4weeds %>% filter(rank == 1 | rank==2) 
rank <- left_join(HR_weed_list_long_remove_na, rank1_2)
## remove all the rows with missing weeds
rank <- rank %>% filter(!is.na(rank))
str(rank)
unique(rank$weed_class)



###summaries the weed class   
str(rank)

rank$weed_class_code <- as.double(rank$weed_class_code)
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
  distinct(Year,AEZ, .keep_all = TRUE) %>% select(AEZ, mode)
rank_density_mode_display_weed2 <- rank_density_mode %>% 
  filter(rank == 2) %>% 
  distinct(Year,AEZ, .keep_all = TRUE) %>% select(AEZ, mode)

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


################################################################################
### Final list of weeds 

str(rank_density_mode)

Final_list_AEZ <-      rank_density_mode %>% 
  distinct(AEZ, rank, .keep_all = TRUE) %>% 
  select(AEZ, rank, weed, percent_occurance, mode ) %>% 
  arrange(AEZ, rank)

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


write.csv(Final_list_AEZ, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/Final_Rank1_2_weed_AEZ.csv")






