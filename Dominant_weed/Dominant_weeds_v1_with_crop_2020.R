
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


HR_weed_list_2020_S_NSW <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "S NSW"
  ) %>% mutate(tab = "S_NSW") %>%
  filter(!is.na(Sample))
HR_weed_list_2020_N_NSW <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "N NSW"
  ) %>% mutate(tab = "N_NSW") %>%
  filter(!is.na(Sample))
HR_weed_list_2020_Qld <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "Qld"
  ) %>% mutate(tab = "Qld") %>%
  filter(!is.na(Sample))
HR_weed_list_2020_SA <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "SA"
  ) %>% mutate(tab = "SA") %>%
  filter(!is.na(Sample))
HR_weed_list_2020_TAs <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "Tas"
  ) %>% mutate(tab = "Tas") %>%
  filter(!is.na(Sample))
HR_weed_list_2020_Vic <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "Vic"
  ) %>% mutate(tab = "Vic") %>%
  filter(!is.na(Sample))
HR_weed_list_2020_WA <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "WA"
  ) %>% mutate(tab = "WA") %>%
  filter(!is.na(Sample))
HR_weed_list_2020_N_NSW_Summer <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "N NSW Summer"
  ) %>% mutate(tab = "N_NSW_Summer") %>%
  filter(!is.na(Sample))
HR_weed_list_2020_Qld_Summer <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "Qld Summer"
  ) %>% mutate(tab = "Qld_Summer") %>%
  filter(!is.na(Sample))


###################################################################################################################
### List of all the weeds .

list1<- as.data.frame(colnames(HR_weed_list_2020_N_NSW)) %>% rename("list" = "colnames(HR_weed_list_2020_N_NSW)")
list2<- as.data.frame(colnames(HR_weed_list_2020_N_NSW_Summer))%>% rename("list" = "colnames(HR_weed_list_2020_N_NSW_Summer)")
list3<- as.data.frame(colnames(HR_weed_list_2020_Qld))%>% rename("list" = "colnames(HR_weed_list_2020_Qld)")
list4<- as.data.frame(colnames(HR_weed_list_2020_Qld_Summer))%>% rename("list" = "colnames(HR_weed_list_2020_Qld_Summer)")
list5<- as.data.frame(colnames(HR_weed_list_2020_S_NSW))%>% rename("list" = "colnames(HR_weed_list_2020_S_NSW)")
list6<- as.data.frame(colnames(HR_weed_list_2020_SA))%>% rename("list" = "colnames(HR_weed_list_2020_SA)")
list7<- as.data.frame(colnames(HR_weed_list_2020_TAs))%>% rename("list" = "colnames(HR_weed_list_2020_TAs)")
list8<- as.data.frame(colnames(HR_weed_list_2020_Vic))%>% rename("list" = "colnames(HR_weed_list_2020_Vic)")
list9<- as.data.frame(colnames(HR_weed_list_2020_WA))%>% rename("list" = "colnames(HR_weed_list_2020_WA)")

list_clms <- rbind(list1, list2, list3, list4, list5, list6, list7, list8, list9)
list_clms <- list_clms %>%  distinct()

rm(list1, list2, list3, list4, list5, list6, list7, list8, list9)
print(list_clms)


list_clms <- array(data = c(unlist(list_clms)))


################################################################################


add_missing_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df <- df %>%
        add_column(!!sym(col) := NA)
    }
  }
  df
}

# Use the function so that all the clms are the same 

HR_weed_list_2020_N_NSW <- add_missing_cols(HR_weed_list_2020_N_NSW,list_clms)
HR_weed_list_2020_N_NSW_Summer <- add_missing_cols(HR_weed_list_2020_N_NSW_Summer,list_clms)
HR_weed_list_2020_Qld <- add_missing_cols(HR_weed_list_2020_Qld,list_clms)
HR_weed_list_2020_Qld_Summer <- add_missing_cols(HR_weed_list_2020_Qld_Summer,list_clms)
HR_weed_list_2020_SA <- add_missing_cols(HR_weed_list_2020_SA,list_clms)
HR_weed_list_2020_TAs <- add_missing_cols(HR_weed_list_2020_TAs,list_clms)
HR_weed_list_2020_Vic <- add_missing_cols(HR_weed_list_2020_Vic,list_clms)
HR_weed_list_2020_WA <- add_missing_cols(HR_weed_list_2020_WA,list_clms)
HR_weed_list_2020_S_NSW <- add_missing_cols(HR_weed_list_2020_S_NSW,list_clms)


### what can I merge?
all_states <- rbind(
  HR_weed_list_2020_N_NSW,
  HR_weed_list_2020_N_NSW_Summer,
  HR_weed_list_2020_Qld,
  HR_weed_list_2020_Qld_Summer,
  HR_weed_list_2020_S_NSW,
  HR_weed_list_2020_SA,
  HR_weed_list_2020_TAs,
  HR_weed_list_2020_Vic,
  HR_weed_list_2020_WA,
  HR_weed_list_2020_S_NSW
)

#remove the empty clms (the ones I think are empty - with numbers)
all_states <- all_states %>%
  select(
    -"...20",-"...38",-"...66",
    -"...65",
    -"...21",
    -"...39",
    -"...9",
    -"...12",
    -"...67",
    -"Species present"#,
    #-"Waypoint" # I replaced this with Sample in the raw data
  )

colnames(all_states)


#################################################################################
# I have one large df with all the states.
## next problem is that some states have no data or no crop class - these need to be filtered out until the database is updated.
unique(all_states$tab)


all_states_with_data <- all_states %>% filter(tab == "N_NSW"|
                                                tab =="N_NSW_Summer"|
                                                tab =="Qld"|
                                                tab =="Qld_Summer"|
                                                tab == "S_NSW"|
                                              #tab ==SA,
                                              #tab ==Tas,
                                              #tab ==Vic,
                                                tab =="WA")


rm(list = setdiff(ls(), "all_states_with_data") )


################################################################################
## ready to start!

str(all_states_with_data)





## Add ID to dataset so each row has a unique ID
all_states_with_data <- all_states_with_data %>% 
  mutate(ID = row_number()) %>% 
  mutate(ID_Jaxs = paste0(ID,"_", tab))

all_states_with_data <- all_states_with_data %>% select(-ID)
all_states_with_data <- all_states_with_data %>% select(tab, everything())
all_states_with_data <- all_states_with_data %>% select(ID_Jaxs, everything())
## make it long 
colnames(all_states_with_data)


all_states_with_data <- all_states_with_data%>%  dplyr::select(ID_Jaxs:"Ox  Tongue" )
dim(all_states_with_data)
all_states_with_data <- all_states_with_data %>%  rename(AEZ = `GRDC AEZ`)
all_states_with_data[, 7:144] <- lapply(all_states_with_data[, 7:144], as.character) #Note that this step will need to be checked ie are all the weed togther and what clm they start and stop


all_states_with_data_long <- pivot_longer(all_states_with_data,
                                  cols = c(`Annual ryegrass`:`Ox  Tongue`),
                                  names_to = "weed",
                                  values_to="weed_class"
                                  )


str(all_states_with_data_long)

unique(all_states_with_data_long$weed_class)

## recode weed class with an number
all_states_with_data_long <-all_states_with_data_long %>% 
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



#### keep the AEZ that we will use ###
unique(all_states_with_data_long$AEZ)

all_states_with_data_long <-
  all_states_with_data_long %>% filter(
      AEZ ==  "NSW NE / Qld SE" |
      AEZ ==  "NSW NW / Qld SW" |
      AEZ ==  "NSW Central" |
      AEZ ==  "Qld Central" |
      AEZ ==  "NSW NEW / Qld SE" | #this looks like a typo
      AEZ ==  "NSW Vic Slopes" |
      AEZ ==  "WA Northern" |
      AEZ ==  "WA Central" |
      AEZ ==  "WA Eastern" |
      AEZ ==  "WA Sandplain"
  )

### Fix up typo 
all_states_with_data_long <-
  all_states_with_data_long %>% mutate(AEZ= case_when(
    AEZ == "NSW NEW / Qld SE" ~"NSW NE / Qld SE",
    TRUE ~AEZ)
  ) 




################################################################################
### Recode crop into groups and drop hort crops

unique(all_states_with_data_long$Crop)
 


all_states_with_data_long <- all_states_with_data_long %>% 
  mutate(crop_grouping = case_when(
      Crop == "Wheat" |
      Crop == "US Wheat" |
      Crop == "Barley" |
      Crop == "US Barley" |
      Crop == "oats" |
      Crop == "Oats" |
      Crop == "Triticale" |
      Crop == "Wheat" |
      Crop == "wheat" |
      Crop == "Barley" |
      Crop == "barley" |
      Crop == "oat" |
      Crop == "Oats" |
      Crop == "wheat /barley" |
      Crop == "Triticale"
        ~ "Cereals",
      
    
      Crop == "Canola" |
      Crop == "Chick Peas" |
      Crop == "Chickpea" |
      Crop == "Chick peas" |
      Crop == "Faba beans" |
      Crop == "Field peas" |
      Crop == "Broad beans" |
      Crop == "Albus Lupins" |
      Crop == "Albus lupins" |
      Crop == "Peas" |
      Crop == "Lupins" |
      Crop == "Lentils" |
      Crop == "Lupins _narrow" |
      Crop == "Lupins_Albus" |
      Crop == "Lupins_narrow" |
      Crop == "Lupins/ wheat" |
      Crop == "Freezer Peas" |
      Crop == "Linseed" |
      Crop == "Lupins/ wheat" |
      Crop == "Canola" |
      Crop == "canola"  |
      Crop == "Chickpea" |
      Crop == "Chick Peas" |
      Crop == "chickpea" |
      Crop == "Faba Beans" |
      Crop == "faba bean"  |
      Crop == "field pea" |
      Crop == "Cow Peas"  |
      Crop == "Broad Beans" |
      Crop == "Lupins"  |
      Crop == "lupin" |
      Crop == "wheat or lupin"  |
      Crop ==  "Vetch"      
      ~ "Broadleaf",
    
    
      Crop == "Pasture" | 
      Crop == "Annual pasture" | 
      Crop == "Annual Pasture" |
      Crop == "Perennial pasture" |
      Crop == "Lucerne" |
      Crop == "Perennial Pasture" |
      Crop =="Annual Pasture"    |
      Crop =="Annual pasture"     |             
      Crop =="Perennial pasture"|
      Crop =="Perennial Pasture" 
      ~ "Pasture",  
    
      Crop == "Fallow"  ~ "Fallow", 
      
      Crop == "Sorghum" ~   "Sorghum"  ,        
      
      Crop == "Safflower"|
      Crop == "Maize" ~ "non_cereal_cotton_crop",
        
    TRUE                      ~ "other"
  ))






## need to check that some of these crops are coded correctly #NOTE WE HAVE HEAPS OF MISSING DATA ENTRIES FOR CROPS 


unique(all_states_with_data_long$crop_grouping)
check_what_coded_other<- all_states_with_data_long %>% filter(crop_grouping == "other")
check_what_coded_Broadleaf<- all_states_with_data_long %>% filter(crop_grouping == "Broadleaf")

rm(check_what_coded_other, check_what_coded_Broadleaf)

################################################################################

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






