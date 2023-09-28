
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
"W:\Economic impact of weeds round 2\HR\Jackie_working\Weed_list\2020_data"

write.csv(all_states_with_data_long, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/all_states_with_data_long_step1.csv")

