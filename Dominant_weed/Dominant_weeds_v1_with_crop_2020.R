
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
  filter(!is.na(Waypoint))
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
# HR_weed_list_2020_Vic <-
#   read_excel(
#     "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
#     sheet = "Vic"
#   ) %>% mutate(tab = "Vic") %>%
#   filter(!is.na(Sample))
HR_weed_list_2020_WA <-
  read_excel(
    "W:/Economic impact of weeds round 2/HR/raw_data/Weed species data 2020 survey.xlsx",
    sheet = "WA"
  ) %>% mutate(tab = "WA") %>%
  filter(!is.na(Waypoint))
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
  filter(!is.na(Waypoint))

HR_weed_list_2020_Qld_Summer<- HR_weed_list_2020_Qld_Summer %>% rename(Sample = Waypoint)
HR_weed_list_2020_WA<- HR_weed_list_2020_WA %>% rename(Sample = Waypoint)
HR_weed_list_2020_Qld<- HR_weed_list_2020_Qld %>% rename(Sample = Waypoint)

###################################################################################################################
### List of all the weeds .

list1<- as.data.frame(colnames(HR_weed_list_2020_N_NSW)) %>% rename("list" = "colnames(HR_weed_list_2020_N_NSW)")
list2<- as.data.frame(colnames(HR_weed_list_2020_N_NSW_Summer))%>% rename("list" = "colnames(HR_weed_list_2020_N_NSW_Summer)")
list3<- as.data.frame(colnames(HR_weed_list_2020_Qld))%>% rename("list" = "colnames(HR_weed_list_2020_Qld)")
list4<- as.data.frame(colnames(HR_weed_list_2020_Qld_Summer))%>% rename("list" = "colnames(HR_weed_list_2020_Qld_Summer)")
list5<- as.data.frame(colnames(HR_weed_list_2020_S_NSW))%>% rename("list" = "colnames(HR_weed_list_2020_S_NSW)")
list6<- as.data.frame(colnames(HR_weed_list_2020_SA))%>% rename("list" = "colnames(HR_weed_list_2020_SA)")
list7<- as.data.frame(colnames(HR_weed_list_2020_TAs))%>% rename("list" = "colnames(HR_weed_list_2020_TAs)")
#list8<- as.data.frame(colnames(HR_weed_list_2020_Vic))%>% rename("list" = "colnames(HR_weed_list_2020_Vic)")
#No data in VIC

list9<- as.data.frame(colnames(HR_weed_list_2020_WA))%>% rename("list" = "colnames(HR_weed_list_2020_WA)")

list_clms <- rbind(list1, list2, list3, list4, list5, list6, list7, list9) #list8VIC has no data
list_clms <- list_clms %>%  distinct()
#list_clms <- toupper(list_clms)

rm(list1, list2, list3, list4, list5, list6, list7,list9) # list8 VIC has no data
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
#HR_weed_list_2020_Vic <- add_missing_cols(HR_weed_list_2020_Vic,list_clms)
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
  #HR_weed_list_2020_Vic,
  HR_weed_list_2020_WA

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
    
    -"...13",  
    -"...2",
    -"...4",
    -"...2",
    -"...16",
    -"...48",
    -"...49",
    -"...27",
    
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
                                                tab =="SA"|
                                                tab =="Tas"|
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


all_states_with_data <- all_states_with_data%>%  dplyr::select(ID_Jaxs:"Wild sage" ) # make sure this is the last weed on the list
dim(all_states_with_data)
all_states_with_data <- all_states_with_data %>%  rename(AEZ = `GRDC AEZ`)
all_states_with_data[, 7:157] <- lapply(all_states_with_data[, 7:157], as.character) #Note that this step will need to be checked ie are all the weed togther and what clm they start and stop


all_states_with_data_long <- pivot_longer(all_states_with_data,
                                  cols = c(`Annual ryegrass`:`Wild sage`), # makes sure this is the last weed on the list
                                  names_to = "weed",
                                  values_to="weed_class"
                                  )


str(all_states_with_data_long)
################################################################################
### some of the weed species names need fixing up.
all_states_with_data_long$weed <- toupper(all_states_with_data_long$weed)

all_states_with_data_long <- all_states_with_data_long %>% arrange(weed)

unique(all_states_with_data_long$weed)



all_states_with_data_long <- all_states_with_data_long %>% mutate(
  weed = case_when(
    weed == "BIND WEED" ~ "BINDWEED",
    weed == "BLACKBERRY NIGHT SHADE" |
    weed == "NIGHTSHADE SPP"  ~ "BLACKBERRY NIGHTSHADE",
    weed == "BRASSICA SPECIES"  ~ "BRASSICA",
   # weed == "BRACHIARIA SPP."  ~ "BRASSICA", ? is this a typo?
    
    weed == "MEXICAN POPPIES"  ~ "MEXICAN POPPY",
    weed == "SHEPHERD'S PURSE CAPSELLA BURSA-PASTORIS"  ~ "SHEPHERDS PURSE",
    weed == "WO"  ~ "WILD OATS",
    weed == "WIRE WEED"  ~ "WIREWEED",
    
    weed == "BLUE HELIOTOPE...86"  ~ "BLUE HELIOTOPE",
    weed == "BLUE HELIOTOPE...87"  ~ "BLUE HELIOTOPE",
    
    
    TRUE ~weed
  )
)







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
      
      weed_class =="pasturH" ~ NA,
      weed_class =="I see no Tasmanian data either" ~ NA,
      TRUE ~ weed_class
    )
  )

unique(all_states_with_data_long$weed_class_code)

#### keep the AEZ that we will use ###
unique(all_states_with_data_long$AEZ)

all_states_with_data_long <-
  all_states_with_data_long %>% filter(
      AEZ ==  "NSW NE / Qld SE" |
      AEZ ==  "NSW NEW / Qld SE" | #this looks like a typo
      AEZ ==  "NSW NW / Qld SW" |
      
      AEZ ==  "Qld Central" |
      AEZ ==  "SA Mid N Lower YP EP" |
      AEZ ==  "SA Vic Mallee"  |
      AEZ ==  "SA Vic Bordertown Wimmera" |                   
        
      AEZ ==  "NSW Central" |
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
      Crop == "Triticale" |
      Crop == "Corn"|
      Crop == "Cereal rye"|
      Crop == "Oaten hay"
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
      Crop == "peas" |
      Crop == "Lupins" |
      Crop == "Lentils" |
      Crop == "lentils" |
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
      Crop == "Lupin"  |
      Crop == "lupin" |
      Crop == "wheat or lupin"  |
      Crop ==  "Vetch" |
      Crop == "Mungbeans"|
      Crop == "Mungbean"|
      Crop == "Field Peas"|
      Crop == "Chickpea (edge)"
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
      Crop =="Perennial Pasture" |
      Crop =="Pasture"~ "Pasture",  
      Crop =="pasture"~ "Pasture",  
      
      Crop == "Fallow"  ~ "Fallow", 
      Crop == "Sorghum" ~   "Sorghum", 
      Crop == "Fallow/ Sorghum" ~   "Sorghum", 
      Crop == "Sorghum/Fallow" ~   "Sorghum", 
      
      Crop == "D/L Cotton" ~   "Cotton",
      Crop == "Safflower"|
      Crop == "Maize" ~ "non_cereal_cotton_crop",
      Crop == "Roadside" ~ "non_cereal_cotton_crop",
      Crop == "Railway" ~ "non_cereal_cotton_crop",
      
      Crop == "No data" ~ "No data",
      Crop == "0" ~ "No data",
    TRUE                      ~ Crop
  ))



unique(all_states_with_data_long$crop_grouping)


## need to check that some of these crops are coded correctly #NOTE WE HAVE HEAPS OF MISSING DATA ENTRIES FOR CROPS 


unique(all_states_with_data_long$crop_grouping)
check_what_coded_other<- all_states_with_data_long %>% filter(crop_grouping == "other")
check_what_coded_Broadleaf<- all_states_with_data_long %>% filter(crop_grouping == "Broadleaf")

rm(check_what_coded_other, check_what_coded_Broadleaf)

################################################################################


write.csv(all_states_with_data_long, "W:/Economic impact of weeds round 2/HR/Jackie_working/Weed_list/2020_data/all_states_with_data_long_step1.csv")
write.csv(all_states_with_data_long, "C:/Users/ouz001/working_from_home_post_Sep2022/weed_round2_offline/all_states_with_data_long_step1.csv")



unique(all_states_with_data_long$weed)
