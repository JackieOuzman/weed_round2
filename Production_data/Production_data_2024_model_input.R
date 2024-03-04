library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

library(lubridate)


################################################################################
yld_2024_AEZ <-  read_excel(path =
                                 "W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/23.09.0014 GRDC AEZ Data with Legumes_jax.xlsx",
                               sheet ="Yield", skip = 3)

str(yld_2024_AEZ)
################################################################################

yld_2024_AEZ_long <- yld_2024_AEZ %>% 
  pivot_longer(cols = "Barley":"Cotton",
               names_to = "Crop",
               values_to = "Value")

yld_2024_AEZ_long <- yld_2024_AEZ_long %>% mutate(Unit = "yield")
rm(yld_2024_AEZ)

#1. merge sandplain and mallee

unique(yld_2024_AEZ_long$`AgroEcological Zone`)

yld_2024_AEZ_long_sand_plain_mallee <- yld_2024_AEZ_long %>% filter(`AgroEcological Zone` == "WA Sandplain" |
                                                                    `AgroEcological Zone` == "WA Mallee"  ) %>% 
  group_by(Year, Crop) %>% 
  summarise(mean = mean(Value, na.rm=TRUE)) 

yld_2024_AEZ_long_sand_plain_mallee<- ungroup(yld_2024_AEZ_long_sand_plain_mallee)

yld_2024_AEZ_long_sand_plain_mallee <- yld_2024_AEZ_long_sand_plain_mallee %>% 
  mutate(`AgroEcological Zone` =  "WA Sandplain/ Mallee",
         Unit = "yield") %>% 
  rename(Value = mean)

str(yld_2024_AEZ_long)
str(yld_2024_AEZ_long_sand_plain_mallee)

yld_2024_AEZ_long <- rbind(yld_2024_AEZ_long, yld_2024_AEZ_long_sand_plain_mallee)
################################################################################
#2. add in AEZ code

yld_2024_AEZ_long <- yld_2024_AEZ_long %>% 
  mutate(AEZ_code = case_when(
    `AgroEcological Zone` == "NSW Central"  ~ 5,
    `AgroEcological Zone` == "NSW NE/Qld SE" ~ 2, 
    `AgroEcological Zone` == "NSW NW/Qld SW" ~ 3,
    `AgroEcological Zone` == "NSW Vic Slopes" ~ 4,
    `AgroEcological Zone` == "Qld Central" ~ 1,
    `AgroEcological Zone` == "SA Midnorth-Lower Yorke Eyre"~ 6,
    `AgroEcological Zone` == "SA Vic Mallee"~ 7,
    `AgroEcological Zone` == "SA Vic Bordertown-Wimmera" ~ 8,
    `AgroEcological Zone` == "Tas Grain" ~ 9,
    `AgroEcological Zone` == "Vic High Rainfall" ~ 10, 
    `AgroEcological Zone` == "WA Central" ~ 11,
    `AgroEcological Zone` == "WA Eastern" ~ 12,
    `AgroEcological Zone` == "WA Northern"~ 13,
    `AgroEcological Zone` == "WA Sandplain/ Mallee"~ 14))


yld_2024_AEZ_long <- yld_2024_AEZ_long %>% filter(!is.na(AEZ_code))
rm(yld_2024_AEZ_long_sand_plain_mallee)
#### remove Sorghum and Cotton for AEZ above 5

unique(yld_2024_AEZ_long$Crop)

yld_2024_AEZ_long <- yld_2024_AEZ_long %>%
  filter(AEZ_code %in% c(1,2,3,4,5) | 
           AEZ_code %in% c(6,7,8,9,10,11,12,13,14) & !Crop %in% c("Grain Sorghum", "Cotton"))
  



################################################################################
H_2024_AEZ <-  read_excel(path =
                              "W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/23.09.0014 GRDC AEZ Data with Legumes_jax.xlsx",
                            sheet ="H Data", skip = 4)

str(H_2024_AEZ)
#################################################################################
#1. merge sandplain and mallee

unique(H_2024_AEZ$`AgroEcological Zone`)

H_2024_AEZ_mallee_sand <- H_2024_AEZ %>% filter(`AgroEcological Zone` == "WA Sandplain" |
                                                                      `AgroEcological Zone` == "WA Mallee"  ) %>% 
  group_by(Year, Crop, Unit) %>% 
  summarise(sum = sum(Value, na.rm=TRUE)) 

H_2024_AEZ_mallee_sand<- ungroup(H_2024_AEZ_mallee_sand)

str(H_2024_AEZ_mallee_sand)

H_2024_AEZ_mallee_sand <- H_2024_AEZ_mallee_sand %>% 
  mutate(`AgroEcological Zone` =  "WA Sandplain/ Mallee") %>% 
  rename(Value = sum)

str(H_2024_AEZ_mallee_sand)
str(H_2024_AEZ)

H_2024_AEZ <- rbind(H_2024_AEZ, H_2024_AEZ_mallee_sand)
################################################################################
#2. add in AEZ code

H_2024_AEZ <- H_2024_AEZ %>% 
  mutate(AEZ_code = case_when(
    `AgroEcological Zone` == "NSW Central"  ~ 5,
    `AgroEcological Zone` == "NSW NE/Qld SE" ~ 2, 
    `AgroEcological Zone` == "NSW NW/Qld SW" ~ 3,
    `AgroEcological Zone` == "NSW Vic Slopes" ~ 4,
    `AgroEcological Zone` == "Qld Central" ~ 1,
    `AgroEcological Zone` == "SA Midnorth-Lower Yorke Eyre"~ 6,
    `AgroEcological Zone` == "SA Vic Mallee"~ 7,
    `AgroEcological Zone` == "SA Vic Bordertown-Wimmera" ~ 8,
    `AgroEcological Zone` == "Tas Grain" ~ 9,
    `AgroEcological Zone` == "Vic High Rainfall" ~ 10, 
    `AgroEcological Zone` == "WA Central" ~ 11,
    `AgroEcological Zone` == "WA Eastern" ~ 12,
    `AgroEcological Zone` == "WA Northern"~ 13,
    `AgroEcological Zone` == "WA Sandplain/ Mallee"~ 14))


H_2024_AEZ <- H_2024_AEZ %>% filter(!is.na(AEZ_code))
rm(H_2024_AEZ_mallee_sand)

H_2024_AEZ <- H_2024_AEZ %>%
  filter(AEZ_code %in% c(1,2,3,4,5) | 
           AEZ_code %in% c(6,7,8,9,10,11,12,13,14) & !Crop %in% c("Grain Sorghum", "Cotton"))

################################################################################


################################################################################
str(H_2024_AEZ)
str(yld_2024_AEZ_long)

Kyntec_Prooduction_all <- rbind(H_2024_AEZ, yld_2024_AEZ_long)


################################################################################


Price_2024_AEZ <-  read_excel(path =
                            "W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/23.09.0014 GRDC AEZ Data with Legumes_jax.xlsx",
                          sheet ="Dollars", 
                          #skip = 6,
                          range = cell_rows(7:24))

Price_Pusles_2024_AEZ<-  read_excel(path =
                                      "W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/23.09.0014 GRDC AEZ Data with Legumes_jax.xlsx",
                                    sheet ="Dollars", 
                                    #skip = 6,
                                    range = cell_rows(33:48))


str(Price_2024_AEZ)
Price_2024_AEZ_long <- Price_2024_AEZ %>% 
  pivot_longer(cols =  c("Wheat Dollars_2019":"Cotton Dollars_3yr Avg"),
               names_to = "crop_year",
               values_to = "Value"
               )

Price_2024_AEZ_long <- Price_2024_AEZ_long %>%
  filter(
    crop_year != "Wheat Dollars_3yr Avg" &
      crop_year != "Barley Dollars_3yr Avg" &
      crop_year != "Oats Dollars_3yr Avg" &
      crop_year != "Canola Dollars_3yr Avg" &
      crop_year != "Grain Sorghum Dollars_3yr Avg" &
      crop_year != "Cotton Dollars_3yr Avg"
  )
str(Price_2024_AEZ_long)


Price_2024_AEZ_long <- Price_2024_AEZ_long %>% separate_wider_delim(crop_year, 
                                                     delim = "_",
                                                     names = c("Crop_text", "Year"))


unique(Price_2024_AEZ_long$Crop_text)
Price_2024_AEZ_long <- Price_2024_AEZ_long %>% mutate(
  Crop = case_when(Crop_text == "Wheat Dollars" ~ "Wheat",
                   Crop_text == "Barley Dollars"~ "Barley",
                   Crop_text ==  "Oats Dollars" ~ "Oats",
                   Crop_text == "Canola Dollars"  ~ "Canola",     
                   Crop_text == "Grain Sorghum Dollars" ~ "Grain Sorghum",
                   Crop_text == "Cotton Dollars"      ~ "Cotton"
  ))
  
  
Price_2024_AEZ_long <- Price_2024_AEZ_long %>%
  select(-Crop_text) %>% 
  mutate(Unit = "Dollars")

#################################################################################
#################################################################################
#1. merge sandplain and mallee

unique(Price_2024_AEZ_long$`AgroEcological Zone`)

Price_2024_AEZ_long_mallee_sand <- Price_2024_AEZ_long %>% filter(`AgroEcological Zone` == "WA Sandplain" |
                                                  `AgroEcological Zone` == "WA Mallee"  ) %>% 
  group_by(Year, Crop) %>% 
  summarise(mean = mean(Value, na.rm=TRUE)) 

Price_2024_AEZ_long_mallee_sand<- ungroup(Price_2024_AEZ_long_mallee_sand)

Price_2024_AEZ_long_mallee_sand <- Price_2024_AEZ_long_mallee_sand %>% 
  mutate(`AgroEcological Zone` =  "WA Sandplain/ Mallee",
         Unit = "Dollars") %>% 
  rename(Value = mean)

Price_2024_AEZ_long <- rbind(Price_2024_AEZ_long, Price_2024_AEZ_long_mallee_sand)

################################################################################
################################################################################
#2. add in AEZ code

Price_2024_AEZ_long <- Price_2024_AEZ_long %>% 
  mutate(AEZ_code = case_when(
    `AgroEcological Zone` == "NSW Central"  ~ 5,
    `AgroEcological Zone` == "NSW NE/Qld SE" ~ 2, 
    `AgroEcological Zone` == "NSW NW/Qld SW" ~ 3,
    `AgroEcological Zone` == "NSW Vic Slopes" ~ 4,
    `AgroEcological Zone` == "Qld Central" ~ 1,
    `AgroEcological Zone` == "SA Midnorth-Lower Yorke Eyre"~ 6,
    `AgroEcological Zone` == "SA Vic Mallee"~ 7,
    `AgroEcological Zone` == "SA Vic Bordertown-Wimmera" ~ 8,
    `AgroEcological Zone` == "Tas Grain" ~ 9,
    `AgroEcological Zone` == "Vic High Rainfall" ~ 10, 
    `AgroEcological Zone` == "WA Central" ~ 11,
    `AgroEcological Zone` == "WA Eastern" ~ 12,
    `AgroEcological Zone` == "WA Northern"~ 13,
    `AgroEcological Zone` == "WA Sandplain/ Mallee"~ 14))


Price_2024_AEZ_long <- Price_2024_AEZ_long %>% filter(!is.na(AEZ_code))
rm(Price_2024_AEZ_long_mallee_sand)

Price_2024_AEZ_long <- Price_2024_AEZ_long %>%
  filter(AEZ_code %in% c(1,2,3,4,5) | 
           AEZ_code %in% c(6,7,8,9,10,11,12,13,14) & !Crop %in% c("Grain Sorghum", "Cotton"))


################################################################################

##################################################################################


str(Price_Pusles_2024_AEZ)
Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ %>% 
  pivot_longer(cols =  c("Chickpea Dollars_2019":"Lupins Dollars_3yr Avg"),
               names_to = "crop_year",
               values_to = "Value"
  )

unique(Price_Pusles_2024_AEZ_long$crop_year)
Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ_long %>%
  filter(
    crop_year != "Chickpea Dollars_3yr Avg" &
      crop_year != "Faba Bean Dollars_3yr Avg" &
      crop_year != "Lentils Dollars_3yr Avg" &
      crop_year != "Field Pea Dollars_3yr Avg" &
      crop_year != "Lupins Dollars_3yr Avg" 
  )
str(Price_Pusles_2024_AEZ_long)


Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ_long %>% separate_wider_delim(crop_year, 
                                                                    delim = "_",
                                                                    names = c("Crop_text", "Year"))


unique(Price_Pusles_2024_AEZ_long$Crop_text)
Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ_long %>% mutate(
  Crop = case_when(Crop_text == "Chickpea Dollars" ~ "Chickpea",
                   Crop_text == "Faba Bean Dollars"~ "Faba Bean",
                   Crop_text ==  "Lentils Dollars" ~ "Lentils",
                   Crop_text == "Field Pea Dollars"  ~ "Field Pea",     
                   Crop_text == "Lupins Dollars"      ~ "Lupins"
  ))


Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ_long %>%
  select(-Crop_text) %>% 
  mutate(Unit = "Dollars")


##############################################################################
################################################################################
#1. merge sandplain and mallee

unique(Price_Pusles_2024_AEZ_long$`AgroEcological Zone`)

Price_Pusles_2024_AEZ_long_mallee_sand <- Price_Pusles_2024_AEZ_long %>% filter(`AgroEcological Zone` == "WA Sandplain" |
                                                  `AgroEcological Zone` == "WA Mallee"  ) %>% 
  group_by(Year, Crop) %>% 
  summarise(mean = mean(Value, na.rm=TRUE)) 

Price_Pusles_2024_AEZ_long_mallee_sand<- ungroup(Price_Pusles_2024_AEZ_long_mallee_sand)

Price_Pusles_2024_AEZ_long_mallee_sand <- Price_Pusles_2024_AEZ_long_mallee_sand %>% 
  mutate(`AgroEcological Zone` =  "WA Sandplain/ Mallee",
         Unit = "Dollars") %>% 
  rename(Value = mean)

str(Price_Pusles_2024_AEZ_long)
Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ_long %>% select( -`...22`)
Price_Pusles_2024_AEZ_long <- rbind(Price_Pusles_2024_AEZ_long, Price_Pusles_2024_AEZ_long_mallee_sand)

#################################################################################
################################################################################
#2. add in AEZ code

Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ_long %>% 
  mutate(AEZ_code = case_when(
    `AgroEcological Zone` == "NSW Central"  ~ 5,
    `AgroEcological Zone` == "NSW NE/Qld SE" ~ 2, 
    `AgroEcological Zone` == "NSW NW/Qld SW" ~ 3,
    `AgroEcological Zone` == "NSW Vic Slopes" ~ 4,
    `AgroEcological Zone` == "Qld Central" ~ 1,
    `AgroEcological Zone` == "SA Midnorth-Lower Yorke Eyre"~ 6,
    `AgroEcological Zone` == "SA Vic Mallee"~ 7,
    `AgroEcological Zone` == "SA Vic Bordertown-Wimmera" ~ 8,
    `AgroEcological Zone` == "Tas Grain" ~ 9,
    `AgroEcological Zone` == "Vic High Rainfall" ~ 10, 
    `AgroEcological Zone` == "WA Central" ~ 11,
    `AgroEcological Zone` == "WA Eastern" ~ 12,
    `AgroEcological Zone` == "WA Northern"~ 13,
    `AgroEcological Zone` == "WA Sandplain/ Mallee"~ 14))


Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ_long %>% filter(!is.na(AEZ_code))
rm(Price_Pusles_2024_AEZ_long_mallee_sand)

Price_Pusles_2024_AEZ_long <- Price_Pusles_2024_AEZ_long %>%
  filter(AEZ_code %in% c(1,2,3,4,5) | 
           AEZ_code %in% c(6,7,8,9,10,11,12,13,14) & !Crop %in% c("Grain Sorghum", "Cotton"))


################################################################################



###################################################################################
### create a Pulses values which is average of the pulse crops 
### NB Brianna has takes an average for 3 years by crop and then averaged these 3 years values to arrive at pulse value.
### I will do it a slightly different way so it matches the other price data
### create a value of pulses per year.

Price_Pusles_2024_AEZ_long <- ungroup(Price_Pusles_2024_AEZ_long)
str(Price_Pusles_2024_AEZ_long)


Pulse_Price_years <- Price_Pusles_2024_AEZ_long %>% 
  group_by(`AgroEcological Zone`,Year, AEZ_code ) %>% 
  summarise(Value = mean(Value,na.rm = TRUE))
  

Pulse_Price_years <- Pulse_Price_years %>% 
  mutate(Unit = "Dollars",
         Crop = "Pulse")

Pulse_Price_years <- ungroup(Pulse_Price_years)
###############################################################################
rm(Price_Pusles_2024_AEZ_long, Price_Pusles_2024_AEZ)

str(Price_2024_AEZ_long)
str(Pulse_Price_years)

Prices <- rbind(Price_2024_AEZ_long, Pulse_Price_years)
rm(Price_2024_AEZ_long, Pulse_Price_years)






Kyntec_Prooduction_all <- rbind(Kyntec_Prooduction_all, Prices)
rm(H_2024_AEZ, Prices, yld_2024_AEZ_long, Price_2024_AEZ_long)


###############################################################################
Kyntec_Prooduction_2019_2020_2021 <- Kyntec_Prooduction_all %>%
  filter(Year == 2019 |
           Year == 2020 |
           Year == 2021)

Kyntec_Prooduction_2019_2020_2021 <- Kyntec_Prooduction_2019_2020_2021 %>% rename(ahh = Value) #something weird was happening ahh fixed it!

Kyntec_Prooduction_2019_2020_2021 <- ungroup(Kyntec_Prooduction_2019_2020_2021)

str(Kyntec_Prooduction_2019_2020_2021)
# min(Kyntec_Prooduction_2019_2020_2021$ahh, na.rm = TRUE)
# sd(Kyntec_Prooduction_2019_2020_2021$ahh, na.rm = TRUE)

Kyntec_Prooduction_2019_2020_2021 %>% sd(ahh)


Summary_Kyntec_Prooduction_2019_2020_2021 <- Kyntec_Prooduction_2019_2020_2021 %>% 
  group_by(Unit, `AgroEcological Zone`, Crop, AEZ_code) %>% 
  summarise(mean_19_21 = mean(ahh,na.rm = TRUE),
            min_19_21 = min(ahh,na.rm = TRUE),
            max_19_21 = max(ahh, na.rm = TRUE),
            stdev_19_21 = sd(ahh, na.rm = TRUE))


Summary_Kyntec_Prooduction_2019_2020_2021 <- Summary_Kyntec_Prooduction_2019_2020_2021 %>% 
  mutate(for_join = paste0(AEZ_code, Crop, Unit))

###############################################################################

Kyntec_Prooduction <- Kyntec_Prooduction_all
Kyntec_Prooduction <- Kyntec_Prooduction %>% rename(ahh = Value) #something werid was happening ahh fixed it!

Kyntec_Prooduction <- ungroup(Kyntec_Prooduction)

Kyntec_Prooduction <- Kyntec_Prooduction %>% rename(ahh = Value) #something weird was happening ahh fixed it!


Summary_Kyntec_Prooduction <- Kyntec_Prooduction %>% 
  group_by(Unit, `AgroEcological Zone`, Crop, AEZ_code) %>% 
  summarise(mean_all = mean(ahh,na.rm = TRUE),
            min_all = min(ahh,na.rm = TRUE),
            max_all = max(ahh, na.rm = TRUE),
            stdev_all = sd(ahh, na.rm = TRUE))

Summary_Kyntec_Prooduction <- Summary_Kyntec_Prooduction %>% 
  mutate(for_join = paste0(AEZ_code, Crop, Unit))
################################################################################




use_this <- full_join(Summary_Kyntec_Prooduction, Summary_Kyntec_Prooduction_2019_2020_2021)


write.csv(use_this,
          "W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/production_data_AEZ.csv",)
