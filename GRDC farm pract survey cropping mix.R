library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

GRDC_crop_mix <-  read_excel(path =
                               "W:/Economic impact of weeds round 2/GRDC_farm_practice_survey/raw_data/2021 FPS data for GRDC.xlsx",
                               sheet ="142 GRDC Farm Practices 2021")

################################################################################
### code farm type as mixed or grain only
# Q1. On the land you manage, do you ... multiple response possible
# Grow grain crops ----------------------------------------------------------------------------- 1
# Run cattle ------------------------------------------------------------------------------------- 2
# Run sheep ------------------------------------------------------------------------------------ 3

str(GRDC_crop_mix)
unique(GRDC_crop_mix$Q1)

GRDC_crop_mix <- GRDC_crop_mix %>% mutate(farm_type = case_when(
  Q1 == "1"~ "grain",
  TRUE     ~ "mixed"
  
))

unique(GRDC_crop_mix$farm_type)

################################################################################
################################################################################
# Q2. Most of the survey questions ask about area. Do you prefer to use hectares or acres?
# Hectares -------------------------------------------------------------------------------------- 1
# Acres ------------------------------------------------------------------------------------------ 2

unique(GRDC_crop_mix$Q2)
GRDC_crop_mix <- GRDC_crop_mix %>% mutate(convert_ha = case_when(
  Q2 == "1"~ 1,
  TRUE     ~ 0.404686
))
unique(GRDC_crop_mix$convert_ha)


################################################################################
################################################################################
#Q3. Can you tell me your total farm area?

str(GRDC_crop_mix$Q3)
GRDC_crop_mix <- GRDC_crop_mix %>% mutate(total_farm_area = Q3 * convert_ha)


################################################################################
################################################################################

#Q4. A) How many (hectares/acres) of winter crop have you sown in 2021? Please include any doublecropped area
#B)How many (hectares/acres) of spring or summer crop did you plant in the 2020-21 season.

str(GRDC_crop_mix$Q4A)
str(GRDC_crop_mix$Q4B)

GRDC_crop_mix <- GRDC_crop_mix %>% mutate(crop_season = case_when(
  Q4A > 0 & Q4B == 0 ~ "winter_only",
  Q4A > 0 & Q4B > 0 ~  "winter_summer",
  Q4A == 0 & Q4B > 0 ~  "summer_only",
  TRUE     ~ "check"
))
names(GRDC_crop_mix)
unique(GRDC_crop_mix$crop_season)

# Q5. Has any crop area been double cropped during 2021?
#   Yes ........................................................................................................ 1 continue
# No ......................................................................................................... 2 go to Q7
# Q6. How many (hectares/acres) have been double cropped?


GRDC_crop_mix <- GRDC_crop_mix %>% mutate(double_cropped = case_when(
  Q5  == 1 ~ "double_cropped",
  Q5  == 2 ~ "single_cropped",
  TRUE     ~ "check"
))
str(GRDC_crop_mix$Q6)
GRDC_crop_mix <- GRDC_crop_mix %>% mutate(double_cropped_ha = Q6*convert_ha)


#Q7. How many (hectares/acres) of your land is under pasture or under a permanent vegetation plan in 2021?
GRDC_crop_mix <- GRDC_crop_mix %>% mutate(pasture_veg_plan_cropped_ha = Q7*convert_ha)

GRDC_crop_mix <- GRDC_crop_mix %>% mutate(potential_crop_ha = total_farm_area -pasture_veg_plan_cropped_ha)

########################################################################################################
#### Crop Type Winter

# Q9. A) What winter crops have you sown in 2021? (do not read, record in grid)
# Winter crops:
#   Bread wheat -------------------------------------------------------------------------------------------------- 1
# Durum wheat ------------------------------------------------------------------------------------------------- 2
# Feed barley (respondent may not know the grade of barley they will achieve, but this may be planned for) ----- 3
# Malt barley (respondent may not know the grade of barley they will achieve, but this may be planned for) ----- 4
# Oats ---------------------------------------------------------------------------------------------------------- 5
# Triticale (pronounced tri – ti – carly) ----------------------------------------------------------------------- 6
# Cereal rye ---------------------------------------------------------------------------------------------------- 7
# Canola -------------------------------------------------------------------------------------------------------- 8
# Mustard ------------------------------------------------------------------------------------------------------ 9
# Linola ------------------------------------------------------------------------------------------------------- 10
# Chickpeas ---------------------------------------------------------------------------------------------------- 11
# Field peas --------------------------------------------------------------------------------------------------- 12
# Lentils ------------------------------------------------------------------------------------------------------ 13
# Lupins ------------------------------------------------------------------------------------------------------- 14
# Faba beans -------------------------------------------------------------------------------------------------- 15
# Vetch -------------------------------------------------------------------------------------------------------- 16
# Other winter crops ------------------------------------------------------------------------------------------ 19
# Crop intended to be or already ‘green manured’ (ploughed into soil prior to maturity for weed or disease management)- 17
# Crop intended to be or already ‘brown manured’ (sprayed out prior to maturity for weed or disease
#                                                 management)--------------------------------------------------- 18
names(GRDC_crop_mix)
unique(GRDC_crop_mix$Q9A)
winter_crop <- GRDC_crop_mix %>% select( `Case ID`,Q9A) 


test_winter_crops <- winter_crop %>%  
  mutate(Bread_wheat = case_when(grepl("01", Q9A) ~ "Bread_wheat")) %>% 
  mutate(Durum_wheat = case_when(grepl("02", Q9A) ~ "Durum_wheat")) %>%                       
  mutate(Feed_barley = case_when(grepl("03", Q9A) ~ "Feed_barley")) %>% 
  mutate(Malt_barley = case_when(grepl("04", Q9A) ~ "Malt_barley")) %>% 
  mutate(Oats = case_when(grepl       ("05", Q9A) ~ "Oats")) %>% 
  mutate(Triticale = case_when(grepl  ("06", Q9A) ~ "Triticale")) %>% 
  mutate(Cereal_rye = case_when(grepl ("07", Q9A) ~ "Cereal_rye")) %>%
  mutate(Canola = case_when(grepl     ("08", Q9A) ~ "Canola")) %>% 
  mutate(Mustard = case_when(grepl    ("09", Q9A) ~ "Mustard")) %>% 
  mutate(Linola = case_when(grepl     ("10", Q9A) ~ "Linola")) %>%
  mutate(Chickpeas = case_when(grepl  ("11", Q9A) ~ "Chickpeas")) %>%
  mutate(Field_peas = case_when(grepl ("12", Q9A) ~ "Field_peas")) %>%
  mutate(Lentils = case_when(grepl    ("13", Q9A) ~ "Lentils")) %>%
  mutate(Lupins = case_when(grepl     ("14", Q9A) ~ "Lupins")) %>%
  mutate(Faba_beans = case_when(grepl ("15", Q9A) ~ "Faba_beans")) %>%
  mutate(Vetch = case_when(grepl      ("16", Q9A) ~ "Vetch")) %>%
  mutate(Other_winter_crops = case_when(grepl("19", Q9A) ~ "Other_winter_crops")) %>%
  mutate(green_manured = case_when(grepl("17", Q9A) ~ "green_manured")) %>%
  mutate(brown_manured = case_when(grepl("18", Q9A) ~ "brown_manured"))
  
### ummm not sure I need the above code

test_winter_crops_v2 <- GRDC_crop_mix %>% select(`Case ID`, `Q10_01. Bread wheat`:`TOTAL. Total Winter crop area` )


### up to here ####



#################################################################################
test <- GRDC_crop_mix %>%  select( `Case ID`, 
                                   Q4A, Q4B, 
                                   crop_season  , 
                                   total_farm_area, double_cropped, double_cropped_ha, 
                                   pasture_veg_plan_cropped_ha,potential_crop_ha )
