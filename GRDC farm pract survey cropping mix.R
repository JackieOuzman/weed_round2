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
winter_crops <- GRDC_crop_mix %>% select(`Case ID`, `Q10_01. Bread wheat`:`TOTAL. Total Winter crop area` )


winter_crops <- winter_crops %>% 
  rename(
    Bread_wheat  = "Q10_01. Bread wheat",                                                                                                                            
    Durum_wheat  ="Q10_02. Durum wheat" ,                                                                                                                           
    Feed_barley   = "Q10_03. Feed barley (IF DK GRADE BEST GUESS)" ,                                                                                                   
    Malt_barley   ="Q10_04. Malt barley (IF DK GRADE BEST GUESS)" ,                                                                                                   
    Oats          ="Q10_05. Oats" ,                                                                                                                                   
    Triticale     ="Q10_06. Triticale" ,                                                                                                                              
    Cereal_rye    ="Q10_07. Cereal rye" ,                                                                                                                             
    Canola        ="Q10_08. Canola"   ,                                                                                                                               
    Mustard       ="Q10_09. Mustard" ,                                                                                                                                
    Linola        ="Q10_10. Linola"  ,                                                                                                                                
    Chickpeas     ="Q10_11. Chickpeas" ,                                                                                                                              
    Field_peas    ="Q10_12. Field peas" ,                                                                                                                             
    Lentils       ="Q10_13. Lentils" ,                                                                                                                                
    Lupins        ="Q10_14. Lupins" ,                                                                                                                                 
    Faba_beans    ="Q10_15. Faba beans"  ,                                                                                                                            
    Vetch         ="Q10_16. Vetch" ,                                                                                                                                  
    manured_crop  ="Q10_17. Crop intended to be or already green/brown manured´ (ploughed into soil or sprayed out prior to maturity for weed or disease management)",
    Other_winter  ="Q10_19. Other Winter crops"   ,                                                                                                                   
    Total_winter  = "TOTAL. Total Winter crop area"    
  )
names(winter_crops)

winter_crops <- winter_crops %>% 
  mutate(Wheat              = Bread_wheat+Durum_wheat) %>% 
  mutate(Barley             = Feed_barley+Malt_barley) %>% 
  mutate(Pulses             = Chickpeas+Faba_beans+Field_peas+Lupins+Lentils) %>% 
  mutate(Other_winter_merge = Triticale+Cereal_rye+Mustard+Linola+Vetch+Other_winter)

winter_crops <- winter_crops %>% select(
  -Bread_wheat,-Durum_wheat,
  -Feed_barley, -Malt_barley,
  -Chickpeas, -Faba_beans, -Field_peas, -Lupins, -Lentils,
  -Triticale, -Cereal_rye, -Mustard, -Linola, -Vetch, -Other_winter)


#######################################################################################################
#### Crop Type Summer
# 9B)What spring or summer crops did you plant in the 2020-21 season
# Summer crops:
#   Sunflower ---------------------------------------------------------------------------------------------------- 20
# Sorghum ----------------------------------------------------------------------------------------------------- 21
# Corn or Maize ------------------------------------------------------------------------------------------------ 22
# Soybeans ---------------------------------------------------------------------------------------------------- 23
# Mungbeans --------------------------------------------------------------------------------------------------- 24
# Cotton -------------------------------------------------------------------------------------------------------- 25
# Rice ----------------------------------------------------------------------------------------------------------- 26
# Other summer crops ---------------------------------------------------------------------------------------- 27
# Crop intended to be or already ‘green manured’ (ploughed into soil prior to maturity for weed ordisease management) -------------------------------------------------------------------------------------- 17
# Crop intended to be or already ‘brown manured’ (sprayed out prior to maturity for weed or disease
#                                                 management) -----------------------------------------------------------------------------------------------

names(GRDC_crop_mix)
summer_crops <- GRDC_crop_mix %>% select(`Case ID`, `Q10_Sunflower`:`TOTAL. Total Spring/Summer crop area` )
names(summer_crops)

summer_crops <- summer_crops %>% 
  rename(
    Sunflower          = "Q10_Sunflower",                                                                                                                            
    Sorghum            ="Q10_Sorghum" ,                                                                                                                           
    Corn_Maize         = "Q10_Corn or Maize" ,                                                                                                   
    Soybeans           ="Q10_Soybeans" ,                                                                                                   
    Mungbeans          ="Q10_Mungbeans" ,                                                                                                                                   
    Cotton             ="Q10_Cotton" ,                                                                                                                              
    Rice               ="Q10_Rice" ,                                                                                                                             
    manured_crop        ="Q10_18. Crop intended to be or already green/brown manured´ (ploughed into soil or sprayed out prior to maturity for weed or disease management)"   ,                                                                                                                               
    Other_summer       ="Q10_27. Other Spring/ Summer crops" ,                                                                                                                                
    Total_summer       = "TOTAL. Total Spring/Summer crop area"    
  )
names(summer_crops)


summer_crops <- summer_crops %>% 
  mutate(Other_summer_merge= Sunflower+Corn_Maize+Soybeans+Mungbeans+Rice+Other_summer)  
summer_crops <- summer_crops %>% select(
  -Sunflower,
  -Corn_Maize,
  -Soybeans,
  -Mungbeans,
  -Rice,
  -Other_summer)



#######################################################################################################
#### Fallow
# Q14. Over the past 12 months, how many (hectares/acres) have been … read out? (MR)
# A.Long fallowed_________
# B.Short fallowed_________

names(GRDC_crop_mix)
fallow <- GRDC_crop_mix %>% select(`Case ID`, `Q14. Long fallowed`:`Q14_Total` )
names(fallow)

fallow <- fallow %>% 
  rename(
    Long_fallowed          = "Q14. Long fallowed",                                                                                                                            
    Short_fallowed         = "Q14. Short fallowed" ,                                                                                                                           
    Total_fallow           = "Q14_Total"   
  )
names(fallow)





#################################################################################
crop_type_area <- GRDC_crop_mix %>%  select( `Case ID`,
                                   convert_ha,
                                   Q4A, Q4B, 
                                   crop_season  , 
                                   total_farm_area, double_cropped, double_cropped_ha, 
                                   pasture_veg_plan_cropped_ha,potential_crop_ha )

names(crop_type_area)
crop_type_area <- crop_type_area %>% 
  rename(winter_crop = Q4A, 
         summer_crop = Q4B)
crop_type_area <- crop_type_area %>% 
  mutate(winter_crop_ha = winter_crop*convert_ha, 
         summer_crop_ha = summer_crop*convert_ha)

crop_type_area <- crop_type_area %>% select(-winter_crop, -summer_crop)


###############################################################################
##add the winter crops and convert to ha

crop_type_area <- left_join(crop_type_area, winter_crops)
names(crop_type_area)
crop_type_area <- crop_type_area %>% 
  mutate(Wheat_ha = Wheat*convert_ha, 
         Barley_ha = Barley*convert_ha,
         Oats_ha = Oats*convert_ha,
         Canola_ha = Canola*convert_ha,
         Pulses_ha = Pulses*convert_ha,
         manured_crop_ha = manured_crop*convert_ha,
         Other_winter_merge_ha = Other_winter_merge*convert_ha,
         Total_winter_ha = Total_winter*convert_ha)
         
         
crop_type_area <- crop_type_area %>% select(
  -Wheat,
  -Barley,
  -Oats,
  -Canola,
  -Pulses,
  -manured_crop,
  -Other_winter_merge,
  -Total_winter
  )

         
###############################################################################
##add the summer crops and convert to ha

crop_type_area <- left_join(crop_type_area, summer_crops)
names(crop_type_area)
crop_type_area <- crop_type_area %>% 
  mutate(Sorghum_ha = Sorghum*convert_ha, 
         Cotton_ha = Cotton*convert_ha,
         manured_crop_summer_ha = manured_crop*convert_ha,
         Other_summer_merge_ha = Other_summer_merge*convert_ha,
         Total_summer_ha = Total_summer*convert_ha,)


crop_type_area <- crop_type_area %>% select(
  -Sorghum,
  -Cotton,
  -manured_crop,
  -Other_summer_merge,
  -Total_summer
)


###############################################################################
##add the fallow and convert to ha

crop_type_area <- left_join(crop_type_area, fallow)
names(crop_type_area)
str(crop_type_area)

crop_type_area$Long_fallowed <- as.double(crop_type_area$Long_fallowed) #DK is in clm - recoded here to NA
crop_type_area$Short_fallowed <- as.double(crop_type_area$Short_fallowed)


crop_type_area <- crop_type_area %>% 
  mutate(Short_fallowed_ha = Short_fallowed*convert_ha, 
         Long_fallowed_ha = Long_fallowed*convert_ha,
         Total_fallow_ha = Total_fallow*convert_ha)


crop_type_area <- crop_type_area %>% select(
  -Short_fallowed,
  -Long_fallowed,
  -Total_fallow
)

