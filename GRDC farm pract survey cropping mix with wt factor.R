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
## subset the data and apply weight factors to all the numeric clms
names(GRDC_crop_mix)
GRDC_crop_mix <- GRDC_crop_mix %>% select("Case ID" ,"State (Primary Location) (Location)", "AEZ",
                                          "Q1":"Q7",
                                          "Q9A":"Q9B",
                                          "Q10_01. Bread wheat":"TOTAL. Total Winter crop area",
                                          "Q10_Sunflower":"TOTAL. Total Spring/Summer crop area",
                                          "Q14. Long fallowed":"Q14_Total",
                                          "weight factor" 
                                          )

str(GRDC_crop_mix)
GRDC_crop_mix <- as.data.frame(GRDC_crop_mix)
names(GRDC_crop_mix)

## Recode the DK to NA
GRDC_crop_mix$`Q14. Long fallowed` <- as.double(GRDC_crop_mix$`Q14. Long fallowed`)
GRDC_crop_mix$`Q14. Short fallowed` <- as.double(GRDC_crop_mix$`Q14. Short fallowed`)
GRDC_crop_mix$`Q14_Total` <- as.double(GRDC_crop_mix$`Q14_Total`)

###############################################################################
##apply the weight factor


GRDC_crop_mix <- GRDC_crop_mix %>% mutate(
  Q3 =   (Q3*`weight factor`),
  Q4A =  (Q4A*`weight factor`) ,
  Q4B =  (Q4B*`weight factor`) ,
  Q6 =   (Q6*`weight factor`) ,
   `Q10_01. Bread wheat` = (`Q10_01. Bread wheat`*`weight factor`) ,
   `Q10_02. Durum wheat` = (`Q10_02. Durum wheat`*`weight factor`) ,
   `Q10_03. Feed barley (IF DK GRADE BEST GUESS)` = (`Q10_03. Feed barley (IF DK GRADE BEST GUESS)`*`weight factor`) ,
   `Q10_04. Malt barley (IF DK GRADE BEST GUESS)` = (`Q10_04. Malt barley (IF DK GRADE BEST GUESS)`*`weight factor`) ,
   `Q10_05. Oats` = (`Q10_05. Oats`*`weight factor`) ,
   `Q10_06. Triticale` = (`Q10_06. Triticale`*`weight factor`) ,
   `Q10_07. Cereal rye` = (`Q10_07. Cereal rye`*`weight factor`) ,
   `Q10_08. Canola` = (`Q10_08. Canola`*`weight factor`) ,
   `Q10_09. Mustard` = (`Q10_09. Mustard`*`weight factor`) ,
   `Q10_10. Linola` = (`Q10_10. Linola`*`weight factor`) ,
   `Q10_11. Chickpeas` = (`Q10_11. Chickpeas`*`weight factor`) ,
   `Q10_12. Field peas` = (`Q10_12. Field peas`*`weight factor`) ,
   `Q10_13. Lentils` = (`Q10_13. Lentils`*`weight factor`) ,
   `Q10_14. Lupins` = (`Q10_14. Lupins`*`weight factor`) ,
   `Q10_15. Faba beans` = (`Q10_15. Faba beans`*`weight factor`) ,
   `Q10_16. Vetch` = (`Q10_16. Vetch`*`weight factor`) ,
   `Q10_17. Crop intended to be or already green/brown manured´ (ploughed into soil or sprayed out prior to maturity for weed or disease management)` = (`Q10_17. Crop intended to be or already green/brown manured´ (ploughed into soil or sprayed out prior to maturity for weed or disease management)`*`weight factor`) ,
   `Q10_19. Other Winter crops` = (`Q10_19. Other Winter crops`*`weight factor`) ,
   `Q10_Sunflower` = (`Q10_Sunflower`*`weight factor` ),
   `Q10_Sorghum` = (`Q10_Sorghum`*`weight factor` ),
   `Q10_Corn or Maize` = (`Q10_Corn or Maize`*`weight factor`) ,
   `Q10_Soybeans` = (`Q10_Soybeans`*`weight factor`) ,
   `Q10_Mungbeans` = (`Q10_Mungbeans`*`weight factor`) ,
   `Q10_Cotton` = (`Q10_Cotton`*`weight factor`) ,
   `Q10_Rice` = (`Q10_Rice`*`weight factor`) ,
   `Q10_18. Crop intended to be or already green/brown manured´ (ploughed into soil or sprayed out prior to maturity for weed or disease management)` = (`Q10_18. Crop intended to be or already green/brown manured´ (ploughed into soil or sprayed out prior to maturity for weed or disease management)`*`weight factor` ),
   `Q10_27. Other Spring/ Summer crops` = (`Q10_27. Other Spring/ Summer crops`*`weight factor`) ,
   `TOTAL. Total Spring/Summer crop area` = (`TOTAL. Total Spring/Summer crop area`*`weight factor`) ,
   `Q14. Long fallowed` = (`Q14. Long fallowed`*`weight factor`) ,
   `Q14. Short fallowed` =(`Q14. Short fallowed`*`weight factor`) ,
   `Q14_Total` = (`Q14_Total`*`weight factor`)
 )


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
str(GRDC_crop_mix)

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



GRDC_crop_mix <- GRDC_crop_mix %>% rename(winter_crop_area = Q4A,
                                          summer_crop_area = Q4B)


GRDC_crop_mix <- GRDC_crop_mix %>% mutate(winter_crop_area_ha = winter_crop_area * convert_ha,
                                          summer_crop_area_ha = summer_crop_area * convert_ha )


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
#replace the na with zeros
winter_crops <- winter_crops %>% replace(is.na(.), 0)

winter_crops <- winter_crops %>% 
  mutate(Wheat              = (Bread_wheat+Durum_wheat)) %>% 
  mutate(Barley             = (Feed_barley+Malt_barley)) %>% 
  mutate(Pulses             = (Chickpeas+Faba_beans+Field_peas+Lupins+Lentils)) %>% 
  mutate(Other_winter_merge = (Triticale+Cereal_rye+Mustard+Linola+Vetch+Other_winter))

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
#replace the na with zeros
summer_crops <- summer_crops %>% replace(is.na(.), 0)

summer_crops <- summer_crops %>% 
  mutate(Other_summer_merge= (Sunflower+Corn_Maize+Soybeans+Mungbeans+Rice+Other_summer))  
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

names(GRDC_crop_mix)

crop_type_area <- GRDC_crop_mix %>%  select( `Case ID`,
                                             AEZ,
                                             
                                   convert_ha,
                                   winter_crop_area_ha , summer_crop_area_ha,
                                   crop_season  , 
                                   total_farm_area, double_cropped, double_cropped_ha, 
                                   pasture_veg_plan_cropped_ha,potential_crop_ha )

names(crop_type_area)


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

names(crop_type_area)

crop_type_area <- crop_type_area %>% 
  mutate(winter_summer_crop_area_ha = winter_crop_area_ha+summer_crop_area_ha )
crop_type_area <- crop_type_area %>% 
  mutate(winter_summer_crop_double_area_ha = winter_crop_area_ha+summer_crop_area_ha+double_cropped_ha )

crop_type_area <- crop_type_area %>% select(
  `Case ID` ,
  AEZ,
  crop_season,
  double_cropped,
  winter_crop_area_ha,
  summer_crop_area_ha,
  double_cropped_ha,
  total_farm_area,
  potential_crop_ha:winter_summer_crop_double_area_ha)
  
str(crop_type_area)




###############################################################################
write.csv(crop_type_area, 
          "W:/Economic impact of weeds round 2/GRDC_farm_practice_survey/Jackie_working/crop_type_area_with_wts.csv",  
          row.names = FALSE)
#################################################################################
rm(fallow, summer_crops, winter_crops)
################################################################################
 
unique(crop_type_area$AEZ)
str(crop_type_area)
dim(crop_type_area)
#### before I make a summary I need to change 0 back to na

crop_type_area <- na_if(crop_type_area, 0)
names(crop_type_area)       

################################################################################
unique(crop_type_area$AEZ)
###############################################################################
## add in national

crop_type_area_national <- crop_type_area
crop_type_area_national <- crop_type_area_national %>% mutate(AEZ = "national")

crop_type_area <- rbind(crop_type_area,crop_type_area_national )


summary_crop_area_mix <- crop_type_area %>% group_by(AEZ) %>% 
  summarise(
    mean_farm_area              = mean(total_farm_area, na.rm = TRUE),
    mean_potential_crop_ha      = mean(potential_crop_ha, na.rm = TRUE),
    mean_winter_summer_crop_area_ha             = mean(winter_summer_crop_area_ha, na.rm = TRUE),
    mean_winter_summer_crop_double_area_ha      = mean(winter_summer_crop_double_area_ha, na.rm = TRUE),
    
    mean_Wheat_ha               = mean(Wheat_ha, na.rm = TRUE),
    mean_Barley_ha              = mean(Barley_ha , na.rm = TRUE),
    mean_Oats_ha                = mean(Oats_ha , na.rm = TRUE),
    mean_Canola_ha              = mean(Canola_ha , na.rm = TRUE),
    mean_Pulses_ha              = mean(Pulses_ha  , na.rm = TRUE),
    mean_manured_winter_crop_ha = mean(manured_crop_ha  , na.rm = TRUE),
    mean_Total_winter_ha        = mean(Total_winter_ha  , na.rm = TRUE),
    
    mean_Sorghum_ha              = mean(Sorghum_ha   , na.rm = TRUE),
    mean_Cotton_ha               = mean(Cotton_ha   , na.rm = TRUE),
    mean_manured_crop_summer_ha  = mean(manured_crop_summer_ha, na.rm = TRUE),
    mean_Total_summer_ha         = mean(Total_summer_ha, na.rm = TRUE),
    
    mean_Short_fallowed_ha         = mean(Short_fallowed_ha, na.rm = TRUE),
    
  )

write.csv(summary_crop_area_mix, 
          "W:/Economic impact of weeds round 2/GRDC_farm_practice_survey/Jackie_working/summary_crop_area_mix_with_wts.csv",  
          row.names = FALSE)



summary_crop_area_mix_sum <- crop_type_area %>% group_by(AEZ) %>% 
  summarise(
    sum_farm_area              = sum(total_farm_area, na.rm = TRUE),
    sum_potential_crop_ha      = sum(potential_crop_ha, na.rm = TRUE),
    sum_winter_summer_crop_area_ha             = sum(winter_summer_crop_area_ha, na.rm = TRUE),
    sum_winter_summer_crop_double_area_ha      = sum(winter_summer_crop_double_area_ha, na.rm = TRUE),
    
    sum_Wheat_ha               = sum(Wheat_ha, na.rm = TRUE),
    sum_Barley_ha              = sum(Barley_ha , na.rm = TRUE),
    sum_Oats_ha                = sum(Oats_ha , na.rm = TRUE),
    sum_Canola_ha              = sum(Canola_ha , na.rm = TRUE),
    sum_Pulses_ha              = sum(Pulses_ha  , na.rm = TRUE),
    sum_manured_winter_crop_ha = sum(manured_crop_ha  , na.rm = TRUE),
    sum_Total_winter_ha        = sum(Total_winter_ha  , na.rm = TRUE),
    
    sum_Sorghum_ha              = sum(Sorghum_ha   , na.rm = TRUE),
    sum_Cotton_ha               = sum(Cotton_ha   , na.rm = TRUE),
    sum_manured_crop_summer_ha  = sum(manured_crop_summer_ha, na.rm = TRUE),
    sum_Total_summer_ha         = sum(Total_summer_ha, na.rm = TRUE),
    
    sum_Short_fallowed_ha         = sum(Short_fallowed_ha, na.rm = TRUE),
    count = n()
    
  )




write.csv(summary_crop_area_mix_sum, 
          "W:/Economic impact of weeds round 2/GRDC_farm_practice_survey/Jackie_working/summary_crop_area_mix_sum_with_wts.csv",  
          row.names = FALSE)



################################################################################
### Not really sure how best to get the av crop area but still get the cropping land to equal the sum of all the crops

#### The GRDC report groups the crops and reports on the average % of crop land for the crops.
### I cant use this because I need more detail eg cotton alone.
### but I could use this approach 
### first sum all the area of each crop type per AEZ as per df = summary_crop_area_mix_sum
### then work out the % of land sown to this crop type.
### then use the avearge cropping land to work out the ha - this is quite good becasue I can fiddle with the average and the % won't change.
################################################################################

## get % of crop type 

str(summary_crop_area_mix_sum)

summary_crop_area_mix_percent <- summary_crop_area_mix_sum %>% 
  mutate(percent_wheat =   (sum_Wheat_ha / sum_winter_summer_crop_area_ha)*100,
         percent_Barley =  (sum_Barley_ha / sum_winter_summer_crop_area_ha)*100,
         percent_Oats =    (sum_Oats_ha / sum_winter_summer_crop_area_ha)*100,
         percent_Canola = (sum_Canola_ha / sum_winter_summer_crop_area_ha)*100,
         percent_Pulses = (sum_Pulses_ha / sum_winter_summer_crop_area_ha)*100,
         percent_manured_winter = (sum_manured_winter_crop_ha/ sum_winter_summer_crop_area_ha)*100,
         percent_Sorghum = (sum_Sorghum_ha / sum_winter_summer_crop_area_ha)*100,
         percent_Cotton = (sum_Cotton_ha  / sum_winter_summer_crop_area_ha)*100,
         percent_manured_summer = (sum_manured_crop_summer_ha / sum_winter_summer_crop_area_ha)*100
         )

names(summary_crop_area_mix_percent)
summary_crop_area_mix_percent <- summary_crop_area_mix_percent %>%  select(AEZ,
                         percent_wheat:percent_manured_summer)

### add in the average
str(summary_crop_area_mix)
crop_area_av_AEZ_subset <- summary_crop_area_mix %>%  select(AEZ, mean_winter_summer_crop_area_ha)
str(summary_crop_area_mix_percent)

summary_crop_area_mix_percent <- left_join(crop_area_av_AEZ_subset,summary_crop_area_mix_percent )

##### cal the ha based on av cropping land and the percenatage grown in each AEZ 

Crop_area_final <- summary_crop_area_mix_percent %>% 
  mutate(
         Wheat = mean_winter_summer_crop_area_ha*(percent_wheat/100),
         Barley = mean_winter_summer_crop_area_ha*(percent_Barley/100),
         Oats = mean_winter_summer_crop_area_ha*(percent_Oats/100),
         Canola = mean_winter_summer_crop_area_ha*(percent_Canola/100),
         Pulses = mean_winter_summer_crop_area_ha*(percent_Pulses/100),
         Manured_winter = mean_winter_summer_crop_area_ha*(percent_manured_winter/100),
         Sorghum = mean_winter_summer_crop_area_ha*(percent_Sorghum/100),
         Cotton = mean_winter_summer_crop_area_ha*(percent_Cotton/100),
         Manured_summer = mean_winter_summer_crop_area_ha*(percent_manured_summer/100))
         
Crop_area_final <- Crop_area_final %>% select(AEZ,mean_winter_summer_crop_area_ha, Wheat:Manured_summer)




write.csv(Crop_area_final, 
          "W:/Economic impact of weeds round 2/GRDC_farm_practice_survey/Jackie_working/Crop_area_final_with_wts.csv",  
          row.names = FALSE)



#################################################################################
### report on the weight factors per AEZ

names(GRDC_crop_mix)
unique(GRDC_crop_mix$`weight factor`)
wt_factor <- GRDC_crop_mix %>% group_by(`State (Primary Location) (Location)`) %>% 
  summarise(min_wt_factor = min(`weight factor`, na.rm=TRUE),
            max_wt_factor = max(`weight factor`, na.rm=TRUE ))
write.csv(wt_factor, 
          "W:/Economic impact of weeds round 2/GRDC_farm_practice_survey/Jackie_working/wt_factor_by_state.csv",  
          row.names = FALSE)
