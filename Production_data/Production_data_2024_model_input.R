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
################################################################################
H_2024_AEZ <-  read_excel(path =
                              "W:/Economic impact of weeds round 2/production_data/1.Input_data_2024_model/23.09.0014 GRDC AEZ Data with Legumes_jax.xlsx",
                            sheet ="H Data", skip = 4)

str(H_2024_AEZ)
################################################################################
str(H_2024_AEZ)
str(yld_2024_AEZ_long)
Kyntec_Prooduction_all <- rbind(H_2024_AEZ, yld_2024_AEZ_long)

###############################################################################
Kyntec_Prooduction_all <- Kyntec_Prooduction_all %>% filter(`AgroEcological Zone` != "Excluded")
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
