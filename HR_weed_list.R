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

HR_weed_list_long <- pivot_longer(HR_weed_list,
                                  cols = c(`3 corner Jack`:`Wireweed`),
                                  names_to = "weed",
                                  values_to="weed_class"
                                  )
################################################################################
###    what have the top weeds for each AEZ ########
################################################################################
unique(HR_weed_list$`GRDC AEZ`)
str(HR_weed_list_long)

HR_weed_list_long %>% filter %>% `GRDC AEZ` == "NSW NW Qld SW" %>% 
  filter %>% is.na(weed_class)

`NSW NW Qld SW`
