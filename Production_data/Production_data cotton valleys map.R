library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(DT)

library(lubridate)


# Central Qld
# Darling Downs
# St George-Dirranbandi
# MacIntyre-Mungindi
# Gwydir
# Bourke & Walgett
# Namoi Lower
# Namoi Upper
# Macquarie Valley
# Southern NSW


################################################################################
Central_Qld <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/Central_Qld.csv"
  ) %>%
  mutate(Cotton_valley = "Central_Qld")
Darling_Downs <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/Darling_Downs.csv"
  ) %>%
  mutate(Cotton_valley = "Darling_Downs")
St_George_Dirranbandi <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/St_George-Dirranbandi.csv"
  ) %>%
  mutate(Cotton_valley = "St_George_Dirranbandi")
  
MacIntyre_Mungindi <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/MacIntyre-Mungindi.csv"
  ) %>%
  mutate(Cotton_valley = "MacIntyre_Mungindi")

Gwydir <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/Gwydir.csv"
  ) %>%
  mutate(Cotton_valley = "Gwydir")

Bourke_Walgett <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/Bourke_&_Walgett.csv"
  ) %>%
  mutate(Cotton_valley = "Bourke_Walgett")

Namoi_Lower <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/SA2_Namoi_Lower.csv"
  ) %>% mutate(Cotton_valley = "Namoi_Lower")

Namoi_Upper <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/SA2_Namoi_Upper.csv"
  )%>%
  mutate(Cotton_valley = "Namoi_Upper")

Macquarie_Valley <-
  read.csv(
    "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/Macquarie_Valley.csv"
  ) %>%
  mutate(Cotton_valley = "Macquarie_Valley")

Southern_NSW <-  read.csv("W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/Southern_NSW.csv") %>% mutate(Cotton_valley = "Southern_NSW")

################################################################################

Cotton_valleys_SA2 <- rbind(Central_Qld,
                            Darling_Downs,
                            St_George_Dirranbandi,
                            MacIntyre_Mungindi,
                            Gwydir,
                            Bourke_Walgett,
                            Namoi_Lower,
                            Namoi_Upper,
                            Macquarie_Valley,
                            Southern_NSW)
names(Cotton_valleys_SA2)
Cotton_valleys_SA2 <- Cotton_valleys_SA2 %>% 
  select(FID,
         SA2_21PPID,
         SA2_21PID,
         SA2_21NAME,
         SA4_21NAME,
         ST_ABBREV,
         Cotton_valley)
write.csv(Cotton_valleys_SA2, "W:/Economic impact of weeds round 2/Spatial/location of cotton regions/SA2_est_Kynetec_map/SA2_cotton_valley_neat.csv",
row.names = FALSE)
