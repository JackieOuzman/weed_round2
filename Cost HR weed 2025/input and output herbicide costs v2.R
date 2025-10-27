## This scripts will collate and report on scenarios

library(tidyverse)
library(readxl)
library(dplyr)



####  Read in scenarios files ####

folder_path <- "W:/Economic impact of weeds round 2/model/9.HR_models"
file_path_baseline <- paste0(  folder_path, "/", 
  "Baseline/",
  "yield loss and expenditure Kynetec Herb dataV2 Cotton Mods.xlsx")

file_path_scenario_B <- paste0(  folder_path, "/", 
                               "Scenario B/",
                               "ScenarioBv2.xlsx")

sheet_names <- excel_sheets(file_path_baseline)
sheet_names



baseline_line_long <- read_excel(file_path_baseline, sheet = "long_data_to_compare")
ScenarioB_long <- read_excel(file_path_scenario_B, sheet = "long_data_to_compare")


####  Merge scenarios files ####

df <- rbind(baseline_line_long, ScenarioB_long)
rm(baseline_line_long, ScenarioB_long)

################################################################################
### Questions - what is the average cost change for fallow herbicides
### Baseline vs Sc B (we want to see a reduction in herbicide costs)
### Part A input costs
### Part B model output costs 
str(df)


df %>%  distinct(AEZ)
df <- df %>% 
  mutate(
    AEZ =  case_when(
      AEZ ==  "Total / National" ~ "Total",
      .default = AEZ))

df$AEZ <- factor(df$AEZ, levels = c("Northern", "Qld Central", "NSW NE/Qld SE", "NSW NW/Qld SW", "NSW Vic Slopes", "NSW Central", 
                                    "Southern", "SA Midnorth-Lower Yorke Eyre", "SA Vic Mallee", "SA Vic Bordertown-Wimmera", "Tas Grain", "Vic High Rainfall",
                                    "Western", "WA Central", "WA Eastern", "WA Northern", "WA Sandplain/ Mallee" ,
                                    "Total" ))
                 




list_of_variable <- df %>%  distinct(variable, .keep_all = TRUE) %>% select(variable, table)

herbicide_input_costs <- df %>% 
   filter(startsWith(variable, "Cost of herbicide :") 
              ) 



herbicide_input_costs

### Helper with plotting ###
herbicide_input_costs <- herbicide_input_costs %>% 
  mutate(variable_plot = str_remove_all(variable, 
                                          "\\s*:\\s*\\([^)]*\\)" )) %>% 
  mutate(variable_plot = str_remove_all(variable_plot,  "Cost of herbicide") )


herbicide_input_costs <- herbicide_input_costs %>%
  mutate(herbicide_type = case_when(
    str_detect(variable_plot, "(?i)knockdown") ~ "Knockdown",
    str_detect(variable_plot, "(?i)pre-emergent") ~ "Pre-emergent",
    str_detect(variable_plot, "(?i)post-emergent") ~ "Post-emergent",
    str_detect(variable_plot, "(?i)fallow") ~ "Fallow",
    TRUE ~ "Other"
  ))

herbicide_input_costs

#



###############################################################################
###  Fallow input costs  ######################################################
###############################################################################

Fallow_herb_cost_plot <- herbicide_input_costs %>% 
  filter(herbicide_type == "Fallow") %>% 
  #filter(!AEZ %in% c("Northern","Southern","Western", "Total")) %>% 
  filter(AEZ %in% c("Northern","Southern","Western", "Total")) %>% 
  ggplot(aes(x = AEZ, y = value, fill = scenario)) + 
  geom_col(position = "dodge") +
  facet_wrap(.~variable_plot, labeller = label_wrap_gen(width = 25)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Input herbicide cost (Chemical only $/ha)",
    #subtitle = "",
    x = "AEZ / Region",
    y = "$ per ha"#,
    #caption = ""
  )
Fallow_herb_cost_plot

##############################################################################
##1. how much have the costs decreased?


herbicide_wide_fallow <- herbicide_input_costs %>% 
  filter(herbicide_type == "Fallow") %>% 
  pivot_wider(
    names_from = scenario,
    values_from = value
  )
herbicide_wide_fallow
herbicide_wide_fallow <- herbicide_wide_fallow %>%
  mutate(pct_change = (ScenarioBv2 - Baseline) / Baseline * 100) %>% 
  select(AEZ, herbicide_type,Baseline, ScenarioBv2 , pct_change)
herbicide_wide_fallow

### Tas is very high what are the averages without Tas

write_csv(herbicide_wide_fallow, paste0(  folder_path, "/", "Scenario B/","check inputs/",
                   "ScB_herbicide_wide_fallow.csv"))

###############################################################################
###  Knockdown input costs  ######################################################
###############################################################################
Kockdown_herb_cost <- herbicide_input_costs %>%   filter(herbicide_type == "Knockdown")
Kockdown_herb_cost %>% distinct(variable_plot)

Kockdown_herb_cost_plot <- Kockdown_herb_cost %>% 
  filter(variable_plot %in% c( " Knockdown Cereal In model" , " Knockdown Broadleaf In model", 
                               " Knockdown Sorghum", " Knockdown Broadleaf In model"
                               )) %>% 
  filter(!AEZ %in% c("Northern","Southern","Western", "Total")) %>% 
  #filter(AEZ %in% c("Northern","Southern","Western", "Total")) %>% 
  ggplot(aes(x = AEZ, y = value, fill = scenario)) + 
  geom_col(position = "dodge") +
  facet_wrap(.~variable_plot, labeller = label_wrap_gen(width = 25)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Input herbicide cost (Chemical only $/ha)",
    #subtitle = "",
    x = "AEZ / Region",
    y = "$ per ha"#,
    #caption = ""
  )
Kockdown_herb_cost_plot


##############################################################################
##1. how much have the costs decreased?


herbicide_wide_knockdown <- Kockdown_herb_cost %>% 
  filter(herbicide_type == "Knockdown") %>% 
  pivot_wider(
    names_from = scenario,
    values_from = value
  )

herbicide_wide_knockdown

herbicide_wide_knockdown <- herbicide_wide_knockdown %>%
  mutate(pct_change = (ScenarioBv2 - Baseline) / Baseline * 100) %>% 
  select(AEZ, variable_plot,herbicide_type,Baseline, ScenarioBv2 , pct_change) %>% 
  filter( variable_plot %in% c(" Knockdown Cereal In model", " Knockdown Broadleaf In model"))
herbicide_wide_knockdown 

write_csv(herbicide_wide_knockdown, paste0(  folder_path, "/", "Scenario B/","check inputs/",
                                          "ScB_herbicide_wide_knockdown.csv"))

###############################################################################
###  Pre-emergent input costs  ######################################################
###############################################################################
Pre_emergent <- herbicide_input_costs %>%   filter(herbicide_type == "Pre-emergent")


Pre_emergent %>% distinct(variable_plot)

Pre_herb_cost_plot <- Pre_emergent %>% 
  #filter(!AEZ %in% c("Northern","Southern","Western", "Total")) %>% 
  filter(AEZ %in% c("Northern","Southern","Western", "Total")) %>% 
  ggplot(aes(x = AEZ, y = value, fill = scenario)) + 
  geom_col(position = "dodge") +
  facet_wrap(.~variable_plot, labeller = label_wrap_gen(width = 25)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Input herbicide cost (Chemical only $/ha)",
    #subtitle = "",
    x = "AEZ / Region",
    y = "$ per ha"#,
    #caption = ""
  )
Pre_herb_cost_plot


##############################################################################
##1. how much have the costs decreased?


herbicide_wide_pre <- Pre_emergent %>% 
  filter(herbicide_type == "Pre-emergent") %>% 
  pivot_wider(
    names_from = scenario,
    values_from = value
  )

herbicide_wide_pre

herbicide_wide_pre <- herbicide_wide_pre %>%
  mutate(pct_change = (ScenarioBv2 - Baseline) / Baseline * 100) %>% 
  select(AEZ, variable_plot,herbicide_type,Baseline, ScenarioBv2 , pct_change)# %>% 
  #filter( variable_plot %in% c(" Knockdown Cereal In model", " Knockdown Broadleaf In model"))
  herbicide_wide_pre 

  write_csv(herbicide_wide_pre, paste0(  folder_path, "/", "Scenario B/","check inputs/",
                                          "ScB_herbicide_wide_pre.csv"))

  ###############################################################################
  ###  Post-emergent input costs  ######################################################
  ###############################################################################
  Post_emergent <- herbicide_input_costs %>%   filter(herbicide_type == "Post-emergent")
  
  
  Post_emergent %>% distinct(variable_plot)
  
  Post_emergent_plot <- Post_emergent %>% 
    #filter(!AEZ %in% c("Northern","Southern","Western", "Total")) %>% 
    filter(AEZ %in% c("Northern","Southern","Western", "Total")) %>% 
    ggplot(aes(x = AEZ, y = value, fill = scenario)) + 
    geom_col(position = "dodge") +
    facet_wrap(.~variable_plot, labeller = label_wrap_gen(width = 25)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Input herbicide cost (Chemical only $/ha)",
      #subtitle = "",
      x = "AEZ / Region",
      y = "$ per ha"#,
      #caption = ""
    )
  Post_emergent_plot
  
  
  ##############################################################################
  ##1. how much have the costs decreased?
  herbicide_wide_post <- Post_emergent %>% 
    filter(herbicide_type == "Post-emergent") %>% 
    pivot_wider(
      names_from = scenario,
      values_from = value
    )
  
  herbicide_wide_post
  
  herbicide_wide_post <- herbicide_wide_post %>%
    mutate(pct_change = (ScenarioBv2 - Baseline) / Baseline * 100) %>% 
    select(AEZ, variable_plot,herbicide_type,Baseline, ScenarioBv2 , pct_change)# %>% 
  #filter( variable_plot %in% c(" Knockdown Cereal In model", " Knockdown Broadleaf In model"))
  herbicide_wide_post 
  
  write_csv(herbicide_wide_post, paste0(  folder_path, "/", "Scenario B/","check inputs/",
                                               "ScB_herbicide_wide_post.csv"))
  