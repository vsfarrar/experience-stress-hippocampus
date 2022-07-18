#Experience-Stress-Hippocampus MS
#DEX Validation Experiement Analysis
#Methods Section

setwd(dir = rstudioapi::selectDirectory())
source("exp-stress-hipp_setup.R")

# SAMPLE SIZE ####
dex_val %>% 
  filter(group != "0.5 mg/kg" & timepoint != "30") %>% #0.5 mg/kg dose and 30m timepoint not tested across all groups, exclude. 
  count(group, timepoint)

#STATS ####

dex_val$timepoint <- as.factor(dex_val$timepoint)

#differences after 60 minutes
dex_val %>% filter(group != "0.5 mg/kg" & timepoint == "60") %>%
  lm(cort ~ group, data = .) %>% summary(.)

#differences after 90 minutes
dex_val %>% filter(group != "0.5 mg/kg" & timepoint == "90") %>%
  lm(cort ~ group, data = .) %>% summary(.)

#differences across vehicle
dex_val %>% filter(group == "vehicle" & timepoint != "30") %>% 
  lm(cort ~ timepoint, data = .) %>% summary(.)

dex_val %>% filter(group == "1 mg/kg" & timepoint != "30") %>% 
  lm(cort ~ timepoint, data = .) %>% summary(.)

dex_val %>% filter(group == "2 mg/kg") %>%
  lm(cort ~ timepoint, data = .) %>% summary(.)


