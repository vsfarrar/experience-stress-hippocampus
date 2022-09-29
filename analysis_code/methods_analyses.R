#Experience-Stress-Hippocampus MS
#Methods Section Analyses

setwd(dir = rstudioapi::selectDirectory())
source("exp-stress-hipp_setup.R")

# EXPERIMENT 1: Hormones ####

# Sample Size ####
horm_blood %>%
  distinct(bird_id, .keep_all = T) %>%
  count(experience_chicks, sex)

# Age and Next Nest Differences ####

#is there an age difference in experienced vs inexperienced birds?
horm_blood %>%
  distinct(bird_id, .keep_all = T) %>% #select 1 row per bird, to avoid pseudoreplication
  t.test(age_when_bled ~ experience_chicks, data = .) 
      #sig.effect, experienced older by ~174.26 days

#does recency of last nest differ between inexp. and exp.?
horm_blood %>%
  distinct(bird_id, .keep_all = T) %>% 
  t.test(days_since_last_nest_ifnest ~ experience_chicks, data = .) 
  #non-sig., experienced slightly more recent but not significant.

#does time to initiation of next nest differ between inexp. and exp.?
horm_blood %>%
  distinct(bird_id, .keep_all = T) %>% 
  t.test(days_until_next_nest ~ experience_chicks, data = .) 
  #sig.effect, experienced had new nests sooner

#Time to Blood and Time of Day Effects ####

#plot of hormones versus time of day collected
ggplot(horm_blood,aes(x = time_bled, y = cort_conc, color = stage)) + 
  geom_point() + geom_label(aes(label = hr_bled)) + 
  geom_smooth(aes(group = 1), method = "lm")


#differences in time to baseline (0m) sample? 

horm_blood %>% filter(timepoint == 0) %>%
aov(cort_conc ~ sec_to_baseline, data = .) %>%
  summary(.)

horm_blood %>% filter(timepoint == 0) %>%
  aov(prl_conc ~ sec_to_baseline, data = .) %>%
  summary(.)

#mean time to baseline sample 

horm_blood %>% filter(timepoint == 0) %>%
  summarise(mean(sec_to_baseline, na.rm = T), 
            sd(sec_to_baseline, na.rm = T))

#effect of time of day on hormones 
cor.test(horm_blood$cort_conc, horm_blood$time_bled)

summary(aov(cort_conc ~ time_bled, data = horm_blood)) 
summary(aov(prl_conc ~ time_bled, data = horm_blood)) 

#effect of date (time of year) on hormones
horm_blood$date_bled <- as.Date(horm_blood$d_bled, "%m/%d/%y")

summary(aov(cort_conc ~ date_bled, data = horm_blood)) 
summary(aov(prl_conc ~ date_bled, data = horm_blood)) 

# EXPERIMENT 2: Hippocampus Gene Exp. ####
#Methods Analyses 

#Sample Size ####
hipp_data %>% 
  filter(gene == "gr") %>%
  count(experience_chicks, sex)

# Avg. Number of Chicks Raised ####

#average number of chicks raised in experienced group
hipp_data %>% 
  filter(gene == "gr" & experience_chicks == 1) %>%
  summarise(mean(total_chicks), 
            sd(total_chicks))

# Effects of Time Since Last Nest ####

#days since last nest between groups
hipp_data %>% 
  filter(gene == "gr" & days_since_last_nest <150) %>% 
  group_by(experience_chicks) %>%
  summarise(mean(days_since_last_nest, na.rm = T),
            std.error(days_since_last_nest, na.rm = T))

hipp_data %>% 
  filter(days_since_last_nest <150) %>%
  t.test(.$days_since_last_nest ~ .$experience_chicks, data = .)

# Effects of Age ####

hipp_data %>% 
  distinct(sample, .keep_all = T) %>%
  t.test(.$age_years ~ .$experience_chicks, data = .)

#qPCR: Reference Gene assessment ####
ref_test <- 
  hipp_data %>%
  distinct(sample,.keep_all = T) %>%
  lm(ref_gene ~ experience_chicks + sex, data = .)  

#Anova 
car::Anova(ref_test, type = 3, test.statistic = "F")
anova(ref_test)