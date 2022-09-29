#Supplemental Table 1. CORT and PRL across Inexperienced birds with and without eggs

source("exp-stress-hipp_setup.R")

#sample size ####
horm_blood %>% filter(stage == "nan") %>% distinct(bird_id, .keep_all = T) %>%
  count(inexp_eggs)

#t-tests ####

#__cort####

horm_blood %>%
  filter(stage == "nan") %>%
  group_by(timepoint) %>% nest() %>%
  mutate(test = map(data, ~ t.test(.x$cort_conc ~ .x$inexp_eggs)),
         tidied = map(test, tidy)) %>%
  unnest(tidied) %>%
  mutate(p.value = round(p.value, digits = 3))  %>%
  select(timepoint, inexperienced_no_eggs = estimate1, inexperienced_eggs = estimate2, t.value = statistic, p.value) %>%
  tab_df(digits = 3)
#all p > 0.1

#__prl####
horm_blood %>%
  filter(stage == "nan") %>%
  group_by(timepoint) %>% nest() %>%
  mutate(test = map(data, ~ t.test(.x$prl_conc ~ .x$inexp_eggs)),
         tidied = map(test, tidy)) %>%
  unnest(tidied) %>%
  mutate(p.value = round(p.value, digits = 3))  %>%
  select(timepoint, inexperienced_no_eggs = estimate1, inexperienced_eggs = estimate2, t.value = statistic, p.value) %>%
  tab_df(digits = 3)
#all p > 0.1