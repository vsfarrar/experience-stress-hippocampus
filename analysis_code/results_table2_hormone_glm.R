#Experience-Stress-Hippocampus MS
#RESULTS: Hormone Concentration Models
#outputs: Table 2

source("exp-stress-hipp_setup.R")

#CORT models ####

#mixed model, log-transformed, all data (random effect of ID)
cort_exp_log <-
  lmer(log(cort_conc) ~ as.factor(experience_chicks)*sex*as.factor(timepoint) + (1|bird_id) + (1|date_bled), 
       data = horm_blood)

anova(cort_exp_log) 

#post-hoc contrasts averaged across sex
summary(emmeans(cort_exp_log, pairwise ~ as.factor(experience_chicks) | as.factor(timepoint)), type = "response")

#PRL model ####

prl_exp_log <-
  lmer(log(prl_conc + 1) ~ as.factor(experience_chicks)*sex*as.factor(timepoint) + (1|bird_id) + (1|date_bled), 
       data = horm_blood) 

car::Anova(prl_exp_log,type = "III") 

#posthoc analyses
summary(emmeans(prl_exp_log, pairwise ~ as.factor(experience_chicks) | as.factor(timepoint)), type = "response")

#examining significant three-way interaction
emms1 <- emmeans(prl_exp_log, ~ as.factor(experience_chicks)*timepoint | sex)
con1 <- contrast(emms1, interaction = "pairwise")
pairs(con1, by = NULL)  

emmip(prl_exp_log, ~ as.factor(experience_chicks) | as.factor(timepoint)| sex) #plots 3-way interaction marginal means

emmeans(prl_exp_log, pairwise ~ as.factor(experience_chicks) | as.factor(timepoint)*sex, type = "response") #shows that effect of experience is larger in females than in males across.

#Correlations ####

horm_blood %>%
  cor.test(.$cort_conc, .$prl_conc, data = .) #n.s. 

#Export Tables ####
#table 2
#tables are exported and edited in Word documents.
sjPlot::tab_df(broom::tidy(car::Anova(cort_exp_log, type = "III")), digits = 3)
sjPlot::tab_df(broom::tidy(car::Anova(prl_exp_log, type = "III")), digits = 3)