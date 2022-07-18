# Supplemental Figure 1: PRL 3-Way Interaction Plot #
source("exp-stress-hipp_setup.R")

#source original models from table 3
setwd("~/Documents/GitHub/experience-stress-hippocampus/")
source("analysis_code/results_table2_hormone_glm.R")

#plot 3 way interaction for PRL experience
suppfig1 <-  
  emmip(prl_exp_log, as.factor(experience_chicks) ~ as.factor(timepoint) | sex, 
      type = "response",
      CIs = TRUE) + 
  labs(x = "timepoint", y = "predicted response (ng/mL)", color = "experience",
       title = "PRL Levels: Interaction between experience,sex, and timepoint") + 
  scale_color_discrete(labels = c("experienced", "inexperienced")) + 
  theme_bw()

#export plot
ggsave(paste0("figures/supp-fig1_emm_PRL_exp_",current_date,".png"), suppfig1, bg = "white",
       width = 660/96, height = 515/96, units = "in", dpi = 300)