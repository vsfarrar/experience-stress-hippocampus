# Figure 4. PRL 3-Way Interaction Plot #
source("exp-stress-hipp_setup.R")

#source original models from table 3
setwd("~/Documents/GitHub/experience-stress-hippocampus/")
source("analysis_code/results_table2_hormone_glm.R")

#plot 3 way interaction for CORT experience
emm_cort <-  
  emmip(cort_exp_log, as.factor(experience_chicks) ~ as.factor(timepoint) | sex, 
      type = "response",
      CIs = TRUE, 
      dodge = 5) + 
  geom_point(position = position_dodge(5))+
  scale_x_continuous(breaks = c(0,30,60,90)) +
  labs(x = "time(minutes)", y = "estimated marginal means (ng/mL)", color = "experience") + 
  scale_color_discrete(labels = c("inexperienced", "experienced")) + 
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(size = 14, color = "black"))

#plot 3 way interaction for PRL experience
emm_prl <-  
  emmip(prl_exp_log, as.factor(experience_chicks) ~ as.factor(timepoint) | sex, 
        type = "response",
        CIs = TRUE,
        dodge = 5) + 
  geom_point(position = position_dodge(5))+
  scale_x_continuous(breaks = c(0,30,60,90)) +
  labs(x = "time(minutes)", y = "estimated marginal means (ng/mL)", color = "experience") + 
  scale_color_discrete(labels = c("inexperienced", "experienced")) + 
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(size = 14, color = "black"))

# cowplot ####
shared_legend_emm <- get_legend(emm_prl)

emmAB <- cowplot::plot_grid(emm_cort + labs(x = NULL) + theme(legend.position = "none"), 
                            emm_prl + theme(legend.position = "none"),
                            align = "h", 
                            nrow = 2, 
                            labels = "AUTO")

emm_full <-
  cowplot::plot_grid(emmAB, shared_legend_emm, 
                     ncol = 2, rel_widths = c(1,0.275), align = "v", axis = "t")

#export plot
ggsave(paste0("figures/fig4_emm_CORT_PRL_exp_",current_date,".png"), emm_full, bg = "white",
       width = 750/96, height = 750/96, units = "in", dpi = 300)