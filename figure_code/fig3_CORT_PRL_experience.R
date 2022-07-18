# FIGURE 3. CORT and Prolactin Stress Series with Experience #
source("exp-stress-hipp_setup.R")

#Labels for Below X Axis ####
text0 <- textGrob("baseline")
text30 <- textGrob("stress-induced")
text90 <- textGrob("neg.feedback")

#A. CORT Experience ####

fig_cort_exp <- 
horm_blood %>%
  ggplot(aes(x = timepoint, y = cort_conc, color =  as.factor(experience_chicks), fill = sex,
             group = interaction(timepoint,  as.factor(experience_chicks),sex))) +
  geom_boxplot(alpha = 0.2, outlier.shape = NULL, position = position_dodge(width = 27)) + 
  geom_point(aes(shape = sex), alpha = 0.5, 
             position = position_dodge(width = 27), size = 2) + 
  geom_vline(xintercept = c(15,45,75), linetype = "dashed") + 
  #add significant effect annotations
  annotate(geom = "label", label = "experience*timepoint",x = Inf,y = Inf, hjust = 1.1,vjust = 1.5, 
           fontface = "bold", fill = "white") + 
  annotate(geom = "text", label = "*", x = c(30,90), y = c(70,30), size = 11, color = "#BA0000") + 
  labs(x = "\ntime (minutes)", y = "CORT (ng/mL)",
       color = "experience", fill = "sex", shape = "sex", linetype = NULL) +  
  scale_fill_manual(values = c("black", "white")) + 
  scale_color_manual(labels = c("inexperienced\n(no active nest)\n", "experienced\n(no active nest)\n"),
                     values = c( "black", "red")) +
  scale_x_continuous(breaks = c(0,30,60,90)) + 
  scale_y_continuous(breaks = seq(0,70,by =10), limits = c(0,75)) + 
  theme_dex + 
  #add below-x-axis labels for timepoints
  annotation_custom(text0,xmin=0,xmax=0,ymin=-13,ymax=-13) + 
  annotation_custom(text30,xmin=30,xmax=30,ymin=-13,ymax=-13) + 
  annotation_custom(text90,xmin=90,xmax=90,ymin=-13,ymax=-13) + 
  coord_cartesian(clip = "off")

# B. PRL Experience ####
fig_prl_exp <- 
  horm_blood %>%
  ggplot(aes(x = timepoint, y = prl_conc, color = as.factor(experience_chicks), fill = sex,
             group = interaction(timepoint, as.factor(experience_chicks),sex))) +
  geom_boxplot(alpha = 0.2, outlier.shape = NULL, position = position_dodge(width = 27)) + 
  geom_point(aes(shape = sex), alpha = 0.5, 
             position = position_dodge(width = 27), size = 2) + 
  geom_vline(xintercept = c(15,45,75), linetype = "dashed") + 
  #add significant effect annotations
  annotate(geom = "label", label = "experience*sex*timepoint",x = Inf,y = Inf, hjust = 1.1,vjust = 1.5, 
           fontface = "bold", fill = "white") +
  geom_signif(annotation = "**", y_position = c(27.5,23.5), xmin = c(19,79), xmax = c(27,87), textsize = 10,vjust = 0.5) +
  #male 30
  geom_signif(annotation = "*", y_position = 18, xmin = 32 , xmax = 40, textsize = 10,vjust = 0.5) +
  #male 90
  geom_signif(annotation = "#", y_position = 18, xmin = 92 , xmax = 100, textsize = 5,vjust = 0.2) +
  labs(x = "\ntime (minutes)", y = "prolactin (ng/mL)",
       color = "experience", fill = "sex", shape = "sex", linetype = NULL) +  
  scale_fill_manual(values = c("f" = "black", "m" = "white")) + 
  scale_color_manual(labels = c("inexperienced\n(no active nest)", "experienced\n(no active nest)"),
                     values = c( "black", "red")) +
  scale_x_continuous(breaks = c(0,30,60,90)) + 
  scale_y_continuous(breaks = seq(0,30,by =5)) + 
  theme_dex + 
  #add below-x-axis labels for timepoints
  annotation_custom(text0,xmin=0,xmax=0,ymin=-4,ymax=-4) + 
  annotation_custom(text30,xmin=30,xmax=30,ymin=-4,ymax=-4) + 
  annotation_custom(text90,xmin=90,xmax=90,ymin=-4,ymax=-4) + 
  coord_cartesian(clip = "off")

# cowplot ####
shared_legend_exp <- get_legend(fig_cort_exp)

figAB <- cowplot::plot_grid(fig_cort_exp + labs(x = NULL) + theme(legend.position = "none"), 
                            fig_prl_exp + theme(legend.position = "none"),
                            align = "h", 
                            nrow = 2, 
                            labels = "AUTO")

full_fig <-
  cowplot::plot_grid(figAB, shared_legend_exp, 
                     ncol = 2, rel_widths = c(1,0.275), align = "v", axis = "t")

# export plot ####
setwd("~/Documents/GitHub/experience-stress-hippocampus/")
ggsave(filename = paste0("figures/fig3_CORT_PRL_experience_",current_date,".png"), plot = full_fig, bg = "white",
       width = 750/96, height = 865/96, units = "in", dpi = 300)


