# FIGURE 4. Hippocampal GR and MR expression #
source("exp-stress-hipp_setup.R")
setwd("~/Documents/GitHub/experience-stress-hippocampus/")

#import Hipp drawing ####
hipp_img <- ggdraw() + draw_image("figures/Hipp_drawing.png")


#GR  ####
gr_plot <- 
  hipp_data %>%
  filter(gene == "gr") %>%
  mutate(treatment = factor(experience_chicks,
                            labels = c("inexperienced",
                                       "experienced"))) %>%
  ggplot(aes(x = treatment, y = log_fold, color = treatment, shape = sex, fill = sex)) + 
  geom_point(aes(shape = sex), alpha = 0.7, 
             position = position_dodge(width = 0.75), size = 2) + 
  geom_boxplot(alpha = 0.2, outlier.shape = NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
  #significance
  #annotate(geom = "text", x = 1.5, y = Inf, label = "*", size = 14, vjust = 1.1) +
  annotate(geom = "label", label = "experience",x = Inf,y = Inf, hjust = 1.1,vjust = 1.5, 
           fontface = "bold", fill = "white") + 
  geom_signif(annotation = "**", y_position = 6.2, xmin = 1 , xmax = 2, 
              textsize = 12,vjust = 0.5, color = "black") + 
  labs(x = NULL, y = "log (fold change)", color = NULL, shape = "sex", fill = "sex",
       subtitle = expression(italic("GR"))) + 
  scale_y_continuous(breaks = seq(-4,8,by =2),limits = c(-4.2,9))+ 
  scale_color_manual(values = c("#545151", "#F8766D"),
                     labels = c("inexperienced \n(no active nest)\n",
                                "experienced \n(no active nest)\n")) + 
  scale_fill_manual(values = c("black", "white")) + 
  theme_dex 

#MR ####
mr_plot <-
  hipp_data %>%
  filter(gene == "mr") %>%
  mutate(treatment = factor(experience_chicks,
                            labels = c("inexperienced",
                                       "experienced"))) %>%
  ggplot(aes(x = treatment, y = log_fold, color = treatment, shape = sex, fill = sex)) + 
  geom_point(aes(shape = sex), alpha = 0.7, 
             position = position_dodge(width = 0.75), size = 2) + 
  geom_boxplot(alpha = 0.2, outlier.shape = NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
  #significance annotations
  annotate(geom = "label", label = "experience*sex",x = Inf,y = Inf, hjust = 1.1,vjust = 1.3, 
           fontface = "bold", fill = "white") + 
  geom_signif(annotation = "**", y_position = 7.5, xmin = 0.8 , xmax = 1.2, 
              textsize = 10,vjust = 0.5, color = "black") +
  labs(x = NULL, y = "log (fold change)", color = NULL, shape = "sex", fill = "sex",
       subtitle = expression(italic("MR"))) + 
  scale_y_continuous(breaks = seq(-4,8,by =2), limits = c(-4.2,9))+ 
  scale_color_manual(values = c("#545151", "#F8766D"),
                     labels = c("inexperienced \n(no active nest)\n",
                                "experienced \n(no active nest)\n")) + 
  scale_fill_manual(values = c("black", "white")) + 
  theme_dex 

#MR:GR Ratio ####
mr_gr_ratio <-
  hipp_data %>%
  select(sample, treatment, gene, experience_chicks, sex, log_fold) %>%
  pivot_wider(names_from = gene, values_from = log_fold) %>%
  mutate(mr_gr_ratio = mr/gr) %>%
  filter(between(mr_gr_ratio,-6,40)) %>%
  mutate(treatment = factor(experience_chicks,
                            labels = c("Inexperienced",
                                       "Experienced"))) %>%
  ggplot(aes(x = treatment, y = mr_gr_ratio, color = treatment, shape = sex)) +
  stat_summary(fun = "mean", geom = "point", 
               position = position_dodge(width = 0.5), size = 3) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5,
               position = position_dodge(width = 0.5))  + 
  geom_point(alpha = 0.7, position = position_dodge(width = 0.5)) + 
  labs(x = NULL, y = "MR:GR ratio", color = NULL, shape = "Sex", 
       subtitle = "MR:GR ratio") + 
  scale_color_manual(values = c("#545151", "#F8766D"),
                     labels = c("Inexperienced \n(no active nest)",
                                "Experienced \n(no active nest)")) + 
  theme_dex


#cowplot ####
qpcr_legend <- get_legend(gr_plot) #grab shared legend

full_fig5 <- 
  cowplot::plot_grid(hipp_img,
                     gr_plot + theme(legend.position = "none"), 
                     mr_plot + theme(legend.position = "none"), 
                     qpcr_legend, 
                     rel_widths = c(0.6,1,1,0.425), align = "h", axis = "t", nrow = 1,
                     labels = c("A","B","C",""))


# export plot ####
setwd("~/Documents/GitHub/experience-stress-hippocampus/")
ggsave(paste0("figures/fig4_Hipp_qpcr_",current_date,".png"), full_fig5, bg = "white",
       width = 1000/96, height = 580/96, units = "in", dpi = 300)