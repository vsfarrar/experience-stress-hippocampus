#Experience Stress Hippocampus MS
#Figure 2: DEX Dosage Validation

source("exp-stress-hipp_setup.R")

#Plot ####
dex_val_plot <- 
  dex_val %>% filter(group != "0.5 mg/kg" & timepoint != "30") %>%
  mutate(timepoint = timepoint + 30, 
         timepoint_group = paste(timepoint,group)) %>%
  ggplot(aes(x=timepoint, y=cort, color = group, shape = group, 
             group = timepoint_group )) + 
  geom_boxplot(position = position_dodge(10), alpha = 0.4)+
  geom_point(position = position_dodge(10), alpha = 0.7) +
  #stat_summary(geom = "point", fun = "mean", size = 3, position = pd) + 
  #stat_summary(geom = "errorbar", fun.data = "mean_se", position = pd, width = 10) + 
  stat_summary(aes(group = group, linetype = group), geom = "line", position = pd, fun = "mean") + 
  scale_color_manual(values = c("#808080","#FF8000", "#CC0066", "#9933FF")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted")) + 
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_x_continuous(breaks = seq(0,120,by = 30)) + 
  scale_y_continuous(breaks = seq(0,55,by = 10)) + 
  labs(x = "Time (minutes)", y = "CORT (ng/ml)",
       color = "DEX dosage", shape = "DEX dosage", linetype = "DEX dosage") +
  #add significance
  annotate(x = c(102,128), y = c(8.5,5), geom = "text", label = "*", size = 10) + 
  #add injected arrow
  annotate(x = 25, y = 50, geom = "text", label = "injected", size = 5, hjust = 0) + 
  annotate(x = 30, y = 48, geom = "text", label = "\u2193", size = 5, hjust = 0) + 
  theme_dex

# Export Plot ####
setwd("~/Documents/GitHub/experience-stress-hippocampus/")
ggsave(paste0("figures/fig2_dex_validation_",current_date,".png"), 
       plot = dex_val_plot,
      height = 355/97, width = 550/97, dpi = 300)
