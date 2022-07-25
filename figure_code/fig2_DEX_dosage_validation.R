#Experience Stress Hippocampus MS
#Figure 2: DEX Dosage Validation

source("exp-stress-hipp_setup.R")

#Plot ####
dex_val_plot <- 
  dex_val %>% filter(group != "0.5 mg/kg" & timepoint != "30") %>%
  mutate(timepoint = timepoint + 30) %>%
  ggplot(aes(x=timepoint, y=cort, color = group, shape = group)) + 
  #geom_point(position = pd, alpha = 0.4) +
  stat_summary(geom = "point", fun = "mean", size = 3, position = pd) + 
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = pd, width = 10) + 
  stat_summary(aes(group = group, linetype = group), geom = "line", position = pd, fun = "mean") + 
  scale_color_manual(values = c("#808080","#FF8000", "#CC0066", "#9933FF")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted")) + 
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_x_continuous(breaks = seq(0,120,by = 30)) + 
  scale_y_continuous(breaks = seq(0,35,by = 5)) + 
  labs(x = "Time (minutes)", y = "CORT (ng/ml)",
       color = "DEX dosage", shape = "DEX dosage", linetype = "DEX dosage") +
  #add significance
  annotate(x = c(94,124), y = c(10.5,5), geom = "text", label = "*", size = 10) + 
  #add injected arrow
  annotate(x = 25, y = 30, geom = "text", label = "injected", size = 5, hjust = 0) + 
  annotate(x = 30, y = 28, geom = "text", label = "\u2193", size = 5, hjust = 0) + 
  theme_dex

# Export Plot ####
setwd("~/Documents/GitHub/experience-stress-hippocampus/")
ggsave(paste0("figures/fig2_dex_validation_",current_date,".png"), 
       plot = dex_val_plot,
      height = 355/97, width = 550/97, dpi = 300)
