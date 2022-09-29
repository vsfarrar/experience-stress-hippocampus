#Experience-Stress-Hippocampus Project: SETUP FILE
#Data,Packages, Functions, and Graphics
#Source this script before running analyses and figure scripts

#PACKAGES ####
library(tidyverse)
library(plotrix) #std.error
library(sjPlot) #export tables
library(cowplot)
library(grid)
library(gridExtra)
library(ggsignif)
#stats
library(emmeans)
library(lme4)
library(lmerTest)

#STATS OPTIONS ####
options(contrasts = c("contr.sum", "contr.poly"))

#DATA ####
#source data from Dryad: https://doi.org/10.25338/B8KK91
setwd("~/Documents/experience-stress-series/dryad-data")

#hormone and bleeding data (CORT and PRL)
horm_blood <- read.csv("Farrar_experience-stress-series_CORT_PRL_hormone-data.csv")

#hippocampus gene expression data
hipp_data <- read.csv("Farrar_experience_hippocampus_gene-expression_data.csv")

#dex validation CORT concentrations
dex_val <- read.csv("Farrar_DEX_dosage_validation_data.csv")

# FUNCTIONS ####

#grab current date using Sys.Date()
current_date <- as.Date(Sys.Date(), format = "%m/%d/%Y")

# PLOTTING FUNCTIONS ####
#figure theme
theme_dex <- 
  theme_classic(base_size = 14) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.line = element_blank(),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black"), 
        axis.ticks = element_line(color = "black"))

pd <- position_dodge(width = 5)