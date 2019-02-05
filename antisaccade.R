# Author: Benjamin Carter
# Date: 2019-02-05
# Project: Eye Movement Consistency Extended
# Purpose: Preprocess and clean visual search data and then perform statistics to determine the consistency of eye movements over time.

###############
# ENVIRONMENT #
###############

# check for required packaages are install them if necessary
list.of.packages <- c("psych","reshape2","car","lme4","ggplot2")                                   # list of packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]        # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages)                                            # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                             # load packages

# variables and paths
REPORT_DIRECTORY <- "~/Box/LukeLab/Caffeine/eyelinkData/reports"

#######################
# PREPROCESSING STEPS #
#######################