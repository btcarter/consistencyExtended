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
REPORT <- "~/Box/LukeLab/Caffeine/eyelinkData/reports/SearchFixationReport.txt"                    # the fixation report from dataViewer
CORRECTIONS <- "~/Dropbox/Lab data & Papers/analyses/caffeine/subjectCorrections.txt"              # this is the matrix containing all the errors and all the corrections
OUTPUT_DIR <- "~/Box/LukeLab/Caffeine/results"                                                     # a path to the output destination

#######################
# PREPROCESSING STEPS #
#######################

# fix broken participant labels c14sy, s06co, s09cg, s09co

  # a function to do it
fixItFelix <- function(original,corrections) {
  for (i in 1:length(corrections$V1)) {
    original[original$V1 == corrections[i,1], ]$V1 = corrections[i,2]
  }
}

  # read in the report and a table of corrections
REPORT <- read.delim(REPORT,header = TRUE, sep = "\t", na.strings = ".")
CORRECTIONS <- read.table(CORRECTIONS,header = FALSE, sep = "\t", dec = ".")

fixItFelix(REPORT,CORRECTIONS)


# parse recording session labels into participantID and treatment condition variable
print(unique(REPORT$RECORDING_SESSION_LABEL))
