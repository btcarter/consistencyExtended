# Author: Benjamin Carter
# Date: 2019-03-06
# Project: Eye Movement Consistency Extended
# Purpose: Preprocess visual search data and then perform statistics to determine the consistency of eye movements over time
#          and if there is a difference between sessions and conditions.


###############
# ENVIRONMENT #
###############

# check for required packaages are install them if necessary
#potential list.of.packages <- c("psych","reshape2","car","lme4","ggplot2","tidyverse","data.table","dplyr","lmerTest")          # list of potential packages
list.of.packages <- c("reshape2","Hmsic")          # list of packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]                           # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)                                          # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                                                # load packages


#######################
# VARIABLES AND PATHS #
#######################

report.dir <- "~/Box/LukeLab/Caffeine/eyelinkData/reports/"
reports <- c("AntiSaccadeFixationReport.txt","SearchFixationReport.txt","ReadingFixationReport.txt")     # names of the fixation reports as an array
output.dir <- "~/Box/LukeLab/Caffeine/results/"                                                          # a path to the output destination
correction.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/subjectCorrections.txt"              # this is the matrix containing all the errors and all the corrections
sessions.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/participantList.txt"                   # a path to the sessions list

#################
# PREPROCESSING #
#################

# combine reports into one data.frame and add a session variable.
#   function to read report, correct errors and add to data frame
preprocessing <- function(report,dataframe,corrections,sessions) {
  original <- read.table(report, header = TRUE, sep = "\t", na.strings = ".", dec = ".",fill = TRUE)
  corrections <- read.table(corrections, header = TRUE, sep = "\t", na.strings = ".", dec = ".",fill = TRUE)
  sessions <- read.table(sessions, header = TRUE, sep = "\t", na.strings = ".", dec = ".",fill = TRUE)
}

# start the data.frame
df.all <- data.frame()

#   lappy preprocessing function to list of fixation reports


iGottaPee <- function(time) {
  a = paste("I have to pee in ",time, "seconds!")
  print(a)
}

lapply(c(6:1),iGottaPee)

##################################
# MADAGASCAR - Playing with LMER #
##################################

#   is there a difference between conditions and what does that look like?
#     fixation duration: lmer
fix.dur = lmer(CURRENT_FIX_DURATION ~ CONDITION + (1 |SUBJECT), data = original)
summary(fix.dur)

#     saccade amplitude: lmer