# Author: Benjamin Carter
# Date: 2019-02-05
# Project: Eye Movement Consistency Extended
# Purpose: Preprocess and clean visual search data and then perform statistics to determine the consistency of eye movements over time.

###############
# ENVIRONMENT #
###############

# check for required packaages are install them if necessary
list.of.packages <- c("psych","reshape2","car","lme4","ggplot2","tidyverse","data.table")          # list of packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]        # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)                       # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                             # load packages

#######################
# VARIABLES AND PATHS #
#######################

fixation.report <- "~/Box/LukeLab/Caffeine/eyelinkData/reports/SearchFixationReport.txt"                 # the fixation report from dataViewer
correction.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/subjectCorrections.txt"              # this is the matrix containing all the errors and all the corrections
output.dir <- "~/Box/LukeLab/Caffeine/results"                                                           # a path to the output destination

#################
# PREPROCESSING #
#################

# read in the report and a table of corrections
original <- read.table(fixation.report, header = TRUE, sep = "\t", na.strings = ".", dec = ".")
corrections <- read.table(correction.matrix, header = TRUE, sep = "\t", na.strings = ".", dec = ".")

original$Subject <- as.character(original$RECORDING_SESSION_LABEL)                                      # create the subject column and set it equal to the characters in the recording session labels
for (i in 1:nrow(corrections)) {                                                                        # for loop to run through the correction data and fix the mistakes in the recording session labels,
  brokenWindow = corrections[i,1]                                                                       # then move those corrections from the subject column to the recording session labels
  brokenWindow = factor(brokenWindow, levels = levels(original$RECORDING_SESSION_LABEL))
  newWindow = corrections[i,2]
  original[original$RECORDING_SESSION_LABEL == brokenWindow, ]$Subject = as.character(newWindow)
}
original$Subject = as.factor(original$Subject)
original$RECORDING_SESSION_LABEL <- original$Subject
original$Subject <- NULL                                                                                # disappear the now redundant subject column

# parse recording session labels into participantID and treatment condition variable
original$SUBJECT <- gsub("s(\\d+)c\\w","\\1", original$RECORDING_SESSION_LABEL)                         # extract subject numbers and create a new subject column and put them in there.
original$CONDITION <- gsub("s\\d+c(\\w)","\\1", original$RECORDING_SESSION_LABEL)                       # now do the same thing for caffeine condition

#########
# MATHS #
#########

# aggregate the data by subject and session, i.e. compute means and standard deviations for each subject and session

test <- by(data = list(original$CURRENT_FIX_DURATION,original$NEXT_SAC_AMPLITUDE,original$NEXT_SAC_PEAK_VELOCITY), INDICES = list(original$SUBJECT,original$CONDITION), FUN = describe)
test2 <- data.frame(test, row.names = c())
print(test2)

# old stuff I tried. everything broke for the saccade amplitude and velocity so I gave up on it.
# #   do it for fixation duration
# MeanFix <- aggregate(original$CURRENT_FIX_DURATION, by=list(original$SUBJECT,original$CONDITION), FUN= function(original$CURRENT_FIX_DURATION){fitdistr(original$CURRENT_FIX_DURATION, densfun = "normal")$estimate} )
# names(MeanFix) <- c("Subject","Condition","fixMean")
# SDFix <- aggregate(original$CURRENT_FIX_DURATION, by=list(original$SUBJECT,original$CONDITION), FUN=sd)
# names(SDFix) <- c("Subject","Condition","fixSD")
# 
# #   do it for saccade amplitude
# MeanSacAmp <- aggregate(original$NEXT_SAC_AMPLITUDE, by=list(original$SUBJECT,original$CONDITION), FUN = mean)
# names(MeanSacAmp) <- c("Subject","Condition","sacAmpMean")
# SDSacAmp <- aggregate(original$NEXT_SAC_AMPLITUDE, by=list(original$SUBJECT,original$CONDITION), FUN=sd)
# names(SDSacAmp) <- c("Subject","Condition","sacAmpSD")
# 
# #   do it for saccade velocity
# MeanSacVel <- aggregate(original$NEXT_SAC_AVG_VELOCITY, by=list(original$SUBJECT,original$CONDITION), FUN=mean)
# names(MeanSacVel) <- c("Subject","Condition","sacVelMean")
# SDSacVel <- aggregate(original$NEXT_SAC_AVG_VELOCITY, by=list(original$SUBJECT,original$CONDITION), FUN=sd)
# names(SDSacVel) <- c("Subject","Condition","sacVelSD")


# #   bring it all together into one data frame
# aggAll <- merge(MeanFix,SDFix,MeanSacAmp,SDSacAmp,MeanSacVel,SDSacVel,by=c("Subject","Condition"))
# 
# test <- by(original$NEXT_SAC_AVG_VELOCITY, list(original$SUBJECT,original$CONDITION), FUN = stat.desc)