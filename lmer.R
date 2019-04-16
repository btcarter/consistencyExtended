# Author: Benjamin Carter
# Date: 2019-03-06
# Project: Eye Movement Consistency Extended
# Purpose: Preprocess visual search data and then perform statistics to determine the consistency of eye movements over time
#          and if there is a difference between sessions and conditions.


#### ENVIRONMENT ####

# check for required packaages are install them if necessary
list.of.packages <- c("lme4","lmerTest")                                                                              # list of packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]                           # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)                                          # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                                                # load packages


#### VARIABLES AND PATHS ####

report.dir <- "~/Box/LukeLab/Caffeine/eyelinkData/reports/"
reports <- c("ProsaccadeFixationReport.txt","AntisaccadeFixationReport.txt","SearchFixationReport.txt","ReadingFixationReport.txt")     # names of the fixation reports as an array
output.dir <- "~/Box/LukeLab/Caffeine/results/"                                                                                         # a path to the output destination
correction.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/subjectCorrections.txt"                                             # this is the matrix containing all the errors and all the corrections
sessions.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/participantList.txt"                                                  # a path to the sessions list

#### PREPROCESSING ####

# combine reports into one data.frame and add a session variable.
#   function to read report, correct errors and add to data frame
preprocessing <- function(report,corrections,sessions) {
  corrections = correction.matrix
  sessions = sessions.matrix
  a = paste(report.dir,report,sep="")
  original <- read.table(a, header = TRUE, sep = "\t", na.strings = ".", dec = ".",fill = TRUE)
  corrections <- read.table(corrections, header = TRUE, sep = "\t", na.strings = ".", dec = ".",fill = TRUE)
  sessions <- read.table(sessions, header = TRUE, sep = "\t", na.strings = ".", dec = ".",fill = TRUE)
  
  # replace broken participant labels
  original$Subject <- as.character(original$RECORDING_SESSION_LABEL)                                      # create the subject column and set it equal to the characters in the recording session labels
  for (i in 1:nrow(corrections)) {                                                                        # for loop to run through the correction data and fix the mistakes in the recording session labels,
    brokenWindow = corrections[i,1]                                                                       # then move those corrections from the subject column to the recording session labels
    brokenWindow = factor(brokenWindow, levels = levels(original$RECORDING_SESSION_LABEL))
    newWindow = corrections[i,2]
    original$Subject[original$RECORDING_SESSION_LABEL == brokenWindow] = as.character(newWindow)
  }
  original$Subject = as.factor(original$Subject)
  original$RECORDING_SESSION_LABEL <- original$Subject
  original$Subject <- NULL                                                                                # disappear the now redundant subject column
  original = original[is.na(original$NEXT_SAC_AMPLITUDE) == FALSE,]                                       # remove rows without saccade information (the NA rows)
  
  # parse recording session labels into participantID and treatment condition variable
  original$SUBJECT <- gsub("s(\\d+)c\\w","\\1", original$RECORDING_SESSION_LABEL)                         # extract subject numbers and create a new subject column and put them in there.
  original$CONDITION <- gsub("s\\d+c(\\w)","\\1", original$RECORDING_SESSION_LABEL)                       # now do the same thing for caffeine condition
  
  # remove participants without four sessions
  all.participants <- unique(original$SUBJECT)
  complete.participants <- unique(sessions$Subject)
  incomplete.participants <- all.participants[!(all.participants %in% complete.participants)]
  for (i in incomplete.participants) {
    original <- original[!(original$SUBJECT == i),]
  }
  
  # add in session IDs for each data point
  sessions$RECORDING_SESSION_LABEL <- paste("s",sessions$Subject,"c",sessions$Condition,sep="")                            # create a RECORDING_SESSION_LABEL variable for the sessions matrix
  original$SESSION = 10                                                                                                    # create the sessions variable for the original dataset and set it equal to integers between 1 and 4 for all entries
  for (i in 1:nrow(sessions)) {                                                                                            # now loop through the session variable in the original report and replace the value with the correct one from the sessions document
    recording.session.label = sessions[i,4]
    session.number = as.numeric(sessions[i,3])
    original$SESSION[original$RECORDING_SESSION_LABEL == recording.session.label] = session.number
  }
  
  # add task variable
  original$TASK = gsub("(\\w+)FixationReport.txt","\\1",report)
  
  # select and order variables of interest
  variables = c("RECORDING_SESSION_LABEL","SUBJECT","CONDITION","SESSION","TASK","CURRENT_FIX_DURATION","NEXT_SAC_DURATION","NEXT_SAC_AVG_VELOCITY","NEXT_SAC_PEAK_VELOCITY","NEXT_SAC_CONTAINS_BLINK","NEXT_SAC_AMPLITUDE")
  original = original[variables]
  
  # return output
  return(original)
}

# start the data.frame
df.all <- data.frame()

# apply preprocessing function to list of fixation reports, adding them to a single data.frame
for (report in reports) {
  preprocessed <- preprocessing(report,correction.matrix,sessions.matrix)
  df.all <- rbind(df.all,preprocessed)
}

#### MADAGASCAR - Playing with LMERs ####

# #   is there a difference between conditions and what does that look like?
# #     fixation duration: lmer
# fix.dur = lmer(CURRENT_FIX_DURATION ~ TASK + (1 | SUBJECT) + (TASK || CONDITION), data = df.all)
# summary(fix.dur)
# 
# #     saccade amplitude: lmer
# sac.amp = lmer(NEXT_SAC_AMPLITUDE ~ TASK + (1 | SUBJECT) + SESSION + CONDITION, data = df.all)
# summary(sac.amp)
# 
# #     saccade peak velocity: lmer
# sac.vel = lmer(NEXT_SAC_PEAK_VELOCITY ~ TASK + (1 | SUBJECT) + SESSION + CONDITION, data=df.all)
# summary(sac.vel)

#   Can we predict readnging?
reading =  lmer(TASK ~ CURRENT_FIX_DURATION + (1 |SUBJECT) + (CURRENT_FIX_DURATION || SUBJECT) + CONDITION, data = df.all)
summary(reading)