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
readingReport <- "ReadingFixationReport.txt"                                                                                            # name of the reading fixation report
latencyReport <- "Saccade Latency - Antisaccade Task.txt"                                                                               # name of saccade latency report from Antisaccade task
searchFixReport <- "SearchFixationReport.txt"                                                                                              # name of search task fixation report
searchRTReport <- "Search RT - Trial Report.txt"                                                                                        # name of search reaction time report
output.dir <- "~/Box/LukeLab/Caffeine/results/"                                                                                         # a path to the output destination
correction.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/subjectCorrections.txt"                                             # this is the matrix containing all the errors and all the corrections
sessions.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/participantList.txt"                                                  # a path to the sessions list

#### PREPROCESSING ####

# split Antisaccade fixation report into two, one for antisaccades and one for prosaccades
antisaccades <- function(report,directory) {
  saccades <- paste(report.dir,latencyReport,sep="")
  saccades <- read.table(saccades,header=TRUE,sep="\t",na.strings=".",dec=".",fill=TRUE)
  anti <- subset(saccades, task == "antisaccade")
  return(anti)
}

prosaccades <- function(report,directory) {
  saccades <- paste(report.dir,latencyReport,sep="")
  saccades <- read.table(saccades,header=TRUE,sep="\t",na.strings=".",dec=".",fill=TRUE)
  pro <- subset(saccades, task == "prosaccade")
  return(pro)
}

# function to read report, correct errors participant naming conventions, remove participants who did not complete the study, and add a task variable
preprocessing <- function(report,report.dir,corrections,sessions) {
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
  
  # return output
  return(original)
}

#### SUMMARY STATISTICS FUNCTIONS ####

#   anti/prosaccade - saccade latencey (i.e. how long till they looked?)
latencyMean <- function(report,directory,corrctions,sessions) {
  DATA <- read.csv(paste(directory,report,sep = ""),header=TRUE,sep="\t",na.strings=".")
  DATA <- preprocessing(DATA,corrections,sessions)
  DATA <- DATA[,c("RECORDING_SESSION_LABEL","CURRENT_FIX_INDEX","CURRENT_FIX_DURATION")]
  for (participant in unique(DATA[["RECORDING_SESSION_LABEL"]])) {
    DATA[["MEAN_LATENCY"]][DATA[["RECORDING_SESSION_LABEL"]] == participant] <-
      mean(DATA[["CURRENT_FIX_DURATION"]][DATA[["RECORDING_SESSION_LABEL"]]==participant & DATA[["CURRENT_FIX_INDEX"]] == 1])
  }
  return(DATA[,c("RECORDING_SESSION_LABEL","MEAN_LATENCY")])
}

#   search - initiation time (how long till they started searching? 
#     This is equal to the duration of the first fixation of a trial in the search report)
initiationMean <- function(report,directory,correction,sessions) {
  DATA <- read.table(paste(directory,report,sep=""),header=TRUE,sep="\t",na.strings=".")
  DATA <- preprocessing(DATA,corrections,sessions)
  DATA <- DATA[,c("RECORDING_SESSION_LABEL","CURRENT_FIX_INDEX","CURRENT_FIX_DURATION")]
  for (participant in unique(DATA[["RECORDING_SESSION_LABEL"]])) {
    DATA[["MEAN_SEARCH_INIT"]][DATA[["RECORDING_SESSION_LABEL"]] == participant] <-
      mean(DATA[["CURRENT_FIX_DURATION"]][DATA[["RECORDING_SESSION_LABEL"]] == participant & DATA[["CURRENT_FIX_INDEX"]] == 1])
  }
  return(DATA[,c("RECORDING_SESSION_LABEL","MEAN_SEARCH_INIT")])
}

#   search - accuracy (did they look in the right place?)
#     This is equal to the % of saccades that fall within the interest area of a trial and are labeled Target_Object_4deg.
accuracyMean <- function(report,directory,correction,sessions) {
  DATA <- read.table(paste(directory,report,sep=""),header=TRUE,sep="\t",na.strings=".")
  DATA <- preprocessing(DATA,corrections,sessions)
  DATA <- DATA[,c("RECORDING_SESSION_LABEL","TRIAL_INDEX","CURRENT_FIX_INTEREST_AREA_LABEL")]
  for (participant in unique(DATA[["RECORDING_SESSION_LABEL"]])) {
    for (trial in unique(DATA[["TRIAL_INDEX"]])) {
      totalFixations <-
        length(DATA[["RECORDING_SESSION_LABEL"]][DATA[["RECORDING_SESSION_LABEL"]] == participant & DATA[["TRIAL_INDEX"]]==trial])
      correctFixations <-
        length(DATA[["RECORDING_SESSION_LABEL"]][DATA[["RECORDING_SESSION_LABEL"]] == participant & 
                                                   DATA[["TRIAL_INDEX"]]==trial &
                                                   DATA[["CURRENT_FIX_INTEREST_AREA_LABEL"]]=="Target_Object_4deg"])
      DATA[["MEAN_ACCUR"]][DATA[["RECORDING_SESSION_LABEL"]]==participant & DATA[["TRIAL_INDEX"]]==trial] <-
        correctFixations / totalFixations
    }
  }
  return()
}


#   search - verification time (how long did it take to press the button?
#     Use the searchRT for this
verificationTime <- function(report,directory,correction,sessions) {
  DATA <- read.table(paste(directory,report,sep=""),header=TRUE,sep="\t",na.strings=".")
  DATA <- preprocessing(DATA,corrections,sessions)
  DATA <- DATA[,c("RECORDING_SESSION_LABEL","REACTION_TIME")]
  for (participant in unique(DATA[["RECORDING_SESSION_LABEL"]])) {
    DATA[["MEAN_VER_TIME"]][DATA[["RECORDING_SESSION_LABEL"]]==participant] <-
      mean(DATA[["REACTION_TIME"]][DATA[["RECORDING_SESSION_LABEL"]]==participant])
  }
  return(DATA[,c("RECORDING_SESSION_LABEL","MEAN_VER_TIME")])
}

#   combine a list of dataframes into a single dataframe by recording_session_label (maybe try do.call if merge doesn't)
summaryStats <- function(LIST,destination) {
  summaryDF <- data.frame()
  df.number <- length(LIST)
  lapply(LIST,merge(destination,by="RECORDING_SESSION_LABEL"))
  return(summaryDF)
}

#### CREATE SUMMARY STATS MATRIX ####


# apply preprocessing function to list of fixation reports, adding them to a single data.frame
for (report in fixationReports) {
  assign(gsub("(\\w+)FixationReport.txt","\\1",report),preprocessing(report,report.dir,correction.matrix,sessions.matrix))
}

assign(LATENCY,preprocessing(paste(latencyReport,report.dir,sep=""),correction.matrix,sessions.matrix))     # preprocess latency report

# PULL SACCADE ACCURACY FROM DATA$CURRENT_FIX_INTEREST_AREA_LABEL

# search initiation time

# search verification time


# combine these stats into a single dataframe. requires stat dfs to be in a list



#### MADAGASCAR - Playing with LMERs ####

#   is there a difference between conditions and what does that look like?
#     fixation duration: lmer
fix.dur = lmer(CURRENT_FIX_DURATION ~ TASK + CONDITION + SUBJECT + (1|SUBJECT/TASK), data = df.all)
summary(fix.dur)

#     saccade amplitude: lmer
sac.amp = lmer(NEXT_SAC_AMPLITUDE ~ TASK + (1 | SUBJECT) + SESSION + CONDITION, data = df.all)
summary(sac.amp)

#     saccade peak velocity: lmer
sac.vel = lmer(NEXT_SAC_PEAK_VELOCITY ~ TASK + (1 | SUBJECT) + SESSION + CONDITION, data=df.all)
summary(sac.vel)
