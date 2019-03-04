# Author: Benjamin Carter
# Date: 2019-02-05
# Project: Eye Movement Consistency Extended
# Purpose: Preprocess visual search data and then perform statistics to determine the consistency of eye movements over time
#          and if there is a difference between sessions and conditions.


###############
# ENVIRONMENT #
###############

# check for required packaages are install them if necessary
list.of.packages <- c("psych","reshape2","car","lme4","ggplot2","tidyverse","data.table","dplyr","lmerTest")          # list of packages
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

# This will clean the data by fixing broken participant labels, remove NA values, remove participants with less than 4 sessions,
# a session variable for each fixation report listed about in the reports array, and compute summary statistics for each participant.
# This is then saved as an object labeled <task>_stats and can be output as a file.

simple.stats.list <- c()                                                                                # this is a list of the objects created by the for loop below (task specific matrices containing subject level statistics) and is used at the end of the for loop

for (z in reports) {
  z = "AntiSaccadeFixationReport.txt" # debugging
  fixation.report <- paste(report.dir,z,sep="")                                                         # path to the fixation report from dataViewer
  
  # read in the report and a table of corrections
  original <- read.table(fixation.report, header = TRUE, sep = "\t", na.strings = ".", dec = ".",fill = TRUE)
  corrections <- read.table(correction.matrix, header = TRUE, sep = "\t", na.strings = ".", dec = ".")
  sessions <- read.table(sessions.matrix, header = TRUE, sep = "\t", na.strings = ".", dec = ".")
  
  # replace broken participant labels
  original$Subject <- as.character(original$RECORDING_SESSION_LABEL)                                      # create the subject column and set it equal to the characters in the recording session labels
  for (i in 1:nrow(corrections)) {                                                                        # for loop to run through the correction data and fix the mistakes in the recording session labels,
    brokenWindow = corrections[i,1]                                                                       # then move those corrections from the subject column to the recording session labels
    brokenWindow = factor(brokenWindow, levels = levels(original$RECORDING_SESSION_LABEL))
    newWindow = corrections[i,2]
    if (is.na(original[original$RECORDING_SESSION_LABEL == brokenWindow, ]$Subject) == FALSE) {
      original[original$RECORDING_SESSION_LABEL == brokenWindow, ]$Subject = as.character(newWindow)
    }
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
  sessions$RECORDING_SESSION_LABEL <- paste("s",sessions$Subject,"c",sessions$Condition,sep="")                            # create a RECORDING_SESSION_LABEL variable for the sessions chart
  original$SESSION = 1                                                                                                     # create the sessions variable for the original dataset and set it equal to integers between 1 and 4 for all entries
  for (i in 1:nrow(sessions)) {                                                                                            # now loop through the session variable in the original report and replace the value with the correct one from the sessions document
    recording.session.label = sessions[i,4]
    session.number = sessions[i,3]
    original[original$RECORDING_SESSION_LABEL == recording.session.label, ]$SESSION = session.number
  }
  
  # aggregate data by subject and session, compute the means and sigma.
  #   fixations
  MeanFix <- aggregate(original$CURRENT_FIX_DURATION, by=list(original$SUBJECT,original$CONDITION,original$SESSION), FUN = mean)
  names(MeanFix) <- c("Subject","Condition","Session","fixMean")
  
  SDFix <- aggregate(original$CURRENT_FIX_DURATION, by=list(original$SUBJECT,original$CONDITION,original$SESSION), FUN = sd)
  names(SDFix) <- c("Subject","Condition","Session","fixSD")
  
  #   saccade amplitude
  MeanSacAmp <- aggregate(original$NEXT_SAC_AMPLITUDE, by=list(original$SUBJECT,original$CONDITION,original$SESSION), FUN = mean)
  names(MeanSacAmp) <- c("Subject","Condition","Session","sacAmpMean")
  
  SDSacAmp <- aggregate(original$NEXT_SAC_AMPLITUDE, by=list(original$SUBJECT,original$CONDITION,original$SESSION), FUN = sd)
  names(SDSacAmp) <- c("Subject","Condition","Session","sacAmpSD")
  
  #   average saccade velocity
  MeanSacVel <- aggregate(original$NEXT_SAC_AVG_VELOCITY, by=list(original$SUBJECT,original$CONDITION,original$SESSION), FUN = mean)
  names(MeanSacVel) <- c("Subject","Condition","Session","sacAvgVelMean")
  
  SDSacVel <- aggregate(original$NEXT_SAC_AVG_VELOCITY, by=list(original$SUBJECT,original$CONDITION,original$SESSION), FUN = sd)
  names(SDSacVel) <- c("Subject","Condition","Session","sacAvgVelSD")
  
  #   make it one table
  task.stats <- merge(MeanFix,SDFix, c("Subject","Condition","Session"))
  task.stats <- merge(task.stats,MeanSacAmp,c("Subject","Condition","Session"))
  task.stats <- merge(task.stats,SDSacAmp,c("Subject","Condition","Session"))
  task.stats <- merge(task.stats,MeanSacVel,c("Subject","Condition","Session"))
  task.stats <- merge(task.stats,SDSacVel,c("Subject","Condition","Session"))
  task.stats <- merge(task.stats,sessions,c("Subject","Condition","Session"))
  task.stats$Task = gsub("(\\w+)FixationReport.txt","\\1",z)
  
  #   save it as object and also write it to a CSV in the output directory
  a <- gsub("(\\w+)FixationReport.txt","\\1",z)                                     # extracts task name
  b <- paste(a,"subj_stats",sep = "_")                                              # creates specific name to be used to assign values in task.stats into an object for future use
  assign(b,task.stats)                                                              # creates object using task name containing same data as task.stats
  write.csv(task.stats,paste(output.dir,b,".csv",sep = ""))                         # outputs a csv of task.stats to the results directory
  simple.stats.list <- append(simple.stats.list,b)                                  # adds object name to a list of the task specific matrices
}

# merge everything into one data frame
all.simple.task.stats <- merge(simple.stats.list[3],merge(simple.stats.list[1],simple.stats.list[2],c("Subject","Condition","Session")),c("Subject","Condition","Session"))

#############################
# SIMPLE MATHS and WIZARDRY #
#############################


# This section preforms all the statistical tests of interest to our study.

# now melt everything so there is just one line per participant, with variables annotated for session and dcast it (see legacy scripts)
hot.cheddar.and.rhye <- melt(task.stats, id=c("Subject","Session","Condition"))                                                     # rearranges the stats so variables are now contained in a single column
tuna.melt <- dcast(hot.cheddar.and.rhye, Subject ~ Session + variable)                                                                 # rearranges data so data is there is one line per subject

# simple correlation - how consistent is everyone across sessions for the search task?
sendIt = paste(output.dir,"/simpleCorrelations.txt",sep = "")                                                                                 # name of output file
write("# This is contains the output of correlations between eye tracking metrics, including both r and p-values.",sendIt,append = FALSE)     # start the output file
write("",sendIt,append = TRUE)

for (i in 1:6) {
  i=i+1
  print(i)
  metric.name <- gsub("\\d_(\\w+)","\\1",names(tuna.melt)[i])                                                                            # get variable name
  write("====================",sendIt,append = TRUE)
  write(metric.name,sendIt,append = TRUE)                                                                                                # put the variable into the output
  write("====================",sendIt,append = TRUE)
  write("",sendIt,append = TRUE)
  a=i+7
  b=i+14
  c=i+21
  mayo <- tuna.melt[c(i,a,b,c)]                                                                                                          # setting variables to compute coefficients for variable of interest
  what.a.mess <- rcorr(as.matrix(mayo),type = "pearson")                                                                                 # rcorr from hmsc - makes a martix of correlation coefficients (r), n, and P values
  write("r",sendIt,append = TRUE)
  write.table(what.a.mess[["r"]],file = sendIt,append = TRUE,sep = "\t",row.names = TRUE, col.names = TRUE)                                               # write coefficients to a table
  write("",sendIt,append = TRUE)
  write("P",sendIt,append = TRUE)
  write.table(what.a.mess[["P"]],file = sendIt,append = TRUE,sep = "\t",row.names = TRUE, col.names = TRUE)                                               # write p-values to same table
  write("",sendIt,append = TRUE)
}

# cor.test gives p-values


##################################
# MADAGASCAR - Playing with LMER #
##################################

#   is there a difference between conditions and what does that look like?
#     fixation duration: lmer
fix.dur = lmer(CURRENT_FIX_DURATION ~ CONDITION + (1 |SUBJECT), data = original)
summary(fix.dur)

#     saccade amplitude: lmer