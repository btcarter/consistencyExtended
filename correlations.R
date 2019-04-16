# Author: Benjamin Carter
# Date: 2019-02-05
# Project: Eye Movement Consistency Extended
# Purpose: Preprocess visual search data and then perform statistics to determine the consistency of eye movements over time
#          and if there is a difference between sessions and conditions.


##### ENVIRONMENT #####

# check for required packaages are install them if necessary
list.of.packages <- c("reshape2","Hmsic")                                                                             # list of packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]                           # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)                                          # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                                                # load packages


###### VARIABLES AND PATHS #####
report.dir <- "~/Box/LukeLab/Caffeine/eyelinkData/reports/"
reports <- c("AntisaccadeFixationReport.txt","ProsaccadeFixationReport.txt","SearchFixationReport.txt","ReadingFixationReport.txt")        # names of the fixation reports as an array
output.dir <- "~/Box/LukeLab/Caffeine/results/"                                                                                            # a path to the output destination
correction.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/subjectCorrections.txt"                                                # this is the matrix containing all the errors and all the corrections
sessions.matrix <- "~/Dropbox/Lab data & Papers/analyses/caffeine/participantList.txt"                                                     # a path to the sessions list


# split Antisaccade fixation report into two, one for antisaccades and one for prosaccades
saccades <- paste(report.dir,"AntiSaccadeFixationReport1.txt",sep="")
saccades <- read.table(saccades,header=TRUE,sep="\t",na.strings=".",dec=".",fill=TRUE)
anti <- subset(saccades, task == "antisaccade")                                                                                                                  # make antisaccade report
pro <- subset(saccades, task == "prosaccade")                                                                                                                    # make prosaccade report
write.table(anti, file = "~/Box/LukeLab/Caffeine/eyelinkData/reports/AntisaccadeFixationReport.txt",sep = "\t",na = ".",col.names = TRUE,row.names = FALSE)      # write it out as a .tab
write.table(pro, file = "~/Box/LukeLab/Caffeine/eyelinkData/reports/ProsaccadeFixationReport.txt",sep = "\t",na = ".",col.names = TRUE,row.names = FALSE)        # write it out as a .tab

##### PREPROCESSING #####
# This will clean the data by fixing broken participant labels, remove NA values, remove participants with less than 4 sessions,
# a session variable for each fixation report listed about in the reports array, and compute summary statistics for each participant.
# This is then saved as an object labeled <task>_stats and can be output as a file.


# a preprocessing function
participant.stats <- function(z,correction.matrix,sessions.matrix) {
  fixation.report <- paste(report.dir,z,sep="")                                                           # path to the fixation report from dataViewer
  
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
  sessions$RECORDING_SESSION_LABEL <- paste("s",sessions$Subject,"c",sessions$Condition,sep="")                            # create a RECORDING_SESSION_LABEL variable for the sessions chart
  original$SESSION = 10                                                                                                    # create the sessions variable for the original dataset and set it equal to integers between 1 and 4 for all entries
  for (i in 1:nrow(sessions)) {                                                                                            # now loop through the session variable in the original report and replace the value with the correct one from the sessions document
    recording.session.label = sessions[i,4]
    session.number = as.numeric(sessions[i,3])
    original$SESSION[original$RECORDING_SESSION_LABEL == recording.session.label] = session.number
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
  
  #   return resulting data.frame
  return(task.stats)
}

# Use function to assemble single data.frame
simple.stats.df <- data.frame()                                                                       # unified data.frame for function output.

for (i in reports) {
  preprocessed <- participant.stats(i,correction.matrix,sessions.matrix)
  simple.stats.df <- rbind(simple.stats.df,preprocessed) 
}

write.csv(simple.stats.df,paste(output.dir,"taskStats",".csv",sep = ""),row.names = FALSE)            # write data.frame to a csv in the output directory, omit row names

###### SIMPLE MATHS and WIZARDRY ######
# This section preforms all the correlational tests of interest to our study.

# melt everything so there is just one line per participant, with variables annotated for session and dcast it (see legacy scripts)
simple.stats.df$Subject <- as.numeric(simple.stats.df$Subject)                                                                               # for some reason the Subject column needs to be turned into a numeric (why is that not the default assumption?).
hot.cheddar.and.rhye <- melt(simple.stats.df, id=c("Subject","Session","Task"))                                                              # rearranges the stats so variables are now contained in a single column
tuna.melt <- dcast(hot.cheddar.and.rhye, Subject ~ Session + Task + variable)                                                                # rearranges data so data is there is one line per subject

# simple correlation - how consistent is everyone across sessions for a task?
#   across sessions, within tasks
sendIt = paste(output.dir,"/simpleCorrelationsByTask.txt",sep = "")                                                                           # name of output file
write("# This is contains the output of correlations between eye tracking metrics, including both r and p-values.",sendIt,append = FALSE)     # start the output file
write("",sendIt,append = TRUE)
for (i in c(1:6,9:14,17:22,25:30)) {
    i=i+2
    metric.name <- gsub("\\d_(\\w+)","\\1",names(tuna.melt)[i])                                                                            # get variable name
    write("====================",sendIt,append = TRUE)
    write(metric.name,sendIt,append = TRUE)                                                                                                # put the variable into the output
    write("====================",sendIt,append = TRUE)
    write("",sendIt,append = TRUE)
    a=i+32
    b=a+32
    c=b+32
    mayo <- tuna.melt[c(i,a,b,c)]                                                                                                          # setting variables to compute coefficients for variable of interest
    what.a.mess <- rcorr(as.matrix(mayo),type = "pearson")                                                                                 # rcorr from hmsc - makes a martix of correlation coefficients (r), n, and P values
    write("r",sendIt,append = TRUE)
    write.table(what.a.mess[["r"]],file = sendIt,append = TRUE,sep = "\t",row.names = TRUE, col.names = TRUE)                                               # write coefficients to a table
    write("",sendIt,append = TRUE)
    write("P",sendIt,append = TRUE)
    write.table(what.a.mess[["P"]],file = sendIt,append = TRUE,sep = "\t",row.names = TRUE, col.names = TRUE)                                               # write p-values to same table
    write("",sendIt,append = TRUE)
  }
  

#   across tasks, within a session - fix the intervals
sendIt = paste(output.dir,"/simpleCorrelationsBySession.txt",sep = "")                                                                        # name of output file
write("# This is contains the output of correlations between eye tracking metrics, including both r and p-values.",sendIt,append = FALSE)     # start the output file
write("",sendIt,append = TRUE)
for (i in c(1:4)) {
  i=3+((i-1)*32)
  metric.name <- gsub("(\\d)_\\w+","Session_\\1",names(tuna.melt)[i])                                                                            # get variable name
  write("====================",sendIt,append = TRUE)
  write(metric.name,sendIt,append = TRUE)                                                                                                # put the variable into the output
  write("====================",sendIt,append = TRUE)
  write("",sendIt,append = TRUE)
  a=i+1 # antisaccade
  b=i+2
  c=i+3
  d=i+4
  e=i+5
  f=i+8 # prosaccade
  g=i+9
  h=i+10
  j=i+11
  k=i+12
  l=i+13
  m=i+16 # reading
  n=i+17
  o=i+18
  p=i+19
  q=i+20
  r=i+21
  s=i+24 # search
  t=i+25
  u=i+26
  v=i+27
  w=i+28
  x=i+29
  mayo <- tuna.melt[c(i,a,b,c,d,e,f,g,h,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)]                                                                  # setting variables to compute coefficients for variable of interest
  what.a.mess <- rcorr(as.matrix(mayo),type = "pearson")                                                                                 # rcorr from hmsc - makes a martix of correlation coefficients (r), n, and P values
  write("r",sendIt,append = TRUE)
  write.table(what.a.mess[["r"]],file = sendIt,append = TRUE,sep = "\t",row.names = TRUE, col.names = TRUE)                                               # write coefficients to a table
  write("",sendIt,append = TRUE)
  write("P",sendIt,append = TRUE)
  write.table(what.a.mess[["P"]],file = sendIt,append = TRUE,sep = "\t",row.names = TRUE, col.names = TRUE)                                               # write p-values to same table
  write("",sendIt,append = TRUE)
}
