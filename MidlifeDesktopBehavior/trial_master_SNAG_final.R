##############################################################################
#     Title: Trial Master for SNAG midlife                                   #
#     Author: Vaisakh P & Daniela C                                         #
#                                                                            #
#   Purpose: This is the second script in the pipeline and requires the      #
#            output from the behavior preprocessing script. Here all rows    #
#             will not be summarized by trial number and variables will be   #
#             extracted                                                      #
#                                                                            #
#   Input: Behavioral.csv file from the first script                         #
#                                                                            #
#   Output: Trial.csv file                                                   #
#                                                                            #
#                                                                            #
##############################################################################
# set working directory here to where the master csv file is located along with the location and dista
working_dir <-  "/Volumes/GoogleDrive/My Drive/MLINDIV_SNAG_preprocessing/Raw sub data"
setwd(working_dir)

# output from explore script 
master_file <- read.csv("MLINDIV_behavioral_master.csv")

library(tidyverse)
library(plyr)
library(dplyr)

#remove block trials from test trials from Master
blockrow_locs <-which(is.na(master_file$ImageFile))  #finding block locations 
master_file <- master_file[-c(blockrow_locs), ] #indexing them out of the master file 

 
# splitting explore and trial rows and process separately 
 trial_df <- master_file[which(c(master_file$Task =="TrialProc")),] # new df with onl trial
 c <- which(c(FALSE, tail(trial_df$Sample,-1) != head(trial_df$Sample,-1))) # indexing by Sample getting locations where sample changes 
 splitrows <- sort(c(c)) # originally combined s and tt rows but we only have one way to split rows so we may not need this 
 splitrows <- splitrows - 1 # idk whatevr rob did so i kept it anyways 
 meta_trial <- split(trial_df, cumsum(1:nrow(trial_df) %in% (splitrows+1))) # creating the meta list just for trials using robs code and idk still 

#REpeating above but for just explore
explr_df <- master_file[which(c(master_file$Task =="ExploreProc")),] # new df with onl explore
explr_d <- which(c(FALSE, tail(explr_df$Task_type,-1) != head(explr_df$Task_type,-1))) # originally was cycle 
explr_splitrows <- sort(explr_d) # originally combined s and tt rows but we only have one way to split rows so we may not need this 
explr_splitrows <- explr_splitrows - 1 # idk whatevr rob did so i kept it anyways 
meta_explr<- split(explr_df, cumsum(1:nrow(explr_df) %in% (explr_splitrows+1))) # creating the meta list just for trials using robs code and idk still 

#merge lists? 
#meta is combining both trialand epxlore together 
meta <- append(meta_explr,meta_trial)

# Meta contains all the different trials within each procedure ran for each participant
paths <- c()
eprocs <- c()
Subject <- c()
Task <- c()
Task_type <- c()
Procedure <- c()
Sample <- c()
startPosition <- c()
startFacing <- c()
EndIm <- c()
StartIm <- c()
Vid_paths <- c()
select_location <- c()
end_rotation <- c()
accuracy <- c()
nodesturns_count <- c()
nodes_count <- c()
average_RT <- c()
trial_starttime <- c()
trial_endtime <- c()
trial_duration <- c()
turns_count <- c()
select_made <- c()
le <- c()
final_movevid_OT <- c()
mvtrial_duration <- c()
e_paths <- c()

print("before Loop")

for (i in 1:length(meta)){
  print(i)
  #Path Hallsnip Vectors
  pvid <- meta[[i]]["hallsnip"][[1]]
  last_vp <- tail(pvid, 1)
  pvid <- paste0(pvid[!is.na(pvid)], collapse=" ")
  Vid_paths <- c(Vid_paths, pvid)
  
  # Selection Made
  select_made <- c(select_made, last_vp == "Selec")
  
  # Path vectors
  path <- meta[[i]]["letter_loc"][[1]]
  path <- paste0(path[!is.na(path)], collapse=" ")
  path <- paste(substr(pvid, 1, 1), path)
  paths <- c(paths, path)
  
  # Expanded path vectors
  e_path <- substr(meta[[i]]["end_location"][[1]], 1, 2)
  e_path <- paste0(e_path[!is.na(path)], collapse=" ")
  e_paths <- c(e_paths, e_path)
  
  # Eprime Procedure file name
  eproc_name <- as.character(meta[[i]]["Eprime.Basename"][[1]][1])
  eprocs <- c(eprocs, eproc_name)
  
  # Subject number
  subject <- as.character(meta[[i]]["Subject"][[1]][1])
  Subject <- c(Subject, subject)
  
  # Task
  task <- as.character(meta[[i]]["Task"][[1]][1])
  Task <- c(Task, task)
  
  # Task_type our task type from the previous script grabs it wrong so i've adjusted it by making it grab "cycle"
  task_type <- as.character(meta[[i]]["Task_type"][[1]][1])
  Task_type <- c(Task_type, task_type)
  
  # Procedure
  proc <- as.character(meta[[i]]["Procedure"][[1]][1])
  Procedure <- c(Procedure, proc)
  
  # Sample
  sample <- as.character(meta[[i]]["Sample"][[1]][1])
  Sample <- c(Sample, sample)
  
  # startPosition
  sp <- as.character(meta[[i]]["startPosition"][[1]][1])
  startPosition <- c(startPosition, sp)
  
  # startFacing
  sf <- as.character(meta[[i]]["startFacing"][[1]][1])
  startFacing <- c(startFacing, sf)
  
  # StartIm
  si <- substr(as.character(meta[[i]]["StartIm"][[1]][1]), 8, 8)
  if (si == "V" & !is.na(si)){ si <- "Y"}
  StartIm <- c(StartIm, si)
  
  # EndIm
  ei <- substr(as.character(meta[[i]]["EndIm"][[1]][1]), 8, 8)
  if(ei == "V" & !is.na(ei)){ei <- "Y"}
  EndIm <- c(EndIm, ei)

  # end_location
  nc <- nchar(path)
  last <- substr(path, nc, nc)
  select_location <- c(select_location, last)
  
  # end_rotation
  er <- meta[[i]]["face_dir"][[1]]
  er <- tail(er[!is.na(er)], 1)
  end_rotation <- c(end_rotation, er)
  
  # v-----Custom Calculations-----v
  
  # Accuracy
  ac <- last == ei
  accuracy <- c(accuracy, ac)
  
  # Nodes Turns Count
  nodes <- strsplit(path, " ")[[1]]
  ncount <- length(nodes)
  nodesturns_count <- c(nodesturns_count, ncount)
  
  # Nodes Count (no turns)
  vp <- strsplit(pvid, " ")[[1]]
  vp <- length(vp[substr(vp, 1, 1) != substr(vp, 4, 4)])
  nodes_count <- c(nodes_count, vp)
  
  # Turn Count
  tc <- ncount- vp
  turns_count <- c(turns_count, tc)
  
  # Average Response Time
  avRT <- mean(as.numeric(meta[[i]]["Choose.RT"][[1]]), na.rm=TRUE)
  average_RT <- c(average_RT, avRT)
  
  # Trial Completion Duration
  starttime <- as.numeric(as.character(meta[[i]]["Choose.OnsetTime"][[1]][1]))
  if (proc == "ExploreProc"){  # this is where i changed it from is.na to if procedure =explore
    endtime <- as.numeric(as.character(meta[[i]]["Finished.OnsetTime"][[1]][1]))
  } else {
    endtime <- as.numeric(as.character(meta[[i]]["ITI.OnsetTime"][[1]][1]))
  }

  tctime <- endtime - starttime
  
  if (last_vp == "Selec" & !is.na(last_vp)){ tctime <- tctime - 1000} # Compansating for duration of Selection made vid duration
  trial_duration <- c(trial_duration, tctime)
  
  
  # Trial Start Time
  trial_starttime <- c(trial_starttime, starttime)
  
  # Trial Completion Time
  trial_endtime <- c(trial_endtime, endtime)
  
  # Final MoveVid Onset Time
  fmvot <- as.integer(as.character(tail(meta[[i]]["MoveVid.OnsetTime"][[1]], 1)))
  final_movevid_OT <- c(final_movevid_OT, fmvot)
  
}

#i removed the variables that we dont have by commenting them at the end of this little chunk of text
trial_master <- data.frame(Subject = Subject, eprocs = eprocs, Task_type = Task_type, Procedure = Procedure,
                           Sample = Sample,startPosition = startPosition, 
                           startFacing = startFacing, paths = paths, e_paths = e_paths, Vid_paths = Vid_paths, select_made = select_made, StartAt = StartIm, EndAt = EndIm, 
                           end_location = select_location, end_rotation = end_rotation, accuracy = accuracy, 
                           nodesturns_count = nodesturns_count, nodes_count = nodes_count, turns_count = turns_count, average_RT = average_RT,
                           final_movevid_OT = final_movevid_OT, trial_starttime = trial_starttime, trial_endtime = trial_endtime, trial_duration = trial_duration #, objlist = objlistPairList = PairList ITIDur = id, ObjDur = od
)

dtable <- read.csv("distancetable.csv")
dtable[30,1] <- "P"
dtable[44, 1] <- "P"
colnames(dtable)[1] <- "StartAt"
colnames(dtable)[2] <- "EndAt"

t <- join(trial_master, dtable, by = c("StartAt", "EndAt"))

t <- t %>% mutate(fmv_duration = trial_endtime - final_movevid_OT)

#Recoding wayfinding trials that were supposed to end at N but actually ended at F facing south as true, as they are close enough to being true. Likewise,
#recoding trials that were supposed to end at Y but actually ended at V facing east as true. 
recode_trials_1<- which(t$Procedure=='TrialProc'& t$EndAt == 'N' & t$end_location =='F' & t$end_rotation=='S') #finding rows satisfying first condition 
recode_trials_2<- which(t$Procedure=='TrialProc'& t$EndAt == 'Y' & t$end_location =='V' & t$end_rotation=='E')#finding rows satisfying second condition
recode_trials<-append(recode_trials_1,recode_trials_2) #combining into one list
t[recode_trials,16]<-"TRUE" #recoding these trials to TRUE 

print("writing CSV")
write.csv(t, "MLINDIV_trial_master.csv")# Split master file into Explore and Tests

print ("All Done")

#skrrrahh(0)
