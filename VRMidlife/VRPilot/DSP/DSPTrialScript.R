# second part of preprocessing 
# using for DSP wayfinding an analysis
# this will output a csv with all the trial information for subjects. 

library(ggplot2)
library(dplyr)
library(tidyverse)
library(wrapr)
library(plyr)
library(stringr)
library(BRRR)


# set our working directory 

workdir <-"/Volumes/GoogleDrive/My Drive/Midlife_walkingVR_Pilot/midlifeVRpilot_2022/behavior_data"

setwd (workdir)


#Read in files
behfile <- read_csv(paste0(workdir,"/","DSPBehaviorFile.csv"))

y <-which(c(FALSE, tail(behfile$Trial,-1) != head(behfile$Trial,-1)))
#x <- which(c(TRUE, tail(behfile$Trial,-1) != head(behfile$Trial,-1)))
#x <-x-1
splitrows <- sort(c(y)) # originally combined s and tt rows but we only have one way to split rows so we may not need this 
splitrows <- splitrows - 1 # idk whatevr rob did so i kept it anyways 
meta_trial <- split(behfile, cumsum(1:nrow(behfile) %in% (splitrows+1)))

#Variables we need 
ID <-c()
timepoint <-c()
TrialID <-c()
PosX<-c() 
PosY <-c()         
PosZ <-c()        
FacingAngle<-c()   
startObj <-c()        
endObj<-c()       
TrialTime <-c()   
Result<-c()
accuracy<- c()



for (i in 1:length(meta_trial)){
  print(i)
  # ParticipantID
  subID <- meta_trial[[i]][["participantID"]][1]
  ID <-c(ID,subID)
  
  #number of Time points 
  timepoints <- length((meta_trial[[i]][["X1"]])) # on mac it was ..1 and laptop its X1. I'm just grabbing the very first column of behfile
  timepoint <- c(timepoint,timepoints)
  
  #Trial number 
   TrialIDs<- meta_trial[[i]][["Trial"]][1]
   TrialID <-c(TrialID,TrialIDs)
  
  # PosX<-() equal to the difference from start and end of trial 
   startX<- head(meta_trial[[i]][["PosX"]],n=1)
   endX <- tail(meta_trial[[i]][["PosX"]],n=1)
   Posx<- endX-startX
   PosX<-c(PosX,Posx)
   
   
   # PosY<-() equal to the difference from start and end of trial 
   startY<- head(meta_trial[[i]][["PosY"]],n=1)
   endY <- tail(meta_trial[[i]][["PosY"]],n=1)
   Posy<- endY-startY
   PosY<-c(PosY,Posy)
   
   # PosZ<-() equal to the difference from start and end of trial 
   startZ<- head(meta_trial[[i]][["PosZ"]],n=1)
   endZ <- tail(meta_trial[[i]][["PosZ"]],n=1)
   Posz<- endZ-startZ
   PosZ<-c(PosZ,Posz)
   
  #Facing
   #average
   FA<- mean(meta_trial[[i]][["FacingAngle"]])
   FacingAngle<-c(FacingAngle,FA)
   # or difference
   # starFZ<- head(meta_trial[[i]][["FacingAngle"]],n=1)
   # endF <- tail(meta_trial[["i]][["FacingAngle"]],n=1)
   # FacingAngle <- endF-startF
   
  # Start object 
   startObjs<-meta_trial[[i]][["Object"]][1]
   startObj <-c(startObj,startObjs)
   
 # Endobject is the object they ended on.
   endObjs<-(meta_trial[[i]][["endloc"]])[1]   
   endObj<-c(endObj,endObjs) 
   
   # Trial time .
   TrialTimes <- meta_trial[[i]][["TrialTime"]][1]
   TrialTime <-c(TrialTime,TrialTimes)
   
   #succes or fail 
   Results<- meta_trial[[i]][["Results"]][1]
   Result<- c(Result,Results)
   
   #Accuracy
    ac <- Results=="Success"
    accuracy <- c(accuracy,ac)
}
#i removed the variables that we dont have by commenting them at the end of this little chunk of text
trial_master <- data.frame(ID=ID,timepoint=timepoint,TrialID =TrialID,PosX= PosX, PosY=PosY,PosZ=PosZ,
                           FacingAngle=FacingAngle, startObj=startObj,endObj= endObj,      
                           TrialTime=TrialTime,Result=Result,accuracy=accuracy)


write.csv(trial_master, "DSFPTrials.csv", col.names =TRUE)




