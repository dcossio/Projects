#step 1 of preprocessing Output data for Walking VR 
# Version 

#load packages 
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

# Creating a big data frame for all subs 

finalDF <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), c("participantID","Trial","Time","PosX","PosY","PosZ","FacingAngle","Object","endloc","TrialTime","Results"))


# Create a list of file folders containing participant data
  filelist <- list.files()
  subjlist <- filelist[nchar(filelist) == 8]


for (i in subjlist){
  
    f <- paste0(workdir,"/",i,"/","DSP","/","Wayfinding", "/")
    rawUnity <- read.delim(paste0(f,list.files(f,pattern = "*.txt")),header=FALSE,sep= ",")
   
    print(i)
 
    # first sweep of cleaning involced quite a bit of seperating 
    firstcleandf <-  rawUnity %>% 
                      separate(V1, c("Time","PosX"), ":", fill = "right",extra = "merge") %>%  # seperating the first oclumn into two columns so that we can get the time value and pos x seperated 
                        separate(PosX, c("Trial","PosX"), " ",extra = "merge") %>%  # Further seperating the PosX so that we can get the trial number 
                          separate(PosX, c("PosX","TimeSpent"), ":", fill = "right",extra = "merge") # separating PosX

    colnames(firstcleandf)[colnames(firstcleandf) %in% c("V2", "V3","V4")] <- c("PosY", "PosZ","FacingAngle")


    # make a dataframe that we'll use later 
    DF <- as.data.frame(matrix(ncol = 9, nrow = 0)) 

    # second clean up 
    beginloc <- which(firstcleandf$PosX == "Begins")
    endloc <- which(firstcleandf$PosX =="Ends")

    for( L in 1:length(beginloc)){
        x <- firstcleandf[c(beginloc[L]): (endloc[L]), ] %>% mutate(Trial = Trial[1]) %>% mutate(Object = PosX[2]) 
        temp <-x %>% filter(Time=="Trial Result") %>%  pivot_longer(PosX) 
        x <- x %>% mutate(Results= str_split_fixed(temp$value, "[ ]", 2) [1]) %>% mutate(participantID = str_split_fixed(rawUnity[1,1], "[:]", 2) [2]) %>% mutate(TrialTime= temp$TimeSpent)
        DF <- rbind(DF,x) 
    }


    # BIG CLEAN and split 
    DF2 <- DF %>% 
            separate(TrialTime, c("objend","TrialTime"), ":", fill = "right",extra = "merge") %>% 
              filter(Time != "Trial No." ) %>%  
                filter(Time != "Trial Result" ) %>% 
                  filter(Time !=  "User confirmed on object")  %>% 
                    separate(objend, c("trash","endloc" , "b")," ", fill = "right",extra = "merge")


    subDF<-DF2 %>% 
              select("participantID","Trial","Time","PosX","PosY","PosZ","FacingAngle","Object","endloc","TrialTime","Results")

   finalDF <- rbind(finalDF,subDF)

}
 
  write.csv(finalDF, "DSPBehaviorFile.csv")
 
 
