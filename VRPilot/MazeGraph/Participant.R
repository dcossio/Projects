############################################################
##                                                         ##
##  AUTHOR: DANIELA COSSIO                                 ##
##                                                         ##
##  PURPOSE: THIS IS THE FINAL STEP IN PREPROCESSING       ##
##           THE UNITY OUTPUT FROM THE VR WALKING          ##
##           MAZE GRAPH TASK. HERE THE CSV FROM THE 2ND    ##
##           STEP IS SUMMARIZED FOR EACH SUBJECT           ##
##                                                         ##
##  INPUT : TRIALS CSV OUTPUT                              ##
##                                                         ##
##  OUPUT: A CSV FILE WITH A ROW FOR EVERY SUBJ            ##
##                                                         ##
##  NOTES: BE CAREFUL WITH THE PARTICIPANT INFO SHEET,     ##
##        THAT PART MAY NOT BE VERY FLEXIBLE               ##
##                                                         ##
############################################################


library(ggplot2)
library(dplyr)
library(tidyverse)
library(wrapr)
library(plyr)
library(stringr)
library(BRRR)
library(autoimage)


# New script who dis? starting fresh 
  rm(list=ls()) 


# set our working directory 

workdir <-"/Volumes/GoogleDrive/My Drive/Midlife_walkingVR_Pilot/midlifeVRpilot_2022/behavior_data"

setwd (workdir)

#Read in files
tm <- read_csv(paste0(workdir,"/","Trials.csv"))

# splitting by subjects
tm_split <- split( tm, tm$ID)

# creating empty tibbles
n_trials <- c()
n_explores <- c()
n_selects <- c()
n_corrects <- c()
n_outoftimes <- c()
ID <-c()
accuracy<-c()
objlocs<- c()
endobjlocs<- c()
accuracy_sd <- c()

# 


# loop to index each of the lists and grab the correct variable
  for (i in 1:length(tm_split)){
  
    IDs <- tm_split[[i]][["ID"]][1]
    n_trial <- length(tm_split[[i]][[1]]) 
    n_select <- sum(!is.na((tm_split[[i]][["endObj"]]))) # the only way we can see which 
    n_correct <- sum(tm_split[[i]][["accuracy"]], na.rm = TRUE)
    n_outoftime <- sum(is.na((tm_split[[i]][["endObj"]])))
    
    acc <- mean(tm_split[[i]][["accuracy"]], na.rm = TRUE)
    acc_sd <- sd(tm_split[[i]][["accuracy"]], na.rm = TRUE)
 
    ID <- c(ID,IDs) 
    n_trials <- c(n_trials, n_trial)
    n_selects <- c(n_selects, n_select)
    n_corrects <- c(n_corrects, n_correct)
    n_outoftimes <- c(n_outoftimes, n_outoftime)
    accuracy <- c(accuracy,acc)
    accuracy_sd<- c(accuracy_sd,acc_sd)
    }



df <-data.frame(ID=ID, n_trials=n_trials, n_selects=n_selects,n_corrects=n_corrects,n_outoftimes=n_outoftimes,accuracy=accuracy,
                accuracy_sd=accuracy_sd )


subjinfo <- read_csv(paste0(workdir,"/","midlife_pilotVR_participantsheet.csv")) %>% select(ID,Age,Sex)

df <- inner_join(df, subjinfo)

df <- df[,c(1,8,9,2,3,4,5,6,7)]


write_csv(df, "participantfile.csv")




#For later to grab objects
# 
# # grabbing  frequency of start objects really unnecessary  objects 
# q <-data.frame(t(count(tm_split[[10]][["startObj"]]))) 
# temp <- q %>% `colnames<-`(c(as.character(q[1,]))) %>% slice(-c(1)) %>% `row.names<-`(c(ID))
# objlocs<- rbind(objlocs,temp) 
# 
# # end objects
# c <-data.frame(t(count(tm_split[[10]][["endObj"]]))) %>% `row.names<-`(c(rep(ID,2)))   
# temp2 <- c %>%  slice(-c(1)) %>% `row.names<-`(c(rep(ID,2)))  
# endobjlocs<- rbind(endobjlocs,temp2) 



