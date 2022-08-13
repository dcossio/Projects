##############################################################################
#   Title: Behavior analysis                                                 #
#   Author:  Daniela C                                                       # 
#                                                                            #
#   Purpose: This is to analyze sex differencec in behavior both test        #
#            and exploration. This usesparts of viasakhs and kates code      #
#                                                                            #
#    input: Trial.csv and participant.csv and participant info               #
#                                                                            #
#                                                                            #
#    output: clean csv with summary data for each subject                    #
#                                                                            #
#    Notes:  make sure to have the location.csv, distance.csv                # 
#           and path distance.csv when running the pipeline                  #
#                                                                            #
##############################################################################

#-------------------------------------------------------------------------------
#load in packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(network)
library(tidygraph)
library(ggraph)
library(igraph)
library(networkD3)
library(CINNA)
library(umap)
library(plotly)
library(factoextra)
library(lsr)
library(car)
library(ggpubr)
library(entropy)
library(ds4psy)
library(pROC)
library(devtools)


#------------------------------------------------------------------------------- 
workdir<- "/Users/danielacossio/Desktop/MLINDIV_analysisDC/MidlifeDesktopBehavior/"
setwd(workdir)

# read files in 

#infosheet with age/sex
subjInfo<-read.csv ("MAZE_data_old.csv")  %>% select(Subject.ID,Sex,Age.Scan..years.,) %>% `colnames<-` (c                     ("Subject","sex","age")) %>% drop_na()  %>% arrange(.$Subject)

#preprocessed data 
trialDF<-read.csv ("MLINDIV_trial_master.csv") %>% filter(Subject %in% c(as.character(subjInfo$Subject))) 

participantDF<-read.csv ("MLINDIV_participant_master.csv") %>%  filter(Subject %in% c(as.character          (subjInfo$Subject)))

master_trial <- inner_join(subjInfo, trialDF, by="Subject") 
master_participant <- inner_join(subjInfo, participantDF, by="Subject") 


#------------------------------------------------------------------------------- 
# prepping variables for later use 
locs <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', "Y", "Z")
objs_orientations<- c('A4', 'I2', 'K4', 'L2', 'N3', 'O2', 'P2', 'Y2', 'W4')
objects<-c('A', 'I', 'K', 'L', 'N', 'O', 'P', 'Y', 'W')
hallways <- c('B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'M', 'Q', 'R', 'S', 'T', 'U', "V", 'X', "Z")

#A=guitar
#I=snowman
#L=spaceship
#K=lamp post
#N=chicken
#O=trophy
#P=chair
#Y=umbrella
#W=cuckoo clock

#ttest output colnames for data later 
#colttest <-  c("estimate","statistic", "p.value", "parameter"," conf.low"," conf.high"," method","alternative")


#------------------------------------------------------------------------------- 
# A little more processing to grab paths of explore sessions
# grab explore data

explr <- master_trial %>% filter(is.na(accuracy) == TRUE) %>% filter(Task_type == 1 | Task_type == 2) # 


explrDF<- data.frame(matrix(nrow = nrow(explr)))#creating empty dataframe to store contents of for loop into 


for (i in 1:nrow(explr)){
  e_string_orient<-as.list(strsplit(explr$e_paths[i],split=" ")) #splitting exploration path trajectory (with orientations) of subject into individual strings and storing as list
  e_string<-as.list(strsplit(explr$paths[i],split=" ")) #splitting exploration path trajectories (w/o orientations) of subject into individual strings 
  F3_index<-which(sapply(e_string_orient[[1]], FUN=function(X) "F3" %in% X))+1 #finding which element in string list is F3, and we need to add a constant of 1 to 
  #match this up to where the same element would be in the "paths" variable. If element doesn't exist, R takes this into account and doesn't add a 1 (i.e., it keeps the
  #index value as 'non existent' even after adding a 1)
  e_string[[1]][F3_index]<-"N" #replacing with N
  V2_index<-which(sapply(e_string_orient[[1]], FUN=function(X) "V2" %in% X))+1#finding which element in string list is V2, and we need to add a constant of 1 to 
  #match this up to where the same element would be in the "paths" variable. If element doesn't exist, R takes this into account and doesn't add a 1 (i.e., it keeps the
  #index value as 'non existent' even after adding 1)  
  e_string[[1]][V2_index]<-"Y" #replacing with Y
  e_string<-paste(e_string[[1]], collapse = " ") #collapsing exploration path trajectory into single string list 
  explrDF[i,1]<-e_string #storing transposed version into dataframe
}


explrDF$Subject<-explr$Subject #adding subject ID to dataframe 
explrDF <- explrDF %>% rename(exploration_paths #renaming column containing exploration trajectories 

master_participant <- inner_join(master_participant,explrDF)
#------------------------------------------------------------------------------- 

