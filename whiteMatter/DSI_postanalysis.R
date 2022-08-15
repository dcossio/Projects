##############################################################################
#   Title: DSI studio white matter tractography figures                      #
#   Author:  Daniela C                                                       # 
#                                                                            #
#   Purpose: This is a script to look at the correlations between men and    #
#             women in white matter structure                                #
#                                                                            #
#    input: csv files from the DSI output for significant correlations       #
#                                                                            #
#                                                                            #
#    output:                                                                 #
#                                                                            #
#    Notes: based on tbss r studio analysis                                  # 
#                                                                            #
#                                                                            #
##############################################################################



################################Load packages and set wd ##################################

library(ggplot2)
library(dplyr)
library(tidyverse)
library(wrapr)
library(plyr)
library(stringr)
library(BRRR)
library(ppcor)
library(car)
library(afex)


workingdir <-"/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/DSI"
setwd(workingdir)

############################## Load files ##################################

# loading in csv summary that includes all subs (male and females)
# need to check if i can get output for all things here 
#need to be careful because the csv contain headers/footers and r can't read it right 
maleraw <- read_csv("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/DSI/AD/SEX/male/pos_male_stats.csv") 
femaleraw <- read_csv("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/DSI/AD/SEX/female/pos_stats_female.csv") 

maleROIs <- maleraw  %>% filter(str_detect(`Tract Name`,"sub")) 
femaleROIs <- femaleraw %>% filter(str_detect(`Tract Name`,"sub")) %>% select(Fornix_L, Fornix_R, CNIII_R) %>% `colnames<-` (c("Fornix_L_female","Fornix_R_female","CNIII_R"))

subinfo <-read.csv ("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/DSI/midlifeparticipantinfo.csv")  %>% 
  dplyr::select(Subject.ID,Sex,Age.Scan..years.,) %>% 
    `colnames<-` (c("Subject","sex","age")) %>% 
      drop_na()  %>% 
        arrange(.$Subject) 

############################## clean up into new DF ##################################

master_df <- cbind(subinfo,maleROIs)
master_df <- cbind(master_df,femaleROIs[,-1])

