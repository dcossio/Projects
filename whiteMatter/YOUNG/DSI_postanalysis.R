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
library(stringr)
library(BRRR)
library(ppcor)
library(car)
library(afex)
library(ggpubr)

workingdir <-"/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/DSI"
setwd(workingdir)

############################## Load files ##################################

# loading in csv summary that includes all subs (male and females)
# need to check if i can get output for all things here 
#need to be careful because the csv contain headers/footers and r can't read it right 
maleraw <- read_csv("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/DSI/AD/SEX/male/pos_male_stats.csv") 
femaleraw <- read_csv("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/DSI/AD/SEX/female/pos_stats_female.csv") 

maleROIs <- maleraw  %>% filter(str_detect(`Tract Name`,"sub")) 
femaleROIs <- femaleraw %>% filter(str_detect(`Tract Name`,"sub")) %>% dplyr::select(Fornix_L, Fornix_R, CNIII_R) %>% `colnames<-` (c("Fornix_L_female","Fornix_R_female","CNIII_R"))

subinfo <-read.csv ("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/DSI/midlifeparticipantinfo.csv")  %>% 
  dplyr::select(Subject.ID,Sex,Age.Scan..years., MAZE.Accuracy....,log.acc ) %>%
    `colnames<-` (c("Subject","sex","age","Accuracy", "logAcc")) %>% 
      drop_na()  %>% 
        arrange(.$Subject) 

############################## clean up into new DF ##################################

master_df <- cbind(subinfo,maleROIs)
master_df <- cbind(master_df,femaleROIs[,-1])


# based on DSI output both men and women had significant tracts in fornix L and R so 
# i'm going to focus on those 

# Fornix_L
FornixLdf <- master_df %>% dplyr::select(Subject,sex,Fornix_L,Accuracy,logAcc)
sam_size <- master_df %>% dplyr::group_by(sex)%>%dplyr::summarize(num=n())

#Violin
FornixLdf %>%
  left_join(sam_size) %>%
  mutate(myaxis=paste0(sex,"\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=Fornix_L, fill=sex)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  scale_colour_brewer() +
  theme(legend.position="none", plot.title = element_text(size=11)) +
  ggtitle("AD Fornix_L") +
  xlab("") +
  ylab("Fornix_L")

# scatter plot 
# Left Fornix

jpeg("LeftFornixAD.jpeg")
ggplot(data = FornixLdf, aes(x = Fornix_L, y = Accuracy)) +
  geom_point(aes(shape = factor(sex))) +
  geom_point(aes(color = factor(sex))) +
  geom_smooth(method = "glm", 
              aes(color = factor(sex))) +
  labs(title = "Correlation between Proportion correct and AD in men vs women",
       x = "Left Fornix",
       y = "Proportion Correct")+
    theme(legend.position = "right") +
    stat_cor(aes(color= sex),method = "spearman", label.x.npc = 0.025, label.y.npc = 0.75) +
    stat_regline_equation(aes(color=sex)) +
    xlim(min(FornixLdf$Fornix_L), max(FornixLdf$Fornix_L)) +
    font("xlab", size=15)+
    font("ylab", size=15)
dev.off()

# Right Fornix
# Fornix_L


FornixRdf <- master_df %>% dplyr::select(Subject,sex,Fornix_R,Accuracy,logAcc)
sam_size <- master_df %>% dplyr::group_by(sex)%>%dplyr::summarize(num=n())


jpeg("RightFornixAD.jpeg")
ggplot(data = FornixRdf, aes(x = Fornix_R, y = Accuracy)) +
  geom_point(aes(shape = factor(sex))) +
  geom_point(aes(color = factor(sex))) +
  geom_smooth(method = "glm", 
              aes(color = factor(sex))) +
  labs(title = "Correlation between Proportion correct and AD in men vs women",
       x = "Right Fornix",
       y = "Proportion Correct")+
  theme(legend.position = "right") +
  stat_cor(aes(color= sex),method = "spearman", label.x.npc = 0.025, label.y.npc = 0.75) +
  stat_regline_equation(aes(color=sex)) +
  xlim(min(FornixRdf$Fornix_R), max(FornixRdf$Fornix_R)) +
  font("xlab", size=15)+
  font("ylab", size=15)
dev.off()

    
    
    
  



# Fornix R






