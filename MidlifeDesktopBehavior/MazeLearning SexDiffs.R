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

########## load in packages ###########
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
library(BRRR)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
###############  Reading in files #######################
workdir<- "/Users/danielacossio/Desktop/MLINDIV_analysisDC/MidlifeDesktopBehavior/"
setwd(workdir)

# read files in 

#infosheet with age/sex
subjInfo<-read.csv ("MAZE_data_old.csv")  %>% select(Subject.ID,Sex,Age.Scan..years.,) %>% `colnames<-` (c("Subject","sex","age")) %>% drop_na()  %>% arrange(.$Subject)

#preprocessed data 
trialDF<-read.csv ("MLINDIV_trial_master.csv") %>% filter(Subject %in% c(as.character(subjInfo$Subject))) 

participantDF<-read.csv ("MLINDIV_participant_master.csv") %>%  filter(Subject %in% c(as.character(subjInfo$Subject)))

master_trial <- inner_join(subjInfo, trialDF, by="Subject") 
master_participant <- inner_join(subjInfo, participantDF, by="Subject") 

############### prepping variables for later use  #######################
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



########### A little more processing to grab paths of explore sessions #######################
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
explrDF <- explrDF %>% rename(paths = matrix.nrow...nrow.explr..) #renaming column containing exploration trajectories 

sub_participantinfo <- inner_join(master_participant,explrDF)


###########some prepossessing to get explore locs from Vaisakhs code ################## 
big_explore<-as.data.frame(str_split_fixed(explrDF$paths," ",nrow(explr)))# splits the path strings into separate letters  
big_explore <- big_explore %>% mutate(subject = explrDF$Subject) #adding subject ID as final column 
big_explore <-big_explore[,c(ncol(big_explore), 1:(ncol(big_explore)-1))] # moving subject ID to first column

row_labels<-c(1:nrow(big_explore))#creating row labels, which will be handy for splitting up the dataframe into chunks below 
big_explore<- split( big_explore , f = row_labels)#

#for loop to find consecutive location repeats, and remove them 
for (i in 1:length(big_explore)){
  big_explore[[i]]<-t(big_explore[[i]])#transposing chunks
  big_explore[[i]][big_explore[[i]] == ""] <- NA #replacing empty cells with NA 
  repeat_logical<-t(c(NA, big_explore[[i]]) == c(big_explore[[i]], NA))[1:length(big_explore[[i]])]
  repeats<- grep("TRUE",repeat_logical) #find positions of items in list that are TRUE, which correspond to row numbers of chunk  
  big_explore[[i]][repeats,1]<-"repeats"  #relabel these to "repeats"
  big_explore[[i]]<-t(big_explore[[i]])#transposing to original form 
  big_explore[[i]]<-as.data.frame(big_explore[[i]]) #re-converting to dataframe 
}

big_explore<-unsplit(big_explore,row_labels)#merging all chunks back into single dataframe 
big_explore[is.na(big_explore)]<-"none" #replacing all NAs with string, otherwise for loop below won't work 
big_explore <-  big_explore %>% mutate(sex = sub_participantinfo$sex)

#for loop to determine location counts 
big_explore_counts <- data.frame(matrix(ncol = 0, nrow = nrow(big_explore)))#creating dataframe to store counts into  

for (i in locs){
  big_explore_counts$subject <- big_explore$subject
  d <- as.data.frame(apply(big_explore,1,function(x) sum(x==i))) # not sure what this function is 
  names(d) <- i
  big_explore_counts <- cbind(d, big_explore_counts)
}

big_explore_counts <- big_explore_counts %>% mutate(sex=sub_participantinfo$sex)
# split up by se

################## SPLIT BY SEX #############
print("now splitting by males")
#Males 
print("Males dat aframes including participant info, explore paths, object/location visits, and trials")

malesubjs <- master_participant %>% filter(sex == "Male")
print(nrow(malesubjs))
maletrials <- master_trial %>% filter(sex== "Male")
male_explr <- explr%>% filter(sex=="Male")
malesubsinfo <-  sub_participantinfo%>% filter(sex=="Male")
male_bigexplr <- big_explore %>% filter(sex=="Male")
male_bigexplr_counts <-  big_explore_counts %>% filter(sex=="Male")

male_explr_analysis<-master_participant %>% filter(sex == "Male") %>% select(Subject) %>% as.data.frame()


# lets do the women now 
print("now splitting by females")
print("females data frames including participant info, explore paths, object/location visits, and trials")

femalesubjs <- master_participant %>% filter(sex == "Female")
print(nrow(femalesubjs))
femaletrials <- master_trial %>% filter(sex== "Female")
female_explr <- explr%>% filter(sex=="Female")
femalesubsinfo <-  sub_participantinfo%>% filter(sex=="Female")
female_bigexplr <- big_explore %>% filter(sex=="Female")
female_bigexplr_counts <-  big_explore_counts %>% filter(sex=="Female")

#empty df for later
female_explr_analysis<- master_participant %>% filter(sex == "Female") %>% select(Subject) %>% as.data.frame()

################   Distance Traveled ###############  
# this will get really repetitive because i have to do the analysis for males and females separately
male_explr_analysis$Dist<-malesubjs$em_path_dist_trav 
female_explr_analysis$Dist<-femalesubjs$em_path_dist_trav 


#Normality test to assess whether samples are normally distributed  # females is not normal 
shapiro.test(male_explr_analysis$Dist)
shapiro.test(female_explr_analysis$Dist)


#Wilcoxon Test # may change this later 
dist_groupdiff<-wilcox.test(female_explr_analysis$Dist,male_explr_analysis$Dist) 
dist_groupdiff_effsize<-cohensD(female_explr_analysis$Dist,male_explr_analysis$Dist)

# pvalue of 0.03 and effect size of 0.468

###### Distance FIGS ##########3

#creating a figure to put this in 
distancedf <- master_participant %>% select(Subject,sex,em_path_dist_trav)
sam_size <- master_participant%>%group_by(sex)%>%summarize(num=n())

 # Violin plot 
distancedf %>%
  left_join(sam_size) %>%
  mutate(myaxis=paste0(sex,"\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=em_path_dist_trav, fill=sex)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  scale_colour_brewer() +
  theme(legend.position="none", plot.title = element_text(size=11)) +
  ggtitle("Path distance traveled in men vs women") +
  xlab("") +
  ylab(" Path Distance Traveled")

 #just box plot
distancedf %>%
  left_join(sam_size) %>%
  mutate(myaxis=paste0(sex,"\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=em_path_dist_trav, fill=sex)) +
  geom_boxplot( width =0.3, color="grey") +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  scale_colour_brewer() +
  theme(legend.position="none", plot.title = element_text(size=11)) +
  ggtitle("Path distance traveled in men vs women") +
  xlab("") +
  ylab(" Path Distance Traveled")

#########
