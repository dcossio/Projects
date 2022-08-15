# TBSS anlaysis script
#writen to be cleaner and more flexible by simply uploading the one file you want ato analyse (FA, MD,RD ,AD )

# only thing to be careful with the output file names and where your directory is 


# Loading all of the necessary libraries mat
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



# loading in the output from TBSS. CSV files with participant 
working_dir <- "/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/RAW/Behavior_only"
setwd(working_dir)

df <- read_csv("JHU-FA_Behav.csv",col_names = TRUE) %>% as.data.frame()
participant_info <- read_csv("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/RAW/SNAG_behav_participantinfo.csv", col_names = TRUE) %>% as.data.frame()

behavior_variables <-read_csv("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/RAW/beh_DWI_participantsheet.csv",col_names = TRUE) %>% as.data.frame()


# creating empty data fram to save the out put during each iteration in the next loop 
df_corrs <-data.frame(matrix(NA,nrow=48,ncol=7))
corrs <-data.frame(matrix(NA,nrow=48,ncol=1))
corrs[,1] <- colnames(df)

newDF <- cbind(df, participant_info)

newDF <- cbind(newDF,behavior_variables)


#--------------------------------------------------------------------------------------------
#ACCURACY
#transforiming the average into the log average. Comment out if unneeded

for (A in 1:nrow(newDF)){    # loop through all subs rows,
  if (newDF$AvgAccuracy[A] == 0){       #if the mean acc is perfect or =1, 
    newDF$AvgAccuracy[A] <- 0.0001   #then change to 0.999 for log purposes 
  } 
  if (newDF$AvgAccuracy[A] == 1){       #if the mean acc is perfect or =1, 
    newDF$AvgAccuracy[A] <- 0.9999   #then change to 0.999 for log purposes 
  } 
  newDF$AvgAccuracy[A] <-log(newDF$AvgAccuracy[A]/(1-newDF$AvgAccuracy[A]),10) # main loop formula replacing raw acc with log acc
}


# normality test on the white matter output
normality <-data.frame(matrix(NA,nrow=48,ncol=2))

for (R in 1:nrow(corrs)){
  tmp <- shapiro.test(newDF[1:nrow(newDF),R]) 
  normality[R,] <-  cbind(corrs[R,], tmp$p.value)
}
colnames(normality) <- c("ROI", "p.value")


#using AOV_car to look at correlation and effects of age and sex on ROI 

longDF <- cbind(df, participant_info) %>% # Use common columns to combine these two
  pivot_longer(cols = c(1:48), names_to = "roi", values_to = "df") %>% # convert our data from wide-format to long-format
  mutate(zAcc = scale(.$'AvgAccuracy'), zdf = scale(.$df)) # make some z-scores

mod <- longDF %>% 
  mutate(sub = as.factor(Participants), roi = as.factor(roi), sex = scale(Sex), age = scale(age)) %>%
  aov_car(formula = zdf ~ zAcc + roi + sex + age + Error(sub/roi), observed = c("age", "sex"), factorize = F, include_aov = T, type = 2, return = afex_options("return_aov")) 
#aov_ez(id = SUBJ.., dv = 'fa', within = 'roi', covariate = list('sex', 'age')) # from package 'afex'
ROI_ACC_interaction <- mod$anova_table

#Looping through each ROI and calculating the correlation while regressing/accounting for age and sex 
#used spearman because accuracy is not normal 
for (R in 1:nrow(corrs)){
  tmp <- pcor.test(newDF$AvgAccuracy,newDF[,R],newDF[,c("Sex","age")], method = c("spearman"))
  df_corrs[R,] <-  cbind(corrs[R,], tmp)
}
colnames(df_corrs) <- c("ROI", "estimate","p.value","statistic","n","gp", "Method")




#writing all output 
#double check where you want your output to go.

# write correlation output
write.csv(df_corrs, paste(working_dir, "FA_log_pearson_correlations.csv", sep = "/"))

#write interaction output
write.csv(ROI_ACC_interaction, paste(working_dir, "FA_ROI-acc_interaction.csv", sep = "/"))


#--------------------------------------------------------------------------------------------
# Trial PATH EFIICIENCY 


#Looping through each ROI and calculating the correlation while regressing/accounting for age and sex 
#used spearman because accuracy is not normal 
for (R in 1:nrow(corrs)){
  tmp <- pcor.test(newDF$path_efficiencies,newDF[,R],newDF[,c("Sex","age")], method = c("pearson"))
  df_corrs[R,] <-  cbind(corrs[R,], tmp)
}
colnames(df_corrs) <- c("ROI", "estimate","p.value","statistic","n","gp", "Method")

# write correlation output
write.csv(df_corrs, paste(working_dir, "FA_Trial_eff_correlations.csv", sep = "/"))

#--------------------------------------------------------------------------------------------
# Moves 

#Looping through each ROI and calculating the correlation while regressing/accounting for age and sex 
#used spearman because accuracy is not normal 
for (R in 1:nrow(corrs)){
  tmp <- pcor.test(newDF$Moves,newDF[,R],newDF[,c("Sex","age")], method = c("pearson"))
  df_corrs[R,] <-  cbind(corrs[R,], tmp)
}
colnames(df_corrs) <- c("ROI", "estimate","p.value","statistic","n","gp", "Method")

write.csv(df_corrs, paste(working_dir, "FA_move_correlations.csv", sep = "/"))

#--------------------------------------------------------------------------------------------
# Exploration distance traveled 

#Looping through each ROI and calculating the correlation while regressing/accounting for age and sex 
#used spearman because accuracy is not normal 
for (R in 1:nrow(corrs)){
  tmp <- pcor.test(newDF$em_path_dist_trav,newDF[,R],newDF[,c("Sex","age")], method = c("pearson"))
  df_corrs[R,] <-  cbind(corrs[R,], tmp)
}
colnames(df_corrs) <- c("ROI", "estimate","p.value","statistic","n","gp", "Method")

write.csv(df_corrs, paste(working_dir, "FA_expl_distTraveled_correlations.csv", sep = "/"))
