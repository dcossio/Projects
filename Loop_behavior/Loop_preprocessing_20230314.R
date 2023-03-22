######### Loop Indiv behavior processing script V1
###

library(ggplot2)
#(plyr)
library(tidyverse)
# library(dplyr)
# library(tidyr)
library(stringr)
# library(data.table)
# library(network)
# library(tidygraph)
# library(ggraph)
# library(igraph)
# library(networkD3)
# library(CINNA)
# library(umap)
# library(plotly)
library(factoextra)
library(lsr)
# library(car)
library(ggpubr)
library(entropy)
#library(ds4psy)
# library(pROC)
library(devtools)
library(BRRR)
library(stats)
library(afex)
library(nationalparkcolors) # for palettes
library(wesanderson)
library(philentropy)






#### Functions

# Here I am creating a function to calculate position error using the disntance formula 
distancecalc <- function(X1, X2, Z1, Z2) {
  sqrt((X2 - X1) ^ 2 + (Z2 - Z1) ^ 2)
}



# creating a list of all of the column names I will need 
# Note: Loopwalk_clockwise is now degrees traveled
Column_names <-
  c(
    "sub_id",
    "sceneName",
    "sceneNumber",
    "task",
    "block",
    "trial_ N",
    "LoopWalk_target",
    "LoopWalk_actualPath",
    "LoopWalk_optimalPath",
    "LoopWalk_excessPath",
    "Degrees_Traveled",
    "LoopWalk_duration",
    "Position_Error"
  )

# creating empty data frames to put the data in during different stages 
master_file <- tibble()
master_trial <- tibble()
master_participant <- tibble() 

### REad in a file and clean it up
#set directory to dir that contains all subject subdirs
working_dir <- "/Users/danielacossio/Downloads/Loop Closure"
setwd(working_dir)

# create a list of subdir folders that are in our main loop behavior folder
Loop_folders <- list.dirs(recursive = FALSE, full.names = FALSE)

#Loop through each subject folder
for (participant_file_folder in 1:length(Loop_folders)) {
  current_sub <-
    Loop_folders[participant_file_folder] # Print the current subject
  print(current_sub)
  
  # creating temporary df for the loop 
  temp_participant <- tibble()
  trial_df <- tibble()
  
  # Find the csv we need
  csv_files <-
    list.files(Loop_folders[participant_file_folder], pattern = ".*_1.csv")
  
  # quality check. If the folder does not have the correct CSV, then we skip into the next folder 
  if (length(csv_files) == 0) {
    next
  }
  
  # if the folder has the correct csv, then we print this message and continue 
  print(paste0(Loop_folders[participant_file_folder], " ", "Has CSV. Continuing the loop"))
  
  # Here we are reading in the CSV, cleaning it and putting it into a variable called subs
  subs <-
    read_csv(paste0(Loop_folders[participant_file_folder], "/", csv_files), show_col_types = FALSE) %>%
    filter(!str_detect(sceneName, c("loopClosurePractice"))) %>% # Ignore all rows that are practice trials 
    # Ignoring any fill trial using the code below
    filter(!str_detect(LoopWalk_target, c("0.5"))) %>%  
    filter(!str_detect(LoopWalk_target, c("1.5"))) %>%
    filter(!str_detect(LoopWalk_target, c("2.5"))) %>%
    discard( ~ all(is.na(.) | . == "")) %>% # remove rows with na 
    add_column(Position_Error = NA) # Create an empty column called position error 
  
  # another quality check to make sure we have all the trials
  if (nrow(subs) < 30) {
    next
  } else {
    print("CSV looks good to continue ")
  }
  
  ##  Calculating Position Error using distance function above and putting it into the new column
  for (PE in 1:nrow(subs)) {
    X1 <- subs$LoopWalk_startX[PE]
    Z1 <- subs$LoopWalk_startZ[PE]
    
    X2 <- subs$LoopWalk_endX[PE]
    Z2 <- subs$LoopWalk_endZ[PE]
    
    subs$Position_Error[PE] <- distancecalc(X1, X2, Z1, Z2)
  }
  
  # MAking sure that we get the absolute error of degree traveled
  subs$LoopWalk_clockwiseTravel <-
    abs(subs$LoopWalk_clockwiseTravel)
  
  # Now we're going to bind all of this into one big CSV for clean raw data
  master_file <- rbind(master_file , subs) %>%
    arrange(id)
  
  write.csv(master_file, "Indiv_loop_master_file.csv")
  
# now we're going to start getting everything into trials   
  for (dist in 1:3) {
    meter <- subs %>%
      filter(str_detect(LoopWalk_target, c(as.character(dist))))
    
    var_averages <-
      meter %>% select(
        'LoopWalk_actualPath',
        'LoopWalk_optimalPath',
        'LoopWalk_excessPath',
        'LoopWalk_clockwiseTravel',
        'LoopWalk_duration',
        'Position_Error'
      ) %>% summarise_all(mean)
    
    temp <-
      cbind(meter[1, 1:8], var_averages) %>% 
      mutate(LoopWalk_target = dist) %>%
      mutate(trial = nrow(meter)) %>%
      select(-c(condition)) 
    
    
    trial_df <- rbind(trial_df, temp) 
    
    sub_id <- trial_df[1,c("id")]
    sceneName <- trial_df[1,c("sceneName")]
    sceneNumber <-  trial_df[1,c("sceneName")] 
    task <- trial_df[1,c("task")] 
    block<- trial_df[1,c( "block")] 
    # aCtual path for each radius
    actual_path_rad1<-trial_df[1,c("LoopWalk_actualPath")]
    actual_path_rad2<-trial_df[2,c("LoopWalk_actualPath")]
    actual_path_rad3<-trial_df[3,c("LoopWalk_actualPath")]
    #optimal path for each radius 
    optimalPath_rad1<-trial_df[1,c("LoopWalk_optimalPath")]
    optimalPath_rad2<-trial_df[2,c("LoopWalk_optimalPath")]
    optimalPath_rad3<-trial_df[3,c("LoopWalk_optimalPath")]
    #Excess path for each radius 
    excessPath_rad1<-trial_df[1,c("LoopWalk_excessPath")]
    excessPath_rad2<-trial_df[2,c("LoopWalk_excessPath")]
    excessPath_rad3<- trial_df[3,c("LoopWalk_excessPath")]
    
    #Degrees Traveled  for each radius 
    Degrees_Traveled_rad1<-trial_df[1,c("LoopWalk_clockwiseTravel")]
    Degrees_Traveled_rad2<-trial_df[2,c("LoopWalk_clockwiseTravel")]
    Degrees_Traveled_rad3<-trial_df[3,c("LoopWalk_clockwiseTravel")]
    
    #Walk Duration   for each radius 
    Walk_duration_rad1<-trial_df[1,c("LoopWalk_duration")]
    Walk_duration_rad2<-trial_df[2,c("LoopWalk_duration")]
    Walk_duration_rad3<-trial_df[3,c("LoopWalk_duration")]
    
    #Position Error for each radius 
    Position_Error_rad1<-trial_df[1,c("Position_Error")]
    Position_Error_rad2<-trial_df[2,c("Position_Error")]
    Position_Error_rad3<-trial_df[3,c("Position_Error")]
    
    temp_participantdf <- cbind(sub_id,sceneName,sceneNumber,task, block,actual_path_rad1,actual_path_rad2, actual_path_rad3,optimalPath_rad1, optimalPath_rad2, optimalPath_rad3, excessPath_rad1,excessPath_rad2,excessPath_rad3,Degrees_Traveled_rad1,Degrees_Traveled_rad2, Degrees_Traveled_rad3, Walk_duration_rad1, Walk_duration_rad2, Walk_duration_rad3, Position_Error_rad1,Position_Error_rad2, Position_Error_rad3) %>% as.data.frame()
    
    
  }
  
  master_trial <- rbind(master_trial,trial_df) %>%
    arrange(id) 
  print(" Starting to compile participant folder")

  master_participant <- rbind(master_participant,temp_participantdf)

}


colnames(master_trial) <- c(Column_names)

write.csv(master_trial, "Indiv_loop_master_trial.csv")
write.csv(master_participant, "Indiv_loop_master_participant.csv")

print("finished")







