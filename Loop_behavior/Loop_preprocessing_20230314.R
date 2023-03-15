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


distancecalc <- function(X1, X2, Z1, Z2) {
  sqrt((X2 - X1) ^ 2 + (Z2 - Z1) ^ 2)
}

master_file <- tibble()
master_trial <- tibble()
master_participant <- tibble()

# data.frame(matrix(nrow = 0, ncol = length(Column_names))) %>% `colnames<-`(Column_names)

# Note: Loopwalk_clockwise is now degrees traveled
Column_names <-
  c(
    "SubjectID",
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

### REad in a file and clean it up
#set directory to dir that contains all subject subdirs
working_dir <- "/Users/danielacossio/Downloads/Loop Closure"
setwd(working_dir)

# craete a list of subdir folders
Loop_folders <- list.dirs(recursive = FALSE, full.names = FALSE)

#Loop through each subject folder
for (participant_file_folder in 1:length(Loop_folders)) {
  current_sub <-
    Loop_folders[participant_file_folder] # Print the current subject
  #print(current_sub)
  temp_participant <- tibble()
  trial_df <- tibble()
  
  # Find the csv we need
  csv_files <-
    list.files(Loop_folders[participant_file_folder], pattern = ".*_1.csv")
  
  if (length(csv_files) == 0) {
    next
  }
  
  print(paste0(Loop_folders[participant_file_folder], " ", "Has CSV. Continuing the loop"))
  
  subs <-
    read_csv(paste0(Loop_folders[participant_file_folder], "/", csv_files), show_col_types = FALSE) %>%
    filter(!str_detect(sceneName, c("loopClosurePractice"))) %>%
    filter(!str_detect(LoopWalk_target, c("0.5"))) %>%
    filter(!str_detect(LoopWalk_target, c("1.5"))) %>%
    filter(!str_detect(LoopWalk_target, c("2.5"))) %>%
    discard( ~ all(is.na(.) | . == "")) %>%
    add_column(Position_Error = NA)
  
  if (nrow(subs) < 30) {
    next
  } else {
    print("CSV looks good to continue ")
  }
  
  ##  Calculating Position Error using distance t
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
  }
  
  master_trial <- rbind(master_trial,trial_df) %>%
    arrange(id) 

}


colnames(master_trial) <- c(Column_names)


# need to export 

# Now we need to break it down by trials 







