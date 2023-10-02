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

distancecalc <- function(X1,X2,Z1,Z2){
  
  sqrt((X2-X1)^2 + (Z2-Z1)^2) 
}

master_raw <- tibble()
master_trial <- tibble()
master_participant <-tibble()
 # data.frame(matrix(nrow = 0, ncol = length(Column_names))) %>% `colnames<-`(Column_names)

# Note: Loopwalk_clockwise is now degrees traveled 
Column_names <- c("SubjectID", "sceneName",  "sceneNumber", "task", "block","trial_ N", "LoopWalk_target", "LoopWalk_actualPath", "LoopWalk_optimalPath", "LoopWalk_excessPath", "Degrees_Traveled", "LoopWalk_duration", "Position_Error")

### REad in a file and clean it up 

# working_dir <-"/Volumes/GoogleDrive/My Drive/MLINDIV_SNAG_preprocessing/Raw sub data"
# setwd(working_dir)
# 
# MLINDIV_filelist <- list.files()
# MLINDIV_filelist <- MLINDIV_filelist[nchar(MLINDIV_filelist) == 3]



subs <- read_csv("/Users/danielacossio/Downloads/Loop Closure/1012/1012_1.csv") %>% 
  filter(!str_detect(sceneName, c("loopClosurePractice"))) %>% 
  filter(!str_detect(LoopWalk_target, c("0.5"))) %>% 
  filter(!str_detect(LoopWalk_target, c("1.5"))) %>% 
  filter(!str_detect(LoopWalk_target, c("2.5"))) %>% 
  discard(~all(is.na(.) | . =="")) %>% 
  add_column(Position_Error = NA)

# # Check here 
#   if ( nrow(subs < 30)){
#     next
#   }

  for (PE in 1:nrow(subs)){
    
    X1 <- subs$LoopWalk_startX[PE] 
    Z1 <- subs$LoopWalk_startZ[PE]
    
    X2 <- subs$LoopWalk_endX[PE]
    Z2 <- subs$LoopWalk_endZ[PE]
    
    subs$Position_Error[PE] <- distancecalc(X1,X2,Z1,Z2)
  }


subs$LoopWalk_clockwiseTravel <- abs(subs$LoopWalk_clockwiseTravel)
subs

# Rbind outputs here to save as a trial csv with all subjects and all trials 

master_raw <- rbind(master_raw ,subs)



# export CSv later 

for (dist in 1:3) {
  
   meter <- subs %>% 
    filter(str_detect(LoopWalk_target, c(as.character(dist))))
   
   var_averages <- meter %>% select('LoopWalk_actualPath', 'LoopWalk_optimalPath', 'LoopWalk_excessPath', 'LoopWalk_clockwiseTravel', 'LoopWalk_duration', 'Position_Error') %>% summarise_all(mean)
   
  subj_info <- meter[1,1:8] 
  
  temp <- cbind(subj_info,var_averages) %>% mutate(LoopWalk_target = dist) %>% mutate(trial = nrow(meter))
  
  master_trial <- rbind(master_trial,temp)
}


