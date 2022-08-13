# Explore script for preprocessing  
#edited for midlife/young on slow computer 
#March 29 2022

#new change is using block trials to 

# This is the first script in the preprocessing pipeline. DMC and VP edited this based off of Rob's script 
# it was adapted to process one single Eprime txt file containing all explore and test trials combined



##############################################################################
#   Title: Explore script for SNAG midlife preprocessing                     #
#   Author: Vaisakh P & Daniela C                                            #
#                                                                            #
#   Purpose: This is the first script in the pipeline and requires the       #
#            output from the behavior preprocessing Here the                 #
#                  #
#                                                                            #
#    input: eprime.txt file one per person                                   #
#                                                                            #
#    output: behavior csv file                                               #
#                                                                            #
#                                                                            #
##############################################################################


#---------------------------------------------------------------------------------------------------------------------------------------------------------    
# Load proper packages. Rprime is built specifically for EPrime. If you do not have these packages installed, use
#     install.packages("[package name goes here]") to install it on your R client.
library(rprime)
library(tidyverse)
library(wrapr)
library(plyr)
library(dplyr)
library(BRRR) #just for fun package. See BRRR on github to add 

#Brrr package for errors 
  # DJ Khaled if theres an error
   options(error = function() {skrrrahh(34)})


#---------------------------------------------------------------------------------------------------------------------------------------------------------    

# Set your working directory here. the one below is mine. Copy and paste yours, and assign it to 'working_dir'. 
# This working directory should contain file folders for each participant, with each folder containing Eprime trial data.
# Each folder is named with participant's number:sss
# i.e.: "002.....103"
working_dir <-"/Volumes/GoogleDrive/My Drive/MLINDIV_SNAG_preprocessing/Raw sub data"
setwd(working_dir)

MLINDIV_filelist <- list.files()
MLINDIV_filelist <- MLINDIV_filelist[nchar(MLINDIV_filelist) == 3]

# Create an empty master data frame; this will contain all Explore and Test data for each participant.
master_file <- tibble()

# start of the loop
for (participant_file_folder in 1:length(MLINDIV_filelist)){
  current_file <- MLINDIV_filelist[participant_file_folder]
  print(current_file)
  master_participant <- tibble()
  eprime_txt_files <- list.files(MLINDIV_filelist[participant_file_folder], pattern = ".*.txt")
  
  for (file_i in 1:length(eprime_txt_files)){
    
    eprime_txt_files <- list.files(MLINDIV_filelist[participant_file_folder], pattern = ".*.txt")
    
    #Create data frame, remove header/ender rows and store for later use
    e_file <- read_eprime(paste0(MLINDIV_filelist[participant_file_folder], "/" , eprime_txt_files[file_i]))
    
    
    if (last(e_file) != "*** LogFrame End ***"){
      print(paste(eprime_txt_files[file_i], "skipped", "incomplete file"))
      next
    }
    e_frame <- FrameList(e_file)
    
    e_df <- to_data_frame(e_frame)
    
    #Here we have to create a loop that will combine the data for move trials into the same column as the ones for explore trials. 
    #Eprime required the same exact variable to be named something else so for move trials all of their variables had an A. 
    #for example, explore trials had the variable Choose.RT but movfe trials was named to chooseA.rt. R studio will not let us merg e or bind the
    #explore and trial rows unless they have the same named columns. Therefore, here i am simply copying the data from ChooseA.rt into the chooseRT 
    #for each moveproc row.   
    #added on feb 10
    
    movetrials <- which(e_df$Procedure =="MoveProc")
    
    for (m in 1:length(movetrials)){
       e_df$Choose.RT[movetrials[m]] <- e_df$ChooseA.RT[movetrials[m]]
       e_df$Choose.OnsetTime[movetrials[m]] <- e_df$ChooseA.OnsetTime[movetrials[m]]
       e_df$Choose.RTTime[movetrials[m]] <- e_df$ChooseA.RTTime[movetrials[m]]
       e_df$MoveVid.OnsetTime[movetrials[m]] <- e_df$MoveVidA.OnsetTime[movetrials[m]]
    }
   
    #Check to see if file contains minimum amount of rows for compilation
    if (nrow(e_df) < 10){
      print(paste(eprime_txt_files[file_i], "skipped", "row #", nrow(e_df)))
      next
    }
    e_df_header <- e_df[1,]
    
    e_df <- e_df[2:(nrow(e_df)-1), ]
    
    #creating explore tibble and removing from data frame. We will come back to this after test trials processing 
    expl_df <-e_df %>% filter(Procedure != "TrialProc")  %>% filter (Procedure !="MoveProc")
    expl_proc <- as_tibble(expl_df) %>%
      select(qc(
        Procedure, Cycle, Sample, ITI.OnsetTime, 
        startPosition, startFacing, 
        StartIm, endPosition, EndIm
      ))
    
    
    # removing explore trials and leaving only test 
    expl_proc_location <- which(e_df$Procedure == "ExploreProc")
    Noexpl_df <- e_df[-c(expl_proc_location), ]  # this has removed all of the explore trials and left only the movement and test trials for the next step 
    
  #---------------------------------------------------------------------------------------------------------------------------------------------------------    
    # Beginning Trial data analysis-> all explore trials have been removed. 
    
    #create a separate tibble for the Trial Procedures, which contain start/end goals and other variables
   
    trial_proc <-Noexpl_df[Noexpl_df$Procedure == "TrialProc",]
      trial_proc <- as_tibble(trial_proc) %>%
       select(qc(
         Procedure,Cycle, Sample, ITI.OnsetTime, 
         startPosition, startFacing, 
         StartIm, endPosition, EndIm
       ))
    

    #Update the e_df data frame to be a tibble that contains only the variables we care about
    #check to see what variables are important to us
    
    trial_tibble <- as_tibble(Noexpl_df) %>%
      select(qc(
        Eprime.Basename,Eprime.LevelName,
        ImageFile,Choose.OnsetTime,
        Choose.RTTime, Choose.RT, 
        VideoFile, MoveVid.OnsetTime

     ))
    
    
    # Create an empty tibble where a for loop creates repetitions of trial_proc rows to be used in adding to 
    # the master tibble as new columns ( so for each row, it will now have not only movement data, 
    # but also what trial type it is with Start and end goals, etc)
      #trial proc location is lining up to locations in master tibble not e_df 
    
      trial_cols <- tibble()
      trial_proc_location <- which(Noexpl_df$Procedure == "TrialProc")
      place <- 0
      i = 1
    
      for (row_location in 1:length(trial_proc_location)){
        repeat_num <- trial_proc_location[row_location] - place - 1
        curr_row <- trial_proc[row_location, ]
        new_rows <- curr_row[rep(seq_len(nrow(curr_row)), each = repeat_num), ]
        trial_cols <- rbind(trial_cols, new_rows)
        place <- trial_proc_location[row_location]
        i <- i + 1
      }
    
    
    # Remove TrialProc rows from master tibble, as well as the last row (which is useless), then column bind the new 
    #trial_cols(which should now have the same # of rows as the master tibble) to the master tibble
    trial_tibble <- trial_tibble[-c(trial_proc_location), ]
    trial_tibble <- trial_tibble[-(nrow(trial_tibble)), ] # removing last row
    trial_tibble <- trial_tibble[-(nrow(trial_tibble)), ] # Since the last row is also empty we're going to remove it as well
    trial_tibble <- cbind(trial_cols, trial_tibble)
  
    
    #Create a data frame with same number of rows as master tibble, that contains 3 columns of repeating values: 
    #Subject (participant #), Task (Explore | Test), TaskType (Explore: 1 or 2 | Test: A1...C3)
    Task <- strsplit(trial_tibble$Procedure, "_")[[1]][[1]]
    Subject <- e_df_header$Subject
    #Task type may be un needed, Will delete in the future if that's the case
    Task_type <- "NA"
    #Task_type <- paste0(strsplit(Task_type, "_")[[1]][5], strsplit(Task_type, "_")[[1]][6])
    
    #Grab the finish time of procedure
    if(!is.null(e_frame[length(e_frame)][[1]]["Finished.OnsetTime"][[1]])){
      Finished.OnsetTime <- as.integer(e_frame[length(e_frame)][[1]]["Finished.OnsetTime"][[1]])
    } else {
      Finished.OnsetTime <- NA
    }
    
    # Grab the finish time of procedure
    trial_meta_df <- cbind(Subject, Task, Task_type, Finished.OnsetTime)
    trial_meta_df <- trial_meta_df[rep(seq_len(nrow(trial_meta_df)), each = nrow(trial_tibble)), ]
    
   newTrial_tibble <- cbind(trial_meta_df, trial_tibble)
    
    # This is the end of the trial processing 
  #----------------------------------------------------------------------------------------------------------------------------------------------------------
    # Begin the explore processing 
    
     explr_tibble <- as_tibble(expl_df) %>%
      select(qc(
        Eprime.Basename,Eprime.LevelName,
        ImageFile,Choose.OnsetTime,
        Choose.RTTime, Choose.RT, 
        VideoFile, MoveVid.OnsetTime
    ))
    
    
   explr_tibble <- cbind(expl_proc, explr_tibble)
   
   # in order to get the correct Task type we need to do a little bit of preprocessing. The block trials tell us where the 1st explore session and 2nd session end 
   #Grabbing all explore trials that occur in session one. Using Procedure since ExploreINITProc1 is the block trial 1 that ends the first explore session
   # then I'm cleaning up and removing the block sessions since we dont need them 
   # then just adding the task type column 
   exploresession1<- explr_tibble[1:which(explr_tibble$Procedure == "ExploreINITProc1"), ]%>% filter(Procedure=="ExploreProc") %>% mutate(Task_type = 1)
   
   #doing the same thing with the second session but using the start of the explore session from the break or block 2 and then ending at block 3 or ExploreINITProc2
   exploresession2 <- explr_tibble[which(explr_tibble$Procedure == "BreakProc"): which(explr_tibble$Procedure == "ExploreINITProc2"), ] %>% filter(Procedure=="ExploreProc") %>% mutate(Task_type = 2)
   
   explr_tibble <- rbind(exploresession1,exploresession2) # simply binding them again by overwriting the explr_tibble
    
    #Create a data frame with same number of rows as master tibble, that contains 3 columns of repeating values: 
    #Subject (participant #), Task (Explore | Test), TaskType (Explore: 1 or 2 | Test: A1...C3)
    Task <- strsplit(expl_proc$Procedure, "_")[[1]][[1]]
    Subject <- e_df_header$Subject
  
    expl_meta_df <- cbind(Subject, Task, Finished.OnsetTime)
    expl_meta_df <- expl_meta_df[rep(seq_len(nrow(expl_meta_df)), each = nrow(explr_tibble)), ]
    
    newexplr_tibble <- cbind(expl_meta_df, explr_tibble)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
    
    #combining the test and explore data back together 
    master_tibble <- rbind(newexplr_tibble,newTrial_tibble)
    
    
    # Append master_tibble to participant_master file through rbind()
    master_participant <- rbind(master_participant, master_tibble)
    
    
  }
  
  # Append master_participant file to the master_file
  master_file <- rbind(master_file, master_participant)
  
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------    

# functions for exctracting variables such as end postion, face direction etc 

convert_to_pos <- function (imagecol){
  end_pos = c()
  for (i in 1:length(imagecol)){
    pos <- strsplit(as.character(imagecol[i]), "/")
    pos <- pos[[1]][2]
    pos <- strsplit(pos, ".jpg")
    pos <- pos[[1]][1]
    # TODO:  If string contains "circle", split
    end_pos <- c(end_pos, pos)
    
  }
  return(end_pos)
}

convert_to_hall <- function(videocol){
  hallsnip <- c()
  for (i in 1:length(videocol)){
    hall <- substr(videocol[i], 8, 12)
    hallsnip <- c(hallsnip, hall)
  }
  return(hallsnip)
}

convert_to_movement <- function(hallcol){
  movement <- c()
  for ( i in 1:length(hallcol)){
    
    mov <- substr(hallcol[i], 1, 1) == substr(hallcol[i], 4, 4)
    if (mov & !is.na(mov)){
      mov <- "Rot"
    } else if (hallcol[i] == "Selec" & !is.na(mov)){
      mov <- "Select"
    } else if (!is.na(mov)){
      mov <- "Walk"
    }
    movement <- c(movement, mov)
  }
  return(movement)
}

convert_to_letter <- function(endloccol){
  lett <- c()
  for (i in 1:length(endloccol)){
    let_loc <- substr(endloccol[i], 1, 1)
    lett <- c(lett, let_loc)
  }
  return(lett)
}

convert_to_dir <- function(endloccol){
  direction <- c()
  for (i in 1:length(endloccol)){
    dirnum <- as.numeric(substr(endloccol[i], 2, 2))
    if (!is.na(dirnum)){
      face_dir <- switch(dirnum, "N", "E", "S", "W")
    } else {
      face_dir <- dirnum
    }
    
    direction <- c(direction, face_dir)
  }
  return(direction)
}

endloc_fix <- function(endloccol){
  end_location <- c()
  for (i in 1:length(endloccol)){
    if (as.character(endloccol[i]) == "V3" & !is.na(endloccol[i])){
      el <- "Y3"
    } else if (as.character(endloccol[i]) == "V4" & !is.na(endloccol[i])){
      el <- "Y4"
    } else if (as.character(endloccol[i]) == "V3_sphere" & !is.na(endloccol[i])){
      el <- "Y3_sphere"
    } else {
      el <- as.character(endloccol[i])
    }
    end_location <- c(end_location, el)
  }
  return(end_location)
}

hallsnip_fix <- function(hallsnipcol){
  hallcol <- c()
  for (i in 1:length(hallsnipcol)){
    if (as.character(hallsnipcol[i]) == "V2_V3" & !is.na(hallsnipcol[i])){
      el <- "V2_Y3"
    } else if (as.character(hallsnipcol[i]) == "V2_V4" & !is.na(hallsnipcol[i])){
      el <- "Y2_Y4"
    } else if (as.character(hallsnipcol[i]) == "V4_V1" & !is.na(hallsnipcol[i])){
      el <- "Y4_V1"
    } else {
      el <- as.character(hallsnipcol[i])
    }
    hallcol <- c(hallcol, el)
  }
  return(hallcol)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------    
# combining all variables 

master_file <- master_file %>% mutate(end_location = convert_to_pos(ImageFile))
print("End Location calculated and added...")
master_file <- master_file %>% mutate(hallsnip = convert_to_hall(VideoFile))
print("Hall video snippet calculated and added...")


master_file <- master_file %>% mutate(end_location = endloc_fix(end_location))
print("End location V & Y values fixed...")
master_file$end_location <- str_replace(master_file$end_location, "P1", "Z1")
master_file$end_location <- str_replace(master_file$end_location, "P4", "Z2")

master_file <- master_file %>% mutate(hallsnip = hallsnip_fix(hallsnip))
print("Hall snip video V & Y values fixed...")
master_file$hallsnip <- str_replace(master_file$hallsnip, "P1", "Z1")
master_file$hallsnip <- str_replace(master_file$hallsnip, "P4", "Z2")

master_file <- master_file %>% mutate(movement = convert_to_movement(hallsnip))
print("Movement type calculated and added...")

master_file <- master_file %>% mutate(face_dir = convert_to_dir(end_location))
print("Facing direction calculated and added...")
master_file <- master_file %>% mutate(sphere = grepl("sphere", master_file$end_location))
print("Next to Sphere boolean calculated and added...")



master_file <- master_file %>% mutate(letter_loc = convert_to_letter(end_location))
print("Letter-End Location calculated and added...")

mcoords <- read.csv("location.csv")
colnames(mcoords)[1] <- "letter_loc"

master_file <- join(master_file, mcoords, by = "letter_loc")
print("X and Y coordinates merged and added...")

print("Writing csv")
write.csv(master_file, "MLINDIV_behavioral_master.csv")

skrrrahh(26) #Brr package
