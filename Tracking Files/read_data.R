####################################################### 
############# Pre-processing/cleaning #################

library(splitstackshape)
library(reshape2)
library(tidyr)

# Read Data

track_raw <- read.table("Tracking Files/803185.dat", sep = ":")
track_cut <- track_raw[1:2]

# split column 2 into players/object movement

track_wide <- cSplit(track_cut, "V2", sep=";")

# Wide to Long. Using Tracker as a place holder name for each segment in chunk 2

track_long <- melt(track_wide, id.vars = "V1", variable.name = "Tracker")

# Split player data

track_long_split <- separate(data = track_long, col = value, 
                            into = c("Team_ID", "System_ID", "Jersey_Number", "X", "Y", "Speed"),
                            sep = ",")

# Split tracker column to remove unessecary string elements. Convert into int.

track_long_split_2 <- separate(data = track_long_split, col = Tracker, 
                               into = c("V", "Tracker_Num"),
                               sep = "_")
track_long_split_2$Tracker_Num <- as.integer(track_long_split_2$Tracker_Num)

#Remove system_id and team_id != 0,1

track_long_player <- track_long_split_2[track_long_split_2$Team_ID %in% c(0,1),]
tracking_data <- subset(track_long_player, select = -c(System_ID, V))

# convert x & y units from cm to m 
tracking_data$X <- as.numeric(tracking_data$X)/100
tracking_data$Y <- as.numeric(tracking_data$Y)/100

# Convert speed to numeric
tracking_data$Speed <- as.numeric(tracking_data$Speed)

# Clear mem
rm(track_cut, track_long, track_long_player, track_long_split, track_long_split_2, track_raw, track_wide)

