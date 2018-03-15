# This was the original script where i was going to answer all the question, but it ended up becoming a
# graveyard of ideas on how to best determine the speed of the player from the difference in their position.
# General idea was to use ((delta_X^2,delta_Y^2)^0.5)/delta_time where delta_time would be in frames converted in seconds.
# This however kept leading to really high instance of speed due to sudden shifts in X,Y between frames. 
# Pretty much everything below was me trying to figure out how to avoid those sudden shifts.



# I've copied some imortant comment below here:
# Further examination of the data showed that speeds existed when there were minimal movement in the X, Y planes. 
# The conclusion that I came up with was that this was movement in the Z-plane (vertical).
# It would explain why I was unable to accurately recreate the speed using the difference in the X, Y plane
# and it would also explain the output below:

# tracking_data[15:30] # Assuming the previous steps have been followed

# Between the frames 1710075-1710081, we see very little movement in the X,Y plane, yet we have a speed of 0.28-0.24.
# My assumption would be that this is due to jumping.



#source("Tracking Files/read_data.R")
#source("Tracking Files/preprocessing.R")


###########################################################
############# Data Analysis ###############################

############ Determine Speed from X, Y position. ###########

# setkeys to include continuous chunks of data
setkey(tracking_data, V1)
setkey(tracking_data, Team_ID, Jersey_Number, Track_Chunk, Con_Chunk)

# Method 1 - Determine speed from X, Y position and compare to speed given. 

# If similar, then we can employ a similar method to find x speed. 
# Function to determine Speed based on change in X, Y position over 25 frames

speed_25_frame_func <- function(X,Y,Size_Chunk){

  # If change in chunk occurs < 25 frames, then no speed will be determined
  # for that chunk as we will have insuffiecient data to find the speed

  if (Size_Chunk < 25){
    speed <- rep(0, Size_Chunk)
    print(Size_Chunk)
  } else {
    speed_less_than_25 <- rep(0, 24)
    X_diff <- (diff(X, lag=24))^2
    Y_diff <- (diff(Y, lag=24))^2

    speed_greater_than_25 <- sqrt(X_diff + Y_diff)

    speed <- c(speed_less_than_25, speed_greater_than_25)
  }
  return(speed)
}


speed_position_25 <- tracking_data[, list(Speed_position_25 = speed_25_frame_func(X,Y)) , by = key(tracking_data)]

# Had to scrap this method after needing a second key for the 'Tracker'. Process started to become unnecessarily complicated.
# Also, using a lag of 24 would leave a large number of speed rows empty.

# Method 2 - determine Speed based on change in X, Y position over 1 frames * 25
speed_1_frame_func <- function(X,Y){
  
  X_diff <- diff(X)^2
  Y_diff <- diff(Y)^2
  speed <- sqrt(X_diff + Y_diff) * 25
  
  return(c(0,speed))
}

speed_position_1 <- tracking_data[, list(Speed_X_Y = speed_1_frame_func (X,Y)) , by = key(tracking_data)]

# Backup datatables for testing
# tracking_data_test <- tracking_data
# tracking_data <- tracking_data_test

tracking_data_test$Speed_X_Y <- speed_position_1$Speed_X_Y

# Simplier than method 1 and will leave only the first row empty. However, very high speed values start to appear:

max(tracking_data_test$Speed_X_Y)
which(tracking_data_test$Speed_X_Y > 20)

# Taking a closer look at the maximum value:
max_speed <- which(tracking_data_test$Speed_X_Y > 70)
tracking_data_test[(max_speed-10):(max_speed+10)]

# We see a break in the frame (1709465-1709468), and there is a large change in the Y plane when we start the next Con-Chunk.
# I expect this to be an anomoly, as a player moving approx 3 metres in one frame (as impressive as that would be) is not realistic.


# Method 3 - determine Speed based on change in X, Y position over 5 frames * 5
speed_5_frame_func <- function(X,Y,Track_Chunk_Size){
  #print(Track_Chunk_Size[1])
  # Certain Tracker_Chunks are < 5 rows in size, they must be populated with the appropriate number of 0 rows 
  if (Track_Chunk_Size[1] < 5){
    speed_total <- rep(0, Track_Chunk_Size[1])
  } else{
    X_diff <- diff(X, lag = 4)^2
    Y_diff <- diff(Y, lag = 4)^2
    speed <- sqrt(X_diff + Y_diff) * 5
    
    speed_total <- c(rep(0,4),speed)
  }
  
  return(speed_total)
}

speed_position_5 <- tracking_data[, list(Speed_X_Y = speed_5_frame_func(X,Y,Track_Chunk_Size)) , by = key(tracking_data)]
tracking_data_test$Speed_X_Y_5 <- speed_position_5$Speed_X_Y

# Produces a similar issue to above, with a max speed of 20 m/s.

# test_chunk1 <- speed_position[speed_position$Chunk == 1,]
# ID <- 1
# Jersey <- 21
# test <- tracking_data[tracking_data$Team_ID == ID  & tracking_data$Jersey_Number == Jersey,]
# test0 <- speed_position[speed_position$Team_ID == ID  & speed_position$Jersey_Number == Jersey ,]


# Further examination of the data showed that speeds existed when there were minimal movement in the X, Y planes. 
# The conclusion that I came up with was that this was movement in the Z-plane (vertical).
# It would explain why I was unable to accurately recreate the speed using the difference in the X, Y plane
# and it would also explain the output below:

tracking_data[15:30] # Assuming the previous steps have been followed

# Between the frames 1710075-1710081, we see very little movement in the X,Y plane, yet we have a speed of 0.28-0.24.
# My assumption would be that this is due to jumping.

# With this assumption in place, the above becomes redundant, however, the method of determining the speed can still be
# applied to the x direction.



# Plot distance vs frame, determine speed from gradient of line
# new column distance travelled




############################ X-Speed #####################################

# Function to determine Speed based on change in X position over 25 frames
x_speed_func <- function(X,Y,Size_Chunk){
  
  # If change in chunk occurs < 25 frames, then we need to apply < 24 0s to the speed
  # speed_less_than_25 <- rep(0, 24)
  
  if (Size_Chunk < 25){
    speed <- rep(0, Size_Chunk)
    print(Size_Chunk)
  } else {
    speed_less_than_25 <- rep(0, 24)
    speed_greater_than_25 <- abs(diff(X, lag=24))
    
    speed <- c(speed_less_than_25, speed_greater_than_25)
  }
  return(speed)
}

x_speed_position <- tracking_data[, list(X_Speed = x_speed_func(X,Y,Size_Chunk)), by = key(tracking_data)]

# Attach Speed determined from X, Y position onto main dataset
tracking_data$Speed_X_Y_Position <- speed_position$Speed_position
tracking_data$Speed_X_Position <- x_speed_position$X_Speed


# Sanity checks. X-speed must be < Speed.
tracking_data[tracking_data$Speed < tracking_data$Speed_X_Position,]
tracking_data[tracking_data$Speed_X_Y_Position < tracking_data$Speed_X_Position,]

# Problem! Speed_X_Y_Position > Speed_X, as expected, however in some cases, Speed is not > Speed_X.
# Speed is therefore determined in a different manner. Perhaps momentary speed?


################## Maximum Speed per player in x-direction ##########

setkey(tracking_data, Team_ID, Jersey_Number)

max_x_speed <-  tracking_data[, list(X_Speed = x_speed_diff(X)), by = key(tracking_data)]

#speed_position[speed_position$Speed_position < speed_position$X_Speed,]




# speed_func <- function(x_y){
#   speed_less_than_25 <- rep(0, 24)
#   speed_greater_than_25 <- abs(diff(x_y, lag=24))
#   return(c(speed_less_than_25,speed_greater_than_25))
# }

# speed_position <- x_y[, list(speed_position = speed_func(x_y)), by = key(tracking_data)]

tracking_data_test$Speed_Position <- speed_position$Speed_position

player_test0 <- tracking_data[tracking_data$Team_ID == 0 & tracking_data$Jersey_Number == 21,]
player_test_speed_x <- abs(diff(player_test$X, lag = 24))
player_test_speed_x_full <- c(rep(0,24), player_test_speed_x)
player_test$X_Speed <- player_test_speed_x_full

setkey(tracking_data, Team_ID, Jersey_Number, Chunk)
X_Speed <- tracking_data[, list(X_Speed = x_speed_diff(X)), by = key(tracking_data)]

#player_test_speed_x_func <- X_Speed_df[X_Speed$Team_ID == 0 & X_Speed$Jersey_Number == 2,]


tracking_data_test <- tracking_data

# Compare func and diff

which(player_test$X_Speed > 10)
which(player_test_speed_x_func$X_Speed > 10)

rownames(player_test) <- NULL
rownames(player_test_speed_x_func) <- NULL

#################################


# Max speed in x direction. Rough estimate would be to find difference per consectutive frame and multiply by 25 (m/s)
# Or find difference between every 25 frames.

chunk <- 0
frame_diff_func <- function(tracking_data){
  
  for (i in frame_diff_index){
    chunk <- chunk + 1
  }
  
}

# tracking_data_x <- tracking_data[, list(X_Speed = frame_diff_func(tracking_data)), by = key(tracking_data)]
# 
# #tracking_data_con <- tracking_data[, list(chunk = frame_diff_func(V1)), by = key(tracking_data)]
# 
# 
# #player_x <- subset(tracking_data, Team_ID == 1 & Jersey_Number == 19)
# 
# 
# # x-direction, 1 sec = 25 frames, determine difference in X in intervals of 25.
# 
# 
# 
# 
# tracking_data_x <- tracking_data[, list(X_Speed = abs(diff(X, lag=24))), by = key(tracking_data)]
# 
# setkey(tracking_data_x, Team_ID, Jersey_Number)
# max_speed_x <- tracking_data_x[, .(Max_speed = max(X_Speed)), by = key(tracking_data_x)]

