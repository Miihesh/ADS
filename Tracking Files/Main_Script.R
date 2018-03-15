source("Tracking Files/read_data.R")
source("Tracking Files/preprocessing.R")

setkey(tracking_data, V1)
setkey(tracking_data, Team_ID, Jersey_Number, Track_Chunk, Con_Chunk)

############### Maximum x-Speed ##########################
# Determine x-speed based on change in X position over 1 frames * 25
x_speed_func <- function(X){
  
  x_diff <- abs(diff(X))
  speed <- x_diff * 25
  
  return(c(0,speed))
}

tracking_data <- tracking_data[, X_Speed := x_speed_func(X) , by = key(tracking_data)]

setkey(tracking_data, Team_ID, Jersey_Number)
max_x_speed <- tracking_data[, .(Max_X_Speed = max(X_Speed)), by = key(tracking_data)]

setkey(player_data, "Team_ID", "Jersey_Number")
setkey(max_x_speed, "Team_ID", "Jersey_Number")

player_data <- merge(x = player_data, y = max_x_speed)

# These values are definitely wrong, but I sadly couldn't figure out a way to remove the anomalously
# high speeds

############### Maximum Speed ############################

max_speed <- tracking_data[, .(Max_speed = max(Speed)), by = key(tracking_data)]

setkey(player_data, "Team_ID", "Jersey_Number")
setkey(max_speed, "Team_ID", "Jersey_Number")

player_data <- merge(x = player_data, y = max_speed)

############ High intensity efforts (> 19.8 km/hr) #############################

# Convert units from m/s to km/hr 
tracking_data$Speed_km <- tracking_data$Speed*(18/5) 

# Subset by speeds greater than 19.8 km/s
HI <- tracking_data[tracking_data$Speed_km > 19.8,]

# Check that frame data is continuous without breaks for each player
frame_diff2 <- abs(diff(HI$V1))
frame_diff_value2 <- frame_diff2[frame_diff2 > 1]

# Split by continuous frame data

# Confirmed framedata is not continuous. Large differences above will be due to half-time? 
# Smaller difference will be due to player briefly not being tracked.
# Therefore cannot rely on row difference to determine speed unless frame data is split into continuous chunks per player
# Here I will be assigning a specific key to each continuous chunk

frame_diff_index2 <- which(frame_diff2 > 1) + 1
frame_diff_chunk2 <- vector("integer", length = length(HI$V1))

con_chunk <- 1
for(i in (1:length(frame_diff_index2))){
  if(i == 1){
    frame_diff_chunk2[0:frame_diff_index2[i]] <- con_chunk
    con_chunk <- con_chunk + 1
  } else{
    frame_diff_chunk2[frame_diff_index2[i-1]:frame_diff_index2[i]] <- con_chunk
    con_chunk <- con_chunk + 1
  }
}

# Assign key id to each player
frame_diff_chunk2[frame_diff_index2[i]:length(frame_diff_chunk2)] <- con_chunk
HI$HI_Chunk <- as.integer(frame_diff_chunk2)

# Determine number of frame player is in HIE per chunk
setkey(HI, Team_ID, Jersey_Number, HI_Chunk)
player_HI <- HI[, .(Frames_In_HIE = .N), by = c("Team_ID", "Jersey_Number", "HI_Chunk")]

# Depending on the definition we choose for High Intensity effort, the number of HIE may vary.
# If we assume HIE is speed > 19.8 km/hr for a minimum of 1 sec, then regardless of length HIE (could be 2 sec or 10 sec),
# as long as it is > 1 sec, it counts as 1 HIE.

# Seconds in HI per frame chunk
player_HI$HI_Seconds <- floor(player_HI$Frames_In_HIE/25) 

# Total number of seconds in HIE during the match
setkey(player_HI, Team_ID, Jersey_Number)
player_HI_sum <- player_HI[, .(HI_Sum_Seconds = sum(HI_Seconds)), by = c("Team_ID", "Jersey_Number")]

# Total number of HIE efforts > 1 sec
player_HI$HI_Count_Bin <- player_HI$HI_Seconds/player_HI$HI_Seconds
player_HI$HI_Count_Bin <- replace(player_HI$HI_Count_Bin, is.na(player_HI$HI_Count_Bin), 0)
player_HI_count <- player_HI[, .(HI_Count = sum(HI_Count_Bin)), by = c("Team_ID", "Jersey_Number")]

setkey(player_HI_count, "Team_ID", "Jersey_Number")
setkey(player_HI_sum, "Team_ID", "Jersey_Number")
player_data <- merge(x = player_data, y = player_HI_count)
player_data <- merge(x = player_data, y = player_HI_sum)

############ Maximum Accelaration #######################################

# Determine Acceleration based on change in Speed position over 1 frames * 25
acceleration_func <- function(speed){
  
  speed_diff <- diff(speed)
  acceleration <- speed_diff * 25
  
  return(c(0,acceleration))
}

setkey(tracking_data, V1)
setkey(tracking_data, Team_ID, Jersey_Number, Track_Chunk, Con_Chunk)

tracking_data <- tracking_data[, Acceleration := acceleration_func(Speed) , by = key(tracking_data)]

setkey(tracking_data, Team_ID, Jersey_Number)
max_acc <- tracking_data[, .(Max_acc = max(Acceleration)), by = key(tracking_data)]

setkey(player_data, "Team_ID", "Jersey_Number")
setkey(max_acc, "Team_ID", "Jersey_Number")

player_data <- merge(x = player_data, y = max_acc)

############ Acceleration and deceleration ###############################

# Split data into acceleration and deceleration

# Subset by acceleration and deceleration
acceleration <- tracking_data[tracking_data$Acceleration > 0,]
deceleration <- tracking_data[tracking_data$Acceleration < 0,]

# on average, players are seen to accelerate and decelerate the same number of times during a match.

hist(c(acceleration$Acceleration,deceleration$Acceleration),
     breaks = 1000, xlim = c(-15,15),
     main = "Histogram of Acceleration",
     xlab = "Acceleration")

hist(acceleration$Acceleration,
     breaks = 1000, xlim = c(0,15),
     main = "Histogram of Acceleration",
     xlab = "Acceleration")

hist(abs(deceleration$Acceleration),
     breaks = 1000, xlim = c(0,15),
     main = "Histogram of Acceleration",
     xlab = "Acceleration")

# Taking logs of the acceleration

acc <- log10(acceleration$Acceleration)
dec <- log10(abs(deceleration$Acceleration))

hist(acc,
     breaks = 100, xlim = c(0,1.5),
     main = "Histogram of Acceleration",
     xlab = "Acceleration")

hist(dec,
     breaks = 100, xlim = c(0,1.5),
     main = "Histogram of Acceleration",
     xlab = "Acceleration")

# Examining the histogram, we see that, acceleration and deceleration are quite symmetrical
# This is a surprising result consider how different acceleration are in terms of biomechanics.
# (Concentric contraction vs eccentric contraction)