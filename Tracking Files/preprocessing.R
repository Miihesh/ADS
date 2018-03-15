# setkeys for datatable
setkey(tracking_data, V1)
setkey(tracking_data, Team_ID, Jersey_Number)

# Get Unique players in the data table
player_data <- unique(tracking_data, by = key(tracking_data))
player_data <- subset(player_data, select = c(Team_ID, Jersey_Number))

# Check that frame data is continuous without breaks for each player
frame_diff <- abs(diff(tracking_data$V1))
frame_diff_value <- frame_diff[frame_diff > 1]

# Method 1 - split by continuous frame data

# Confirmed framedata is not continuous. Large differences above will be due to half-time? 
# Smaller difference will be due to player briefly not being tracked.
# Therefore cannot rely on row difference to determine speed unless frame data is split into continuous chunks per player
# Here I will be assigning a specific key to each continuous chunk

frame_diff_index <- which(frame_diff > 1) + 1
frame_diff_chunk <- vector("integer", length = length(tracking_data$V1))

con_chunk <- 1
for(i in (1:length(frame_diff_index))){
  if(i == 1){
    frame_diff_chunk[0:frame_diff_index[i]] <- con_chunk
    con_chunk <- con_chunk + 1
  } else{
    frame_diff_chunk[frame_diff_index[i-1]:frame_diff_index[i]] <- con_chunk
    con_chunk <- con_chunk + 1
  }
}

frame_diff_chunk[frame_diff_index[i]:length(frame_diff_chunk)] <- con_chunk
tracking_data$Con_Chunk <- as.integer(frame_diff_chunk)

# Splitting by continous frame data is not good enough because 
# there are still jumps in the X,Y when 'Tracker' changes within a continuous chunk

# Method 2 - split by tracker and continous frame chunks (end up with 2 keys)

tracker_index <- 1+which(diff(tracking_data$Tracker_Num)!=0)
tracker_diff_chunk <- vector("integer", length = length(tracking_data$V1))

track_chunk <- 1
for(j in (1:length(tracker_index))){
  if(j == 1){
    tracker_diff_chunk[0:tracker_index[j]] <- track_chunk
    track_chunk <- track_chunk + 1
  } else{
    tracker_diff_chunk[tracker_index[j-1]:tracker_index[j]] <- track_chunk
    track_chunk <- track_chunk + 1
  }
}

tracker_diff_chunk[tracker_index[j]:length(tracker_diff_chunk)] <- track_chunk
tracking_data$Track_Chunk <- as.integer(tracker_diff_chunk)

# Find size of each Tracker_Chunk

Track_Chunk_Size <- diff(c(0,tracker_index, length(tracker_diff_chunk)))
tracker_diff_chunk_df <- data.frame(Track_Chunk = c(1:length(Track_Chunk_Size)),
                                    Track_Chunk_Size)
tracking_data <- merge(x = tracking_data, y = tracker_diff_chunk_df, by = "Track_Chunk", all=T)
# Same issue as with Attempt 1. Therefore need a combination of splitting the data into continous frame chunks 
# and into tracker chunks.

