Attached is the raw data from our tracking data for the match on Aug 24 2015.

Each line contains the locations of the players at that point in time.  There are 25 measurements per second. The format of the line is:
-The first chunk contains the frame number
-The second chunk contains the players' locations and identities.
-The third chunk can be ignored
Chunks are separated by colons (:)
The format of the second chunk is:
-Each player is separated by semicolons (;)
-The fields for each player are separated by commas
-The fields are:
	Team Id (1=Home, 0=Away)
	System Id (internal use only)
	Jersey Number
	X Position in CM
	Y Position in CM
	Speed in m/s


Please answer the follow questions:
-What is each player's maximum speed in the x direction?
-What is each player's maximum speed?
-How many high intensity efforts did each player perform (anything over 19.8 km/hr)?
-What is each player's maximum acceleration?
-Is a player's ability to accelerate and decelerate symmetrical?

Please include all code and include any assumptions you've made or additional comments you'd like to make about how you arrived at your answers.