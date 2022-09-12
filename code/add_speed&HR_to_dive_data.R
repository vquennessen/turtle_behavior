# adding in GPS data (home range and speed) per surface interval
# Vic Quennessen
# last updated: 9/5/2022

# set working directory
setwd("~/Projects/turtle_behavior")

# load libraries
library(dplyr)

# # strip timezone function
# strip.tz <- function(dt) {
#   fmt <- "%Y-%m-%d %H:%M:%S"
#   strptime(strftime(dt, format = fmt, tz=""), format = fmt, tz="UTC")
# }

# list all turtle IDs to be analyzed
turtle_IDs <- c(41589, 41591, 146294, 146295, 146297, 146298, 146299, 146300, 
                146303, 146304)

# import data: make sure to change this so you import the data from your own 
# working directory

# import GPS data (will need the turtle ID in the GPS file name)
GPS <- read.csv('data/New_data_H_R_3_2.csv')

# convert Day_time object to as.POSIXlt
GPS$time <- as.POSIXlt(GPS$Day_time, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')

# # remove timezone
# strip.tz(GPS$time)

# import dive data
all_dive <- read.csv('data/Dive&Surface&Activity_only_complete_IN.csv', 
                     header = TRUE)

# add speed and home range columns
all_dive$speed <- NA
all_dive$homerange <- NA

# transform End column to as.POSIXlt object
all_dive$new_End <- as.POSIXlt(all_dive$End, 
                               format = "%m/%d/%Y %H:%M", tz = 'UTC')

# # remove timezone
# strip.tz(all_dive$new_End)

# intialize empty dataframe
new_all_dive <- data.frame()

# for each turtle you want to ID 
for (t in 1:length(turtle_IDs)) {
  
  # filter out turtle from dive data and remove nesting rows 
  turtle_GPS <- GPS %>% 
    filter(Name == turtle_IDs[t]) %>%
    filter(Activity == 1)
  
  # filter out turtle from dive data 
  turtle_dive <- all_dive %>% 
    filter(Name == turtle_IDs[t]) 
  
  # for each row in the turtle_dive object
  for (i in 1:nrow(turtle_dive)) {
    
    # if AvgSurfDuration is not equal to zero (i.e. it's a surface row)
    if (turtle_dive$Activity[i] == 1 & 
        turtle_dive$AvgSurfDuration[i] > 0 &
        turtle_dive$new_End[i] > min(turtle_GPS$time, na.rm = TRUE)) {
      
      # extract time of end of surface
      end_time <- turtle_dive$new_End[i]
      
      # find GPS time point that is closest to this time, but below it
      exists <- any(turtle_GPS$time == max(turtle_GPS$time[turtle_GPS$time < end_time]))
      
      # if the index exists, calculate it
      if (exists == TRUE) {
        
        GPS_index <- which(turtle_GPS$time == max(turtle_GPS$time[turtle_GPS$time < end_time]))
        
      }
      
      if (is.integer(GPS_index)) {
        
        # apply speed and home range values to new_all_dive 
        turtle_dive$speed[i] <- turtle_GPS$s[GPS_index]
        turtle_dive$homerange[i] <- turtle_GPS$Home_range[GPS_index]
        
      } 
      
    }
    
  }
  
  # add this turtle to new_all_dive file to be saved
  new_all_dive <- rbind(new_all_dive, turtle_dive)
  
}

# filter out duplicate speeds by only saving the first one (not counting NAs)
# turtle_dive$speed[duplicated(turtle_dive$speed, incomparables = NA)] <- NA

# duplicate TRUE/FALSE vector
duplicates <- duplicated(new_all_dive$speed, incomparables = NA)

if (sum(duplicates) > 0) {
  
  # duplicate indices
  d_ind <- which(duplicates)
  
  # for each duplicate, backwards, replace with NA if the previous duplicate 
  # has index - 2
  for (d in length(d_ind):1) {
    
    if (!is.na(new_all_dive$speed[d_ind[d] - 2]) &
        !is.na(new_all_dive$speed[d_ind[d]])) {
      
      # if the speed is the same as the speed 2 indices earlier
      if (new_all_dive$speed[d_ind[d]] == new_all_dive$speed[d_ind[d] - 2]) {
        
        # replace the speed with NA
        new_all_dive$speed[d_ind[d]] <- -3
        
      }
      
    }
    
  }
  
}

# where speed = NA, set homerange = NA
new_all_dive$homerange[which(is.na(new_all_dive$speed) == TRUE)] <- -3
new_all_dive$speed[which(is.na(new_all_dive$speed) == TRUE)] <- -3

# save new_all_dive file
write.csv(new_all_dive, 
          file = '~/Projects/turtle_behavior/data/new_Dive&Surface&Activity_only_complete_IN.csv')
