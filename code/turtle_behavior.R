# turtle dive behavior
# Vic Quennessen
# last updated: June 12, 2022

# load libraries
library(dplyr)
library(lubridate)

# list all turtle IDs to be analyzed
turtle_IDs <- c(41589)

# import data: make sure to change this so you import the data from your own 
# working directory

# import GPS data
# GPS <- read.csv('~/Projects/turtle_behavior/data/IN_Ei_data_processed_2_hour.csv', 
#                 header = TRUE)

GPS <- read.csv('~/Projects/turtle_behavior/data/IN_Ei_data_processed_without_nesting.csv',
                header = TRUE)

# for each turtle ID: 
for (t in 1:length(turtle_IDs)) {
  
  # filter out GPS data for turtle
  GPS_turtle <- GPS %>% filter(id == turtle_IDs[t])
  
  # import behavior data for turtle
  behavior <- read.csv(paste('~/Projects/turtle_behavior/data/', turtle_IDs[t], 
                             '-Behavior.csv', sep = ''), header = TRUE)
  
  # make date objects
  behavior$Start <- as.POSIXlt(behavior$Start, format = "%H:%M:%S %d-%b-%Y")
  behavior$End <- as.POSIXlt(behavior$End, format = "%H:%M:%S %d-%b-%Y")
  GPS_turtle$date <- as.POSIXlt(GPS_turtle$date, 
                                format = "%Y-%m-%d %H:%M:%S")
  
  # trim behavior based on GPS intervals - comment out if not wanted
  behavior <- behavior %>%
    filter(behavior$Start > GPS_turtle$date[1] & 
             behavior$End < GPS_turtle$date[nrow(GPS_turtle)])
  
  # extract only dives from behavior
  dives <- behavior %>% filter(What == 'Dive')
  
  # extract only surfaces from behavior
  surfaces <- behavior %>% filter(What == 'Surface')
  
  # Dive duration (seconds)
  # create total time interval column in dives dataframe, in seconds
  dives$Time_sec <- as.numeric(dives$End - dives$Start)
  
  # create total time interval column in surfaces dataframe, in seconds
  surfaces$Time_sec <- as.numeric(surfaces$End - surfaces$Start)
  
  # Dive depth (m)
  # create depth column that's the average between DepthMin and DepthMax
  dives$Depth <- (dives$DepthMin + dives$DepthMax) / 2
  
  # add dive and surface columns to GPS_turtle
  GPS_turtle$Number_dives <- NA               # total number of dives
  GPS_turtle$Dives <- NA                      # dive indices
  GPS_turtle$Total_dive_time_sec <- NA        # total dive time, in seconds
  GPS_turtle$Avg_dive_time_sec <- NA          # average dive time, in seconds
  GPS_turtle$Avg_dive_depth <- NA             # average dive depth, in meters
  GPS_turtle$SD_dive_depth <- NA              # standard deviation of dive depth
  
  GPS_turtle$Number_surfaces <- NA            # total number of surfaces
  GPS_turtle$Surfaces <- NA                   # surface indices
  GPS_turtle$Total_surface_time_sec <- NA     # total surface time, in seconds
  GPS_turtle$Avg_surface_time_sec <- NA       # average surface time, in seconds
  
  # for each interval, count number of dives
  for (gps in 1:(nrow(GPS_turtle))) {
    
    if (gps < nrow(GPS_turtle)) {
      
      # how many dives are fully inside the interval
      whole_dives <- which(dives$Start >= GPS_turtle$date[gps] & 
                             dives$End <= GPS_turtle$date[gps + 1])
      
      # how many surfaces are fully inside the interval
      whole_surfaces <- which(surfaces$Start >= GPS_turtle$date[gps] & 
                                surfaces$End <= GPS_turtle$date[gps + 1])
      
      # how many dives overlap with this interval
      partial_dives <- which((dives$Start <= GPS_turtle$date[gps] & 
                                dives$End >= GPS_turtle$date[gps] &
                                dives$End <= GPS_turtle$date[gps + 1]) |
                               (dives$Start <= GPS_turtle$date[gps] &
                                  dives$End >= GPS_turtle$date[gps + 1]) |
                               (dives$Start >= GPS_turtle$date[gps] &
                                  dives$Start <= GPS_turtle$date[gps + 1] &
                                  dives$End >= GPS_turtle$date[gps + 1]))
      
      # how many surfaces overlap with this interval
      partial_surfaces <- which((surfaces$Start <= GPS_turtle$date[gps] & 
                                   surfaces$End >= GPS_turtle$date[gps] &
                                   surfaces$End <= GPS_turtle$date[gps + 1]) |
                                  (surfaces$Start <= GPS_turtle$date[gps] &
                                     surfaces$End >= GPS_turtle$date[gps + 1]) |
                                  (surfaces$Start >= GPS_turtle$date[gps] &
                                     surfaces$Start <= GPS_turtle$date[gps + 1] &
                                     surfaces$End >= GPS_turtle$date[gps + 1]))
      
      # process partial dives, assign those with > 50% of dive time to interval
      if (length(partial_dives) > 0) {
        
        # extract dive times and initialize vector for how many seconds from the 
        # partial dives are in this interval
        dive_times <- dives$Time_sec[partial_dives]
        seconds_overlap <- rep(NA, length(partial_dives))
        
        # for each partial dive
        for (pd in 1:length(partial_dives)) {
          
          # time of dives in interval - in seconds
          GPS_interval <- interval(GPS_turtle$date[gps], GPS_turtle$date[gps + 1])
          dive_interval <- interval(dives$Start[partial_dives[pd]], 
                                    dives$End[partial_dives[pd]])
          seconds_overlap[pd] <- lubridate::second(
            as.period(intersect(GPS_interval, dive_interval), "seconds"))
        }
        
        # add actual dive numbers to GPS dataframe, with partial dives
        GPS_turtle$Dives[gps] <- list(sort(unique(c(whole_dives, 
                                                    partial_dives[seconds_overlap >= 
                                                                    0.5*dive_times]))))
        
        # if there are no partial dives
      } else if (length(partial_dives) == 0) {
        
        # add actual dive numbers to GPS dataframe, with no partial dives
        GPS_turtle$Dives[gps] <- list(c(whole_dives))
        
      }
      
      # process partial surfaces, assign those with > 50% of surface time to interval
      if (length(partial_surfaces) > 0) {
        
        # extract surface times and initialize vector for how many seconds from the 
        # partial surfaces are in this interval
        surface_times <- surfaces$Time_sec[partial_surfaces]
        seconds_overlap_surfaces <- rep(NA, length(partial_surfaces))
        
        # for each partial surface
        for (ps in 1:length(partial_surfaces)) {
          
          # time of surfaces in interval - in seconds
          GPS_interval_surfaces <- interval(GPS_turtle$date[gps], GPS_turtle$date[gps + 1])
          surface_interval_surfaces <- interval(surfaces$Start[partial_surfaces[ps]], 
                                                surfaces$End[partial_surfaces[ps]])
          seconds_overlap_surfaces[ps] <- lubridate::second(
            as.period(intersect(GPS_interval_surfaces, surface_interval_surfaces), 
                      "seconds"))
        }
        
        # add actual surface numbers to GPS dataframe, with partial surfaces
        GPS_turtle$Surfaces[gps] <- list(sort(unique(c(whole_surfaces, 
                                                       partial_surfaces[seconds_overlap_surfaces >= 
                                                                          0.5*surface_times]))))
        
        # if there are no partial surfaces
      } else if (length(partial_surfaces) == 0) {
        
        # add actual surface numbers to GPS dataframe, with no partial surfaces
        GPS_turtle$Surfaces[gps] <- list(c(whole_surfaces))
        
      }
      
      # remove any dives (after interval 1) that are in the previous interval already
      if (gps > 1 & sum (unlist(GPS_turtle$Dives[gps]) %in% 
                         unlist(GPS_turtle$Dives[gps - 1])) > 0) {
        
        # TRUE if repeated, FALSE if not repeated
        repeated <- unlist(GPS_turtle$Dives[gps]) %in% 
          unlist(GPS_turtle$Dives[gps - 1])
        
        # to keep (if not repeated)
        GPS_turtle$Dives[gps] <- list(unlist(GPS_turtle$Dives[gps])[which(repeated == FALSE)])
        
      }
      
      # remove any surfaces (after interval 1) that are in the previous interval already
      if (gps > 1 & sum (unlist(GPS_turtle$Surfaces[gps]) %in% 
                         unlist(GPS_turtle$Surfaces[gps - 1])) > 0) {
        
        # TRUE if repeated, FALSE if not repeated
        repeated <- unlist(GPS_turtle$Surfaces[gps]) %in% 
          unlist(GPS_turtle$Surfaces[gps - 1])
        
        # to keep (if not repeated)
        GPS_turtle$Surfaces[gps] <- list(unlist(GPS_turtle$Surfaces[gps])[which(repeated == FALSE)])
        
      }
      
      # for last interval, where there is no next interval
    } else if (gps == nrow(GPS_turtle)) {
      
      ##########################################################################
      
      ##### error handling #####################################################
      # ##### dives and intervals split between 2 intervals evenly ###############
      # 
      # ##### dives
      # 
      # # duplicated dives
      # dup_dives <- anyDuplicated(unlist(GPS_turtle$Dives[1:(gps - 1)]))
      # 
      # # if there are any duplicated dives: 
      # if (dup_dives > 0) {
      #   
      #   # for each duplicated dive
      #   for (dd in dup_dives) {
      #     
      #     # identify the GPS intervals that contain the duplicates
      #     intervals <- GPS_turtle$Dives[sapply(GPS_turtle$Dives, 
      #                                          `%in%`, x = dd)]
      #     
      #     # remove dive from not minimum value
      #     shortcut <- GPS_turtle$Dives[max(intervals)]  # set dives shortcut
      #     GPS_turtle$Dives[max(intervals)] <- shortcut[shortcut != dd]
      #     
      #   }
      #   
      # }
      # 
      # ##### surfaces
      # 
      # # duplicated surfaces
      # dup_surfaces <- anyDuplicated(unlist(GPS_turtle$Surfaces[1:(gps - 1)]))
      # 
      # # if there are any duplicated surfaces: 
      # if (dup_surfaces > 0) {
      #   
      #   # for each duplicated surface
      #   for (dd in dup_surfaces) {
      #     
      #     # identify the GPS intervals that contain the duplicates
      #     intervals <- GPS_turtle$Surfaces[sapply(GPS_turtle$Surfaces, 
      #                                             `%in%`, x = dd)]
      #     
      #     # remove dive from not minimum value
      #     # set surfaces shortcut
      #     shortcut <- GPS_turtle$Surfaces[max(intervals)]  
      #     GPS_turtle$Surfaces[max(intervals)] <- shortcut[shortcut != dd]
      #     
      #   }
      #   
      # }
      # 
      ##### dives and surfaces split over > 2 intervals ########################
      
      ##### dives
      
      # missing dives 
      missing_dives <- which(! 1:nrow(dives) %in% unlist(GPS_turtle$Dives[1:(gps - 1)]))
      
      # if there are any missing dives
      if (length(missing_dives) > 0) {
        
        # for each missing dive
        for (md in 1:length(missing_dives)) {
          
          # dive interval
          dive_interval <- interval(dives$Start[missing_dives[md]], 
                                    dives$End[missing_dives[md]])
          
          # find start and end intervals
          start_int <- max(which(GPS_turtle$date < dives$Start[missing_dives[md]]))
          end_int <- max(which(GPS_turtle$date < dives$End[missing_dives[md]]))
          intervals <- start_int:end_int
          
          # initialize seconds_overlap vector
          seconds_overlap <- rep(NA, length(intervals))
          
          # for each interval
          for (i in 1:length(intervals)) {
            
            # time of dives in interval - in seconds
            GPS_interval <- interval(GPS_turtle$date[intervals[i]], 
                                     GPS_turtle$date[intervals[i] + 1])
            
            seconds_overlap[i] <- lubridate::second(
              as.period(intersect(GPS_interval, dive_interval), "seconds"))
          }
          
          # minimum interval with maximum overlap
          max_overlap <- max(seconds_overlap)
          max_interval <- intervals[min(which(seconds_overlap == max_overlap))]
          
          # assign dive to that interval
          
          # if there are already other dives in that interval
          if (is.numeric(GPS_turtle$Dives[max_interval])) {
            GPS_turtle$Dives[max_interval] <- list(unlist(GPS_turtle$Dives[max_interval]),
                                                   missing_dives[md])
            GPS_turtle$Number_dives[max_interval] <- GPS_turtle$Number_dives[max_interval] + 1
            GPS_turtle$Total_dive_time_sec[max_interval] <- 
              GPS_turtle$Total_dive_time_sec[max_interval] + max_overlap
            GPS_turtle$Avg_dive_time_sec[max_interval] <- 
              mean(c(unlist(GPS_turtle$Avg_dive_time_sec[max_interval]), max_overlap))
            GPS_turtle$Avg_dive_depth[max_interval] <- 
              mean(c(unlist(GPS_turtle$Avg_dive_depth[max_interval]), 
                     dives$Depth[missing_dives[md]]))            
            
            # if there were no other dives in that interval
          } else {
            GPS_turtle$Dives[max_interval] <- list(missing_dives[md])
            GPS_turtle$Number_dives[max_interval] <- 1
            GPS_turtle$Total_dive_time_sec[max_interval] <- max_overlap
            GPS_turtle$Avg_dive_time_sec[max_interval] <- max_overlap
            GPS_turtle$Avg_dive_depth[max_interval] <- dives$Depth[missing_dives[md]]
            
          }
          
        }
        
      }
      
      ##### surfaces
      
      # missing surfaces 
      missing_surfaces <- which(! 1:nrow(surfaces) %in% unlist(GPS_turtle$Surfaces[1:(gps - 1)]))
      
      # if there are any missing dives
      if (length(missing_surfaces) > 0) {
        
        # for each missing dive
        for (ms in 1:length(missing_surfaces)) {
          
          # dive interval
          surface_interval <- interval(surfaces$Start[missing_surfaces[ms]], 
                                       surfaces$End[missing_surfaces[ms]])
          
          # find start and end intervals
          start_int <- max(which(GPS_turtle$date < surfaces$Start[missing_surfaces[ms]]))
          end_int <- max(which(GPS_turtle$date < surfaces$End[missing_surfaces[ms]]))
          intervals <- start_int:end_int
          
          # initialize seconds_overlap vector
          seconds_overlap <- rep(NA, length(intervals))
          
          # for each interval
          for (i in 1:length(intervals)) {
            
            # time of surfaces in interval - in seconds
            GPS_interval <- interval(GPS_turtle$date[intervals[i]], 
                                     GPS_turtle$date[intervals[i] + 1])
            
            seconds_overlap[i] <- lubridate::second(
              as.period(intersect(GPS_interval, surface_interval), "seconds"))
          }
          
          # minimum interval with maximum overlap
          max_overlap <- max(seconds_overlap)
          max_interval <- intervals[min(which(seconds_overlap == max_overlap))]
          
          # assign surface to that interval
          
          # if there are already other surfaces in that interval
          if (is.numeric(GPS_turtle$Surfaces[max_interval])) {
            GPS_turtle$Surfaces[max_interval] <- list(unlist(GPS_turtle$Surfaces[max_interval]),
                                                      missing_surfaces[ms])
            GPS_turtle$Number_surfaces[max_interval] <- GPS_turtle$Number_surfaces[max_interval] + 1
            GPS_turtle$Total_surface_time_sec[max_interval] <- 
              GPS_turtle$Total_surface_time_sec[max_interval] + max_overlap
            GPS_turtle$Avg_surface_time_sec[max_interval] <- 
              mean(c(unlist(GPS_turtle$Avg_surface_time_sec[max_interval]), max_overlap))
            
            # if there were no other surfaces in that interval
          } else {
            GPS_turtle$Surfaces[max_interval] <- list(missing_surfaces[ms])
            GPS_turtle$Number_surfaces[max_interval] <- 1
            GPS_turtle$Total_surface_time_sec <- max_overlap
            GPS_turtle$Avg_surface_time_sec <- max_overlap
            
          }
          
        }
        
      }
      
      
      # all leftover dives assigned to last interval
      GPS_turtle$Dives[gps] <- list(setdiff(1:nrow(dives), 
                                            unlist(GPS_turtle$Dives[1:(gps-1)])))
      
      # all leftover surfaces assigned to last interval
      GPS_turtle$Surfaces[gps] <- list(setdiff(1:nrow(surfaces), 
                                               unlist(GPS_turtle$Surfaces[1:(gps-1)])))
      
    }
    
    # add whole dives + any partial dives that are 50% or more of the dive time 
    # to GPS dataframe in the Dives column
    GPS_turtle$Number_dives[gps] <- length(GPS_turtle$Dives[[gps]])
    
    # add dive times to GPS dataframe, in seconds
    GPS_turtle$Total_dive_time_sec[gps] <- sum(dives$Time_sec[GPS_turtle$Dives[[gps]]])
    
    # add average dive time to GPS dataframe, in seconds
    GPS_turtle$Avg_dive_time_sec[gps] <- mean(dives$Time_sec[GPS_turtle$Dives[[gps]]])
    
    # add average dive depth to GPS dataframe, in m
    GPS_turtle$Avg_dive_depth[gps] <- mean(dives$Depth[GPS_turtle$Dives[[gps]]])
    
    # add standard deviation of dive depth to GPS dataframe
    GPS_turtle$SD_dive_depth[gps] <- sd(dives$Depth[GPS_turtle$Dives[[gps]]])
    
    # add whole surfaces + any partial surfaces that are 50% or more of the surface time 
    # to GPS dataframe in the surfaces column
    GPS_turtle$Number_surfaces[gps] <- length(GPS_turtle$Surfaces[[gps]])
    
    # add surface times to GPS dataframe, in seconds
    GPS_turtle$Total_surface_time_sec[gps] <- sum(surfaces$Time_sec[GPS_turtle$Surfaces[[gps]]])
    
    # add average surface time to GPS dataframe, in seconds
    GPS_turtle$Avg_surface_time_sec[gps] <- mean(surfaces$Time_sec[GPS_turtle$Surfaces[[gps]]])
    
  }
  
  ##### dives and surfaces with integer(0) #####################################
  
  ##### dives
  
  # indices of number_dives = 0
  ind_dives <- which(GPS_turtle$Number_dives == 0)
  
  # set all relevant values to NA
  GPS_turtle$Dives[ind_dives] <- NA
  GPS_turtle$Total_dive_time_sec[ind_dives] <- NA
  GPS_turtle$Avg_dive_time_sec[ind_dives] <- NA
  GPS_turtle$Avg_dive_depth[ind_dives] <- NA
  
  ##### surfaces
  
  # indices of number_surfaces = 0
  ind_surfaces <- which(GPS_turtle$Number_surfaces == 0)
  
  # set all relevant values to NA
  GPS_turtle$Surfaces[ind_surfaces] <- NA
  GPS_turtle$Total_surface_time_sec[ind_surfaces] <- NA
  GPS_turtle$Avg_surface_time_sec[ind_surfaces] <- NA
  
  ##############################################################################
  
  # # check if any dives are in more than one row / interval (if not, should be 0): 
  # anyDuplicated(unlist(GPS_turtle$Dives))
  
  # # check if any surfaces are in more than one row / interval (if not, should be 0): 
  # anyDuplicated(unlist(GPS_turtle$Surfaces))
  
  if (t == 1) {
    
    new_GPS <- GPS_turtle
    
  } else if (t > 1) {
    
    new_GPS <- rbind(new_GPS, GPS_turtle)
    
  }
  
}

# make Dives and Surfaces columns character vectors instead of lists
new_GPS$Dives <- paste(new_GPS$Dives)
new_GPS$Surfaces <- paste(new_GPS$Surfaces)

# write new GPS dataframe to new .csv file
write.csv(new_GPS, file = '~/Projects/iliketurtles/data/new_GPS.csv')
