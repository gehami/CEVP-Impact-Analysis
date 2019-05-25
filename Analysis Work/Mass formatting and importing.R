rm(list=ls()) #clearing environment

######### Standard Functions ##########


sample_dat = function(dat, rows = 10){
  return(dat[sample(1:nrow(dat), rows),])
}

install_and_load = function(library){
  if(!is.character(library)){
    message("Please type library name as a character value")
    return(NULL)
  }
  if(!require(library, character.only = T)){
    install.packages(library)
    require(library, character.only = T)
  }
  return(NULL)
}

####### Loading Libraries ###########
install_and_load('sp')
install_and_load('rgdal')
install_and_load('raster')
install_and_load('dplyr')
install_and_load('leaflet')
install_and_load('rgeos')
install_and_load('magrittr')


####### CONSTANTS ########

NEW_TRIP_THRESHOLD = 8 #in seconds. No longer used to mark new trips, but used to mark the buffer for when multiple trips are happening at the same time. 
PROJ_4_STRING = CRS('+init=epsg:4326')
COUNTY_INTERSECTION_BUFFER = 33 #in meters
TRIP_POINTS_BUFFER = 20 #in meters
DEFAULT_SPEED = 25 #in mph
CAD_FILE_DIRECTORY = 'CAD data/Trip Files'


########### Getting list of all files #############

files = list.files(CAD_FILE_DIRECTORY)


############ Loading in Data ###########
i = 1
full_cevp_agg = NULL


cevp = read.csv(paste0(CAD_FILE_DIRECTORY, '/', files[i]), stringsAsFactors = FALSE, header = TRUE) %>% select(-utc, -start_date, -date)

colnames(cevp) = c('date_time', 'veh_id', 'lat', 'lon', 'status', 'inc_id')

cevp$full_id = paste0(cevp$veh_id, '_', cevp$inc_id)


########### Creating spdf to identify when a vehicle goes through a non-sj intersection - non_sj_ints_buffer_spdf ##########

#loading in the county intersection point data
#sheet name: OtherAgencySignals
install_and_load('readxl')
non_sj_intersections = readxl::read_excel('Non-City signals.xlsx', sheet = 'OtherAgencySignals')
non_sj_ints_spdf = sp::SpatialPointsDataFrame(coords = data.frame(lng = non_sj_intersections$Y,
                                                                  lat = non_sj_intersections$X),
                                              data = non_sj_intersections,
                                              proj4string = PROJ_4_STRING)

#It looks like a buffer of 33 meters should do the trick. 
non_sj_ints_buffer = raster::buffer(non_sj_ints_spdf, width = COUNTY_INTERSECTION_BUFFER, dissolve = FALSE)

#making a spdf from the non_sj_ints_buffer
non_sj_ints_buffer_polys = non_sj_ints_buffer@polygons
non_sj_ints_buffer_spolys = SpatialPolygons(non_sj_ints_buffer_polys, proj4string = PROJ_4_STRING)
non_sj_ints_buffer_spdf = SpatialPolygonsDataFrame(non_sj_ints_buffer_spolys, data = data.frame(non_sj_ints_spdf@data))


######## Pulling in Roads with Speed Limit Data - roads_with_speed_limits #######

#opening the roads file
roads_with_speed_limits = readOGR('SpeedSegments/doc.kml') #this seems to be the easiest and best way

roads_with_speed_limits = spTransform(roads_with_speed_limits, PROJ_4_STRING)

#In the file we get speed limits in the data. It is hidden within the 'Description' like so: '<td>SPD</td> <td>35</td>'
#let's extract that out. 

#convert the html to text
install_and_load('XML')
text_of_html = htmltools::htmlEscape(roads_with_speed_limits@data$Description)

with_spd_limit = regexpr('SPD&lt;/td&gt; &lt;td&gt;[0-9]+', text_of_html)#identifies the location of the last number in the speed limit
without_spd_limit = regexpr('SPD&lt;/td&gt; &lt;td&gt;[0-9]', text_of_html)#identifies the location of the first number in the speed limit

# #this gets the length of the text match
# attributes(with_spd_limit)$match.length
# #this gets the starting position of the text match
# as.numeric(with_spd_limit)
speed_limits = substr(text_of_html, start = (as.numeric(without_spd_limit) + attributes(without_spd_limit)$match.length - 1),
                      stop = (as.numeric(with_spd_limit) + attributes(with_spd_limit)$match.length - 1))#pulls the speed limit out of the jumble of text.

roads_with_speed_limits@data = data.frame(Name = roads_with_speed_limits@data[,1], speed_limit = as.numeric(speed_limits))
#so it looks like this is not all roads, but just some roads. we need to check on this. 

#yeah so it covers the main roads pretty well, but the small roads are not covered much at all. 
# map_with_roads <- map %>% addPolylines(data = roads_with_speed_limits, color = '#7af441') %>% addPolygons(data = non_sj_ints_buffer, color = '#f4426e') %>%
#   addCircleMarkers(data = trip_spdf, radius = 1)


######## Pulling in all intersection information - intersections_buffer_spdf ###########

#opening the intersections file
intersections = readOGR('SignalRetimingIntersections/doc.kml', layer = 'RetimingSignals') #this seems to be the easiest and best way
# corridors = readOGR('SignalRetimingIntersections/doc.kml', layer = 'Corridors')

intersections = spTransform(intersections, PROJ_4_STRING)


intersections_buffer = raster::buffer(intersections, width = COUNTY_INTERSECTION_BUFFER, dissolve = FALSE)

#making a spdf from the intersections_buffer
intersections_buffer_polys = intersections_buffer@polygons
intersections_buffer_spolys = SpatialPolygons(intersections_buffer_polys, proj4string = PROJ_4_STRING)
intersections_buffer_spdf = SpatialPolygonsDataFrame(intersections_buffer_spolys, data = data.frame(id = seq_len(length(intersections_buffer_polys))))

rm(intersections, intersections_buffer, intersections_buffer_polys, intersections_buffer_spolys)


########## Marking the CEVP signal intersections - cevp_ints_buffer_spdf ################ 

cevp_ints = readOGR('CEVP_Intersections', layer = 'CEVP_Intersections') #this seems to be the easiest and best way

cevp_ints = spTransform(cevp_ints, PROJ_4_STRING)


cevp_ints_buffer = raster::buffer(cevp_ints, width = COUNTY_INTERSECTION_BUFFER, dissolve = FALSE)

#making a spdf from the cevp_ints_buffer
cevp_ints_buffer_polys = cevp_ints_buffer@polygons
cevp_ints_buffer_spolys = SpatialPolygons(cevp_ints_buffer_polys, proj4string = PROJ_4_STRING)
cevp_ints_buffer_spdf = SpatialPolygonsDataFrame(cevp_ints_buffer_spolys, data = data.frame(id = seq_len(length(cevp_ints_buffer_polys))))

rm(cevp_ints, cevp_ints_buffer, cevp_ints_buffer_polys, cevp_ints_buffer_spolys)


######## creating a posix date time var from the date_time var ###########

add_dt = function(cevp){
  date_time_reformat = gsub('([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([[:print:]]*)', 
                            '\\1-\\2-\\3 \\4:\\5:\\6', cevp$date_time)#formats date_time into a machine-readable format to convert into a date_time object
  dt = as.POSIXct(date_time_reformat)
  cevp$dt = dt
  return(cevp)
  
}


######### Calculating the time (in seconds) from one record to the next for a vehicle ##########


add_time_diffs = function(cevp){
  #if we order cevp by vehicle and then by dt we can get a clear picture
  cevp = cevp[order(cevp$full_id, cevp$dt),]
  
  #since the dataset is now ordered by veh_id and then by time, we can just subtract dt[i] from dt[i+1] to get the difference in time from each log
  #this will lead to us substracting the first time stamp of veh_id i from the last time stamp of veh_id i-1. To fix this we
  #identify all time differences that are from two different vechiles and set those to 0.
  last_time = cevp$dt[-length(cevp$dt)] 
  new_time = cevp$dt[-1] 
  
  time_diff = new_time - last_time #this is the difference in seconds from each interval
  
  last_id = cevp$full_id[-nrow(cevp)]#marking which time diffs are from two different trips/vehicles to set them to 0
  new_id = cevp$full_id[-1]
  
  
  time_diff[last_id != new_id] = 0
  
  #setting the time diff for the initial observation to 0
  time_diff = c(0, time_diff)
  
  cevp$time_diff = time_diff
  
  return(cevp)
}


######### Marking when a trip begins and ends ##########

add_trip_start_and_end = function(cevp){
  last_id = cevp$full_id[-nrow(cevp)]
  new_id = cevp$full_id[-1]
  trip_begin = rep(0, nrow(cevp))
  
  #marking trips beginning when the current id (new_id) does not match the previous id (last_id). adding 1 to the indexes to match proper indexes
  trip_begin[which(last_id != new_id)+1] = 1
  
  #any observation before a trip_begin is a trip_end mark
  trip_end = rep(0, length(trip_begin))
  trip_end[which(trip_begin == 1) - 1] = 1
  
  #marking the first observation as the beginning of a trip.
  trip_begin[1] = 1
  #marking the last observation as the end of a trip
  trip_end[length(trip_end)] = 1
  
  #adding trip_begin and trip_end as vars to cevp
  cevp$trip_begin = trip_begin
  cevp$trip_end = trip_end
  #works
  
  return(cevp)
  
  
}


########### counting the number of vehicles in operation at any given time #############

add_other_veh_count = function(cevp, NEW_TRIP_THRESHOLD){
  # obs_ind = 283
  #given an ind of interest, the cevp data, and the threshold for time, returns the number of fire, police, and total emergency vehicles out at any given time beyond the vehicle of interest.
  get_other_vehicles = function(obs_ind, cevp, NEW_TRIP_THRESHOLD){
    veh_inds = which(cevp$dt < cevp$dt[obs_ind] + NEW_TRIP_THRESHOLD & cevp$dt > cevp$dt[obs_ind] - NEW_TRIP_THRESHOLD)
    # time_diffs = abs(cevp$dt - cevp$dt[obs_ind])
    veh = as.character(unique(cevp$veh_id[veh_inds]))
    veh = veh[which(!(veh %in% cevp$veh_id[obs_ind]))]
    return(veh)
    # works
  }
  cevp$num_veh = 0
  for(n in seq_len(nrow(cevp))){
    other_veh = get_other_vehicles(n, cevp, NEW_TRIP_THRESHOLD)
    cevp$num_veh[n] = length(other_veh)
  }
  
  # target_time = cevp$dt[2]
  # 
  # head(cevp[cevp$veh_id == 'B13' & cevp$dt < target_time + NEW_TRIP_THRESHOLD & cevp$dt > target_time - NEW_TRIP_THRESHOLD,])
  return(cevp)
  #works
  
}

########### aggregating each trip into a single observation - cevp_agg ###############

# trip = cevp[cevp$full_id == unique(cevp$full_id)[3],]
#given the data for a single trip, aggregates all of the data of a trip into a single row. Can be edited to make different kinds of data
get_agg_cevp = function(trip, non_sj_ints_buffer_spdf, PROJ_4_STRING, roads_with_speed_limits,
                        TRIP_POINTS_BUFFER, 
                        cevp_ints_buffer_spdf){
  
  #creating a spatial version of the trip. Will come in handy here and later on. 
  trip_spdf = sp::SpatialPointsDataFrame(coords = data.frame(lng = trip$lon, lat = trip$lat),
                                         data = trip, proj4string = PROJ_4_STRING)
  
  #getting the x and y coordinates for dist calculations later. note that utm coords are in meters, so distance will also be in meters. yay.

  trip_spdf_xy = spTransform(trip_spdf, CRS("+proj=utm +zone=10 +datum=WGS84"))
  trip$x_ll = trip_spdf_xy@coords[,1]
  trip$y_ll = trip_spdf_xy@coords[,2]

  
  
  #adding buffer to roads is too hard. Add buffer to trip points instead. 
  trip_buffer = buffer(trip_spdf, TRIP_POINTS_BUFFER, dissolve =FALSE)
  
  #making a spdf from the non_sj_ints_buffer
  trip_buffer_polys = trip_buffer@polygons
  trip_buffer_spolys = SpatialPolygons(trip_buffer_polys, proj4string = PROJ_4_STRING)
  trip_buffer_spdf = SpatialPolygonsDataFrame(trip_buffer_spolys, data = data.frame(trip_spdf@data), match.ID = FALSE)
  
  roads_trip_is_on_matrix = gIntersects(trip_buffer_spdf, roads_with_speed_limits, byid = TRUE) #this works
  
  #in the case of multiple roads being within the trip_point's buffer, we default to the faster speed.
  point_speed = rep(DEFAULT_SPEED, nrow(trip_spdf))
  for(n in seq_along(point_speed)){
    point_speed[n] = max(roads_with_speed_limits@data[which(as.logical(roads_trip_is_on_matrix[,n])),2], DEFAULT_SPEED)
  }#this works
  
  
  last_ind = nrow(trip)
  ret_dat = data.frame(full_id = trip$full_id[1], veh_id = trip$veh_id[1],
                       start_lon = trip$lon[1], start_lat = trip$lat[1], end_lon = trip$lon[last_ind], end_lat = trip$lat[last_ind],
                       start_time = trip$dt[1], end_time = trip$dt[last_ind], travel_time = sum(trip$time_diff, na.rm = TRUE),
                       avg_time_diff = mean(trip$time_diff, na.rm = TRUE), max_time_diff = max(trip$time_diff, na.rm = TRUE),
                       last_time_diff = trip$time_diff[last_ind],
                       num_veh = weighted.mean(trip$num_veh, trip$time_diff), 
                       avg_speed_limit = weighted.mean(point_speed, trip$time_diff))
  
  # first_y_ll = trip$y_ll[-last_ind]
  # last_y_ll = trip$y_ll[-1]
  # first_x_ll = trip$x_ll[-last_ind]
  # last_x_ll = trip$x_ll[-1]
  # y_ll_dists = abs(first_y_ll - last_y_ll)
  # x_ll_dists = abs(first_x_ll - last_x_ll)
  # dists = sqrt(y_ll_dists^2 + x_ll_dists^2)
  # full_dist = sum(dists)
  #doing all of the above in a single function for the function to go faster. Please see above for a more human-readable version of this.
  ret_dat$full_dist = ((trip$y_ll[-last_ind] - trip$y_ll[-1])^2 + (trip$x_ll[-last_ind] - trip$x_ll[-1])^2) %>% sqrt() %>% sum()
  
  
  #identifying which points are over the buffer points for the non_sj_intersections
  trip_points_over_non_sj_ints = over(trip_spdf, non_sj_ints_buffer_spdf) %>% select(OperatorAgency, IntNo, IntName) %>% filter(!is.na(IntNo))
  num_non_sj_ints = length(unique(trip_points_over_non_sj_ints$IntNo))
  num_county_ints = length(unique(trip_points_over_non_sj_ints$IntNo[grep('County', trip_points_over_non_sj_ints$OperatorAgency)]))
  num_other_non_sj_ints = num_non_sj_ints - num_county_ints
  
  ret_dat$non_sj_ints = num_non_sj_ints
  ret_dat$county_ints = num_county_ints
  ret_dat$other_non_sj_ints = num_other_non_sj_ints

  #counting CEVP intersections the trip goes through. 
  trip_points_over_cevp_ints = cevp_ints_buffer_spdf@data$id[which(!is.na(over(cevp_ints_buffer_spdf, trip_spdf))[,1])] %>% unique()
  
  ret_dat$cevp_ints = length(trip_points_over_cevp_ints)
  
  
  return(ret_dat)
}

agg_cevp_data = function(cevp, non_sj_ints_buffer_spdf, PROJ_4_STRING, roads_with_speed_limits,
                         TRIP_POINTS_BUFFER,
                         cevp_ints_buffer_spdf){

  cevp_agg = get_agg_cevp(cevp[cevp$trip_id == 1,],  non_sj_ints_buffer_spdf, 
                          PROJ_4_STRING, roads_with_speed_limits, TRIP_POINTS_BUFFER,
                          cevp_ints_buffer_spdf)
  for(n in 2 : max(cevp$trip_id)){
    cevp_agg = rbind(cevp_agg, get_agg_cevp(cevp[cevp$trip_id == n,],
                                            non_sj_ints_buffer_spdf, PROJ_4_STRING, 
                                            roads_with_speed_limits, TRIP_POINTS_BUFFER,
                                            #intersections_buffer_spdf, 
                                            cevp_ints_buffer_spdf))
  }
  return(cevp_agg)
}


####### Final Function which uses all above functions ###########

make_agg_cevp_data_from_file = function(cevp, NEW_TRIP_THRESHOLD,
                                        non_sj_ints_buffer_spdf, PROJ_4_STRING, roads_with_speed_limits, 
                                        TRIP_POINTS_BUFFER, cevp_ints_buffer_spdf){
  cevp = add_dt(cevp)
  cevp = add_time_diffs(cevp)
  cevp = add_trip_start_and_end(cevp)
  cevp$trip_id = cumsum(cevp$trip_begin) #keeps track of all the different trips.
  cevp = add_other_veh_count(cevp, NEW_TRIP_THRESHOLD)
  agg_cevp = agg_cevp_data(cevp, non_sj_ints_buffer_spdf, PROJ_4_STRING, roads_with_speed_limits, 
                           TRIP_POINTS_BUFFER, cevp_ints_buffer_spdf)
  return(agg_cevp)
}

######## Looping through each file to create the full dataset - full_cevp_agg ############

full_cevp_agg_list = list() #I am going to attempt to add everything to a list, and then fill in the dataset afterwards to avoid the time-consuming process of rbind-ing after every iteration

for(i in seq_along(files)){
  cevp = read.csv(paste0(CAD_FILE_DIRECTORY, '/', files[i]), stringsAsFactors = FALSE, header = TRUE) %>% select(-utc, -start_date, -date)
  colnames(cevp) = c('date_time', 'veh_id', 'lat', 'lon', 'status', 'inc_id')
  cevp$full_id = paste0(cevp$veh_id, '_', cevp$inc_id)
  
  full_cevp_agg_list[[i]] = make_agg_cevp_data_from_file(cevp, NEW_TRIP_THRESHOLD,
                                                    non_sj_ints_buffer_spdf, PROJ_4_STRING, roads_with_speed_limits,
                                                    TRIP_POINTS_BUFFER, cevp_ints_buffer_spdf)
  print(paste0(i, ' of ', length(files), ' done'))
  
  #save every 10 iterations
  if(i %% 10 == 0){
    saveRDS(full_cevp_agg_list, paste0('CAD data/cevp_agg_list_', i, '_of_', length(files), '_done.rds'))
    if(i > 20){
      #removes the file that is 20 its behind the current it. This way we keep a back-up and a back-up back-up at all times. 
      file.remove(paste0('CAD data/cevp_agg_list_', i-20, '_of_', length(files), '_done.rds'))
    }
  }
}
saveRDS(full_cevp_agg_list, paste0('CAD data/cevp_agg_list_', i, '_of_', length(files), '_done.rds'))



#it looks like each iteration takes a little over a minute. Maybe ~80 seconds. This is an all-day thing you can run on monday.


###### the below is just to get some informatin for Jose Joseph, disregard it. ############
# test = sample_dat(full_cevp_agg_list[[length(full_cevp_agg_list)]])
# print(test)
# write.csv(test, 'clipboard')
# 
#



