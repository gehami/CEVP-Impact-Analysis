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

####### CONSTANTS ########

NEW_TRIP_THRESHOLD = 8
PROJ_4_STRING = CRS('+init=epsg:4326')
COUNTY_INTERSECTION_BUFFER = 33
TRIP_POINTS_BUFFER = 20
DEFAULT_SPEED = 25

############ Loading in Data ###########

cevp = read.delim('CevpTcpMessages.txt', sep = '|', header = FALSE)

colnames(cevp) = c('id', 'date_time', 'company', 'veh_id', 'veh_type', 'x1', 'inc_desc',
                   'inc_subtype', 'inc_type', 'lon', 'lat', 'speed', 'x2', 'x3') #the columns with an 'x' title are marked for removal

cevp = cevp[,grep('^x[0-9]+$', colnames(cevp), invert = TRUE)]

######## Removing some unexpected characters #######

#for some reason the date_time var has this \002 in front of every observation. Let's remove that
cevp$date_time = gsub('^\\\002', '', cevp$date_time)

######## creating a posix date time var from the date_time var ###########

date_time_reformat = gsub('([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})', 
                          '\\1-\\2-\\3 \\4:\\5:\\6', cevp$date_time)#formats date_time into a machine-readable format to convert into a date_time object

dt = as.POSIXct(date_time_reformat)

cevp$dt = dt

rm(date_time_reformat, dt)

######### Calculating the time (in seconds) from one record to the next for a vehicle ##########

#if we order cevp by vehicle and then by dt we can get a clear picture
cevp = cevp[order(cevp$veh_id, cevp$dt),]

#since the dataset is now ordered by veh_id and then by time, we can just subtract dt[i] from dt[i+1] to get the difference in time from each log
#this will lead to us substracting the first time stamp of veh_id i from the last time stamp of veh_id i-1. To fix this we
#identify all time differences that are from two different vechiles and set those to 0.
last_time = cevp$dt[-length(cevp$dt)] 
new_time = cevp$dt[-1] 

time_diff = new_time - last_time #this is the difference in seconds from each interval

last_id = cevp$veh_id[-length(cevp$veh_id)] #marking which time diffs are from two different vehicles to set them to 0
new_id = cevp$veh_id[-1]


time_diff[last_id != new_id] = 0

#setting the time diff for the initial observation to 0
time_diff = c(0, time_diff)

cevp$time_diff = time_diff

rm(last_time, new_time)

############## Identifying the threshold for time difference that likely marks a new journey


# print(quantile(time_diff, seq(0, 1, by = 0.1))) #looks like 3
# print(quantile(time_diff, seq(0, 1, by = 0.01))) #looks like 4
# print(quantile(time_diff, seq(0, 1, by = 0.001))) #looks like 4

#based on this, we can assume that any time_diff greater than 4 seconds marks a different journey. For the sake of simplicity,
#we will set the benchmark to 8 seconds to give additional leeway. 


######### Marking when a trip begins and ends ##########

#first mark that trips begin when the time difference is greater than the threshold (since the time diff is t - (t-1), if it was (t+1) - t then we would mark the trip end based on time_diff)
trip_begin = rep(0, length(time_diff))
trip_begin[time_diff > NEW_TRIP_THRESHOLD] = 1

#also marking trips beginning when the current id (new_id) does not match the previous id (last_id). adding 1 to the indexes to match proper indexes
trip_begin[which(last_id != new_id)+1] = 1


#any observation before a trip_begin is a trip_end mark
trip_end = rep(0, length(time_diff))
trip_end[which(trip_begin == 1) - 1] = 1

#marking the first observation as the beginning of a trip.
trip_begin[1] = 1
#marking the last observation as the end of a trip
trip_end[length(trip_end)] = 1

#adding trip_begin and trip_end as vars to cevp
cevp$trip_begin = trip_begin
cevp$trip_end = trip_end
#works

rm(last_id, new_id, trip_begin, trip_end, time_diff)

############ creating a unique trip id for each trip #########

#a simple method here is giving each trip a different number (just cumsum the trip_begin var), and then using that as the trip id.
cevp$trip_id = cumsum(cevp$trip_begin)


########### counting the number of vehicles in operation at any given time #############

# obs_ind = 283
#given an ind of interest, the cevp data, and the threshold for time, returns the number of fire, police, and total emergency vehicles out at any given time beyond the vehicle of interest.
count_num_vehicles = function(obs_ind, cevp, NEW_TRIP_THRESHOLD){
  veh_inds = which(cevp$dt < cevp$dt[obs_ind] + NEW_TRIP_THRESHOLD & cevp$dt > cevp$dt[obs_ind] - NEW_TRIP_THRESHOLD)
  # time_diffs = abs(cevp$dt - cevp$dt[obs_ind])
  veh = as.character(unique(cevp$veh_id[veh_inds]))
  veh = veh[which(!(veh %in% cevp$veh_id[obs_ind]))]
  num_fire = length(grep('F', veh))
  num_police = length(grep('P', veh))
  num_veh = length(veh)
  return(c(num_veh, num_police, num_fire))
}
cevp$num_veh = 0
cevp$num_police = 0
cevp$num_fire = 0
for(n in seq_len(nrow(cevp))){
  num_veh_all = count_num_vehicles(n, cevp, NEW_TRIP_THRESHOLD)
  cevp$num_veh[n] = num_veh_all[1]
  cevp$num_police[n] = num_veh_all[2]
  cevp$num_fire[n] = num_veh_all[3]
}

rm(num_veh_all)



########## Marking the CEVP signal intersections - cevp_ints_buffer_spdf ################ 

cevp_ints = readOGR('CEVP_Intersections', layer = 'CEVP_Intersections') #this seems to be the easiest and best way

cevp_ints = spTransform(cevp_ints, PROJ_4_STRING)


cevp_ints_buffer = raster::buffer(cevp_ints, width = COUNTY_INTERSECTION_BUFFER, dissolve = FALSE)

#making a spdf from the cevp_ints_buffer
cevp_ints_buffer_polys = cevp_ints_buffer@polygons
cevp_ints_buffer_spolys = SpatialPolygons(cevp_ints_buffer_polys, proj4string = PROJ_4_STRING)
cevp_ints_buffer_spdf = SpatialPolygonsDataFrame(cevp_ints_buffer_spolys, data = data.frame(id = seq_len(length(cevp_ints_buffer_polys))))

rm(cevp_ints, cevp_ints_buffer, cevp_ints_buffer_polys, cevp_ints_buffer_spolys)


########## IRRELEVANT Marking the non-CEVP signal intersections - non_cevp_ints_buffer_spdf IRRELEVANT ##########
non_cevp_ints = readOGR('EVPLayer/doc.kml', layer = 'Signals with No EVP')

non_cevp_ints = spTransform(non_cevp_ints, PROJ_4_STRING)


non_cevp_ints_buffer = raster::buffer(non_cevp_ints, width = COUNTY_INTERSECTION_BUFFER, dissolve = FALSE)

#making a spdf from the non_cevp_ints_buffer
non_cevp_ints_buffer_polys = non_cevp_ints_buffer@polygons
non_cevp_ints_buffer_spolys = SpatialPolygons(non_cevp_ints_buffer_polys, proj4string = PROJ_4_STRING)
non_cevp_ints_buffer_spdf = SpatialPolygonsDataFrame(non_cevp_ints_buffer_spolys, data = data.frame(id = seq_len(length(non_cevp_ints_buffer_polys))))

rm(non_cevp_ints, non_cevp_ints_buffer, non_cevp_ints_buffer_polys, non_cevp_ints_buffer_spolys)



########### aggregating each trip into a single observation - cevp_agg ###############

# trip = cevp[cevp$trip_id == 3,]
#given the data for a single trip, aggregates all of the data of a trip into a single row. Can be edited to make different kinds of data
get_agg_cevp = function(trip, non_sj_ints_buffer_spdf, PROJ_4_STRING, roads_with_speed_limits,
                        TRIP_POINTS_BUFFER, #intersections_buffer_spdf,
                        cevp_ints_buffer_spdf){
  
  #creating a spatial version of the trip. Will come in handy here and later on. 
  trip_spdf = sp::SpatialPointsDataFrame(coords = data.frame(lng = trip$lon, lat = trip$lat),
                                         data = trip, proj4string = PROJ_4_STRING)
  
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
  ret_dat = data.frame(trip_id = trip$trip_id[1], company = trip$company[1], veh_id = trip$veh_id[1], veh_type = trip$veh_type[1], inc_desc = trip$inc_desc[1], 
                       start_lon = trip$lon[1], start_lat = trip$lat[1], end_lon = trip$lon[last_ind], end_lat = trip$lat[last_ind],
                       avg_speed = weighted.mean(trip$speed, trip$time_diff, na.rm = TRUE), start_time = trip$dt[1], end_time = trip$dt[last_ind], travel_time = sum(trip$time_diff, na.rm = TRUE),
                       num_veh = weighted.mean(trip$num_veh, trip$time_diff), 
                       num_police = weighted.mean(trip$num_police, trip$time_diff), num_fire = weighted.mean(trip$num_fire, trip$time_diff),
                       avg_speed_limit = weighted.mean(point_speed, trip$time_diff))
  
  # first_lat = trip$lat[-last_ind]
  # last_lat = trip$lat[-1]
  # first_lon = trip$lon[-last_ind]
  # last_lon = trip$lon[-1]
  # lat_dists = abs(first_lat - last_lat)
  # lon_dists = abs(first_lon - last_lon)
  # dists = sqrt(lat_dists^2 + lon_dists^2)
  # full_dist = sum(dists)
  #doing all of the above in a single function for the function to go faster. Please see above for a more human-readable version of this.
  ret_dat$full_dist = ((trip$lat[-last_ind] - trip$lat[-1])^2 + (trip$lon[-last_ind] - trip$lon[-1])^2) %>% sqrt() %>% sum()
  
  #identifying which points are over the buffer points for the non_sj_intersections
  trip_points_over_non_sj_ints = over(trip_spdf, non_sj_ints_buffer_spdf) %>% select(OperatorAgency, IntNo, IntName) %>% filter(!is.na(IntNo))
  num_non_sj_ints = length(unique(trip_points_over_non_sj_ints$IntNo))
  num_county_ints = length(unique(trip_points_over_non_sj_ints$IntNo[grep('County', trip_points_over_non_sj_ints$OperatorAgency)]))
  num_other_non_sj_ints = num_non_sj_ints - num_county_ints
  
  ret_dat$non_sj_ints = num_non_sj_ints
  ret_dat$county_ints = num_county_ints
  ret_dat$other_non_sj_ints = num_other_non_sj_ints
  
  #BUNK counting the total number of signalized intersections the trip goes through. NOT REAL INTERSECTION COUNT. USE CEVP INSTEAD BELOW
  # trip_points_over_ints = over(trip_spdf, intersections_buffer_spdf) %>% filter(!is.na(id)) %>% unique()
  # 
  # ret_dat$sig_ints = nrow(trip_points_over_ints)
  
  
  #counting CEVP intersections the trip goes through. 
  trip_points_over_cevp_ints = cevp_ints_buffer_spdf@data$id[which(!is.na(over(cevp_ints_buffer_spdf, trip_spdf))[,1])] %>% unique()
  
  ret_dat$cevp_ints = length(trip_points_over_cevp_ints)
  
  
  return(ret_dat)
}

install_and_load('magrittr')
cevp_agg = get_agg_cevp(cevp[cevp$trip_id == 1,],  non_sj_ints_buffer_spdf, 
                        PROJ_4_STRING, roads_with_speed_limits, TRIP_POINTS_BUFFER,
                        #intersections_buffer_spdf, 
                        cevp_ints_buffer_spdf)
for(n in 2 : max(cevp$trip_id)){
  cevp_agg = rbind(cevp_agg, get_agg_cevp(cevp[cevp$trip_id == n,],
                                          non_sj_ints_buffer_spdf, PROJ_4_STRING, 
                                          roads_with_speed_limits, TRIP_POINTS_BUFFER,
                                          #intersections_buffer_spdf, 
                                          cevp_ints_buffer_spdf))
}
#it looks like the sig_ints is based on something foolish. There is around 900 signalized intersections (according to DoT), but intersections_buffer_spdf only has 697 rows.
#meanwhile, cevp_ints_buffer_spdf has 902 rows, which sounds more accurate. Since all SJ signalized interesections have CEVP, we can just drop the intersections_buffer_spdf and the sig_ints data.


########## Setting up the data for analysis - study_dat #############

study_dat = cevp_agg %>% dplyr::select(travel_time, num_police, num_fire, 
                                       num_veh, full_dist, start_time, end_time,
                                       county_ints, sig_ints, non_sj_ints,
                                       avg_speed_limit, avg_speed) #so it looks like the avg_speed variable is bunk, which is fine. we can do without it.


########## For testing - trip, trip_spdf, map, trip_map, and intersection_map ##########

trip = cevp[cevp$trip_id == 5,]

trip_spdf = sp::SpatialPointsDataFrame(coords = data.frame(lng = trip$lon, lat = trip$lat),
                                       data = trip, proj4string = PROJ_4_STRING)

map <- leaflet() %>%
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = -121.88, lat = 37.32, zoom = 12)

trip_map <- map %>% addCircleMarkers(data = trip_spdf) %>% addCircleMarkers(data = trip_spdf[1,], color = '#f4426e', opacity = 1, fillOpacity = 0.8) %>% 
  addMarkers(data = trip_spdf[nrow(trip_spdf@data),])


intersection_map <- map %>% addPolygons(data = intersections_buffer_spdf) %>% addCircleMarkers(data = trip_spdf, color = '#f4426e')#interestingly, there are two intersection dots in some intersections, and only one dot in other intersections. I think it has to do with the number of lights present at the intersection, but regardless, we need to make sure only one interesection is counted in each spot.
#this can be accomplished by focusing on the trip dots. Whenever a trip dot is within one or more intersection dots, it only counts it as one intersection passed through. 


########### COMMENTED OUT Studying the County Roads Data #######

#let's just look at the intersections and see how far apart they are from one another.
#first we can do this by plotting the points of a single trip onto a leaflet map. 
#This way we can see how much travel space is between each point, to see what sort of leeway 
#we need to give to most accurately identify how many county roads fire passed through. 


# cevp_spdf = sp::SpatialPointsDataFrame(coords = data.frame(lng = cevp$lon, lat = cevp$lat),
#                                        data = cevp, proj4string = PROJ_4_STRING)
# 
# #overlaying these points onto a leaflet map
# install_and_load('leaflet')
# #starter map
# 
# #overall the points seem pretty close, so it's possible we can just see if any county intersection points are x meters away.
# #Let's see how they compare to county intersections. 
# test_map <- map %>% addCircleMarkers(data = trip_spdf, weight = .5, radius = 5)
# 
# 
# #loading in the county intersection point data
# #sheet name: OtherAgencySignals
# install_and_load('readxl')
# non_sj_intersections = readxl::read_excel('Non-City signals.xlsx', sheet = 'OtherAgencySignals')
# non_sj_ints_spdf = sp::SpatialPointsDataFrame(coords = data.frame(lng = non_sj_intersections$Y,
#                                                                   lat = non_sj_intersections$X),
#                                               data = non_sj_intersections,
#                                               proj4string = PROJ_4_STRING)
# 
# #adding the non-sj intersections to the map
# #overall it looks like the point method should work pretty well actually. We just need to identify the buffer.
# full_test_map <- test_map %>% addCircleMarkers(data = non_sj_ints_spdf, color = '#f4426e')
# 
# #It looks like a buffer of 33 meters should do the trick. 
# non_sj_ints_buffer = raster::buffer(non_sj_ints_spdf, width = COUNTY_INTERSECTION_BUFFER, dissolve = FALSE)
# buffer_test_map <- test_map %>% addPolygons(data = non_sj_ints_buffer, color = '#f4426e', stroke = TRUE)
# 
# #making a spdf from the non_sj_ints_buffer
# non_sj_ints_buffer_polys = non_sj_ints_buffer@polygons
# non_sj_ints_buffer_spolys = SpatialPolygons(non_sj_ints_buffer_polys, proj4string = PROJ_4_STRING)
# non_sj_ints_buffer_spdf = SpatialPolygonsDataFrame(non_sj_ints_buffer_spolys, data = data.frame(non_sj_ints_spdf@data))
# 
# #identifying which points are over the buffer points
# trip_points_over_non_sj_ints = over(trip_spdf, non_sj_ints_buffer_spdf) %>% select(OperatorAgency, IntNo, IntName) %>% filter(!is.na(IntNo))
# num_non_sj_ints = length(unique(trip_points_over_non_sj_ints$IntNo))
# num_county_ints = length(unique(trip_points_over_non_sj_ints$IntNo[grep('County', trip_points_over_non_sj_ints$OperatorAgency)]))




