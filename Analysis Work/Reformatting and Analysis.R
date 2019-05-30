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

####### Loading in Data #######

cevp_agg_list = readRDS('CAD data/cevp_agg_list_483_of_483_done.rds')

for(n in cevp_agg_list){
  n[,1] = as.character(n[,1])
  n[,2] = as.character(n[,2])
  # n[,3] = as.numeric(n[,3])
  # n[,4] = as.numeric(n[,4])
  # n[,5] = as.numeric(n[,5])
  # n[,6] = as.numeric(n[,6])
  # n[,7] = as.character(n[,7])
}

code_3 = read.table(file = 'CAD data/CODE 3 ids.txt', sep = '|', header = TRUE, stringsAsFactors = FALSE)
code_3$sdts = as.POSIXct(code_3$sdts)


####### CONSTANTS ########

PROJ_4_STRING = CRS('+init=epsg:4326')
BEFORE_CEVP_LIMIT = as.POSIXct('2018-03-01')
AFTER_FULL_CEVP = as.POSIXct('2019-03-01')
CAD_FILE_DIRECTORY = 'CAD data/Trip Files'

#Getting list of all files
files = list.files(CAD_FILE_DIRECTORY)



###### Putting list into a single data frame #####

#first we need to identify how big to make the data frame
num_rows = 0
for(n in cevp_agg_list){
  num_rows = num_rows + nrow(n)
}
num_cols = ncol(cevp_agg_list[[1]])

c_dat = data.frame(array(dim = c(num_rows, num_cols)))
colnames(c_dat) = colnames(cevp_agg_list[[1]])


current_row = 1
for(n in cevp_agg_list){
  for(m in 1:ncol(n)){
    c_dat[current_row:(nrow(n) + current_row - 1),m] = as.character(n[,m])
  }
  current_row = current_row + nrow(n)
}

#currently every column is in character format to maintain formatting. It is now time to convert all of them to 
#their rightful column types

#these stay as characters
for(n in c(1, 2)){
  c_dat[,n] = as.character(c_dat[,n])
}

#these become numeric
for(n in c(3:6, 9:ncol(c_dat))){
  c_dat[,n] = as.numeric(c_dat[,n])
}


#these become date_time (posixct)
for(n in c(7, 8)){
  c_dat[,n] = as.POSIXct(c_dat[,n])
}

#all works. Good for analysis. 



######## Adding a factored var which marks if the trip was before CEVP, during implementation, or after full implementation #########

c_dat$cevp_status = "before"
c_dat$cevp_status[c_dat$start_time >= BEFORE_CEVP_LIMIT & c_dat$start_time <= AFTER_FULL_CEVP] = "during"
c_dat$cevp_status[c_dat$start_time > AFTER_FULL_CEVP] = "after"

c_dat$cevp_status = as.factor(c_dat$cevp_status)


######## Marking which trips were code 3 and keeping only those rows - c_dat ############

#incident ids from the c_dat full_id var
inc_ids = gsub('^[[:alnum:]]+_', '', c_dat$full_id)
#making sure that the code_3 ids are in the inc_ids... they aren't. Let's check the ones that aren't in there.
print(nrow(code_3))
print(length(which(code_3$ï..incident_no %in% inc_ids)))

#ones that don't match any inc_ids
print(sample_dat(code_3[which(!(code_3$ï..incident_no %in% inc_ids)),]))
#ones that do match an inc_id
print(sample_dat(code_3[which((code_3$ï..incident_no %in% inc_ids)),]))
#there doesn't seem to be any major difference. It's possible some part of this just got removed or something. For all intents and purposes though, let's just look at the code 3s that we know exist. 

#alternatively, let's see what matching there is with the start dts. 
length(which(code_3$sdts %in% c_dat$start_time))#the answer is almost none... why are things so hard. 

#so let's just, for the time being use the code_3 ids that seem to match. and see what results we get. 


c_dat$code = 2
c_dat$code[inc_ids %in% code_3$ï..incident_no] = 3

# c_dat = c_dat[c_dat$code == 3,]

######## Adding a real_travel_time variable from the CAD data Jose provided us - c_dat$real_travel_time ##########
acc_trav_time = read.table('CAD data/CAD Data with On Scene timestamp.txt', sep = '|', header = TRUE, stringsAsFactors = FALSE)

#checking to see any  null entries in the start time and the end time
# table(acc_trav_time$SDTS)[order(-table(acc_trav_time$SDTS))][1:10]
# table(acc_trav_time$Ar)[order(-table(acc_trav_time$Ar))][1:10]
# length(which(acc_trav_time$Ar == 'NULL')) #a lot of NULL  arrival times.
# length(which(acc_trav_time$Ar != 'NULL')) #but still plenty non-NULL arrival times.
# length(which(acc_trav_time$SDTS == 'NULL')) #no NULL start times. 

#removing NULL entries
acc_trav_time = acc_trav_time[acc_trav_time$Ar != 'NULL',]


#creating a dataframe of just the variables I need
att = data.frame(full_id = paste0(acc_trav_time$UNID, '_', acc_trav_time$incident_no), 
                 start_dt = as.POSIXct(acc_trav_time$SDTS, format = '%m/%d/%Y %H:%M:%OS'), end_dt = as.POSIXct(acc_trav_time$Ar, format = '%m/%d/%Y %H:%M:%OS'),
                 stringsAsFactors = FALSE)

#calculating real travel time
att$travel_time = att$end_dt - att$start_dt
att$num_travel_time = as.numeric(att$travel_time) #converting it to numeric (in seconds)

#adding the real_travel_time var to c_dat
#I'm not saying this is the most efficient way to do this, but I know this way will work, and it shouldn't take too long.
#I will order both the att and c_dat by full_id.
#then I will add the vars "real_end_dt" and "real_travel_time" to c_dat
#Then I will go down each row of c_dat, check to see if a full_id matches in the att dataset.
#If yes, then I will add that matching att row's end_dt and num_travel_time vars as that c_dat row's real_end_dt and real_travel_time vars, respectively.

#to dang slow, not gonna bother. 
# c_dat = c_dat[order(c_dat$full_id),]
# att = att[order(att$full_id),]
# c_dat$real_end_dt = NA
# c_dat$real_travel_time = NA
# for(n in seq_len(nrow(c_dat))){
#   match_row = att[att$full_id == c_dat$full_id[n],]
#   if(nrow(match_row) > 0){
#     c_dat$real_end_dt[n] = match_row$end_dt
#     c_dat$real_travel_time[n] = match_row$num_travel_time
#   }
#   if(n %% 2000 == 0){print(paste0(n, ' of ', nrow(c_dat), ' done'))}
# }

#okay going to see if I can do the above much, much faster - we can. 
c_dat = c_dat[which(c_dat$full_id %in% att$full_id),]
att_match = att[which(att$full_id %in% c_dat$full_id),]
c_dat = c_dat[order(c_dat$full_id),]
att_match = att_match[order(att_match$full_id),]
print(identical(att_match$full_id, c_dat$full_id))#works
c_dat$real_start_dt = att_match$start_dt
c_dat$real_end_dt = att_match$end_dt
c_dat$real_travel_time = att_match$num_travel_time



summary(c_dat)
#so, interestingly there are some travel times that are negative. let's take a look at those
print(c_dat[c_dat$real_travel_time < 0,])
#there are three observations which this is the case. They all happened on the same day around the same time and the trips ended within 15 seconds of each other.
#In short, it seems like something just happened on accident here (some human error). In these cases, the best thing to do would be to remove them, since it is only three observations.
c_dat_warts_and_all = c_dat
c_dat = c_dat[c_dat$real_travel_time > 0,]

summary(c_dat) #interestingly, the real travel times are now higher than the travel times (in mean and median). huh. That being said, the max travel_time is larger than the max real_travel_time.
#not worth questioning because it's just going to cause a rukus. let's keep moving here. 


######applying fixes to data in a separate version of the data - c_fix ########

c_fix = c_dat
#The last_time_diff is often massive (500 seconds or more). This is likely due to a reporting error.
#Since the fire officers have to physically stop the timer once they arrive (sometimes? Idk), the timer is at risk to keep running.
#This max time step will be subtracted from all observations' travel time. 
c_fix$travel_time = c_dat$travel_time - c_dat$last_time_diff


#it seems there are some observations that have missing vars. Let's see which those are
focus_cols = c('travel_time', 'full_dist', 'non_sj_ints', 'avg_speed_limit',
               'num_veh', 'cevp_status', 'cevp_ints')
#yeah okay. Specifically there are 2566 obs that are missing both the avg_speed_limit var and the num_veh var.
#let's take a look at some of them.
na_inds = which(is.na(c_fix$avg_speed_limit))
sample_dat(c_fix[na_inds,])
#so from the looks of it, these observations have no travel time. No nothing. They are just stationary obs.
#we can remove them.

c_fix = c_fix[!is.na(c_fix$num_veh) & !is.na(c_fix$avg_speed_limit),]


#there is at least one observation with a ridiculous average time diff and max time diff. We should explore those ob(s) and remove it/them
# print(quantile(c_fix$travel_time, seq(.95, 1.00, length.out = 20)))#there seems to be only one major outlier here
# print(quantile(c_fix$travel_time, seq(.99, 1.00, length.out = 50)))#there seems to be only one major outlier here
# print(quantile(c_fix$travel_time, seq(.999, 1.00, length.out = 50)))#unclear if it is a single outlier.
# 
# 
# #let's do a general density plot
# plot(density(c_fix$travel_time))#it looks like there is nothing relevant past 1500
# 
# #let's do a density plot of all the observations that are above 1000 seconds.
# plot(density(c_fix$travel_time[c_fix$travel_time > 1000]))#after 4000 there is basically nothing
# 
# #let's do a density plot of all the observations that are above 4000 seconds.
# plot(density(c_fix$travel_time[c_fix$travel_time > 4000]))#there are only 36 observations with more than 4000 seconds. this is over an hour travel time. For all intents and purposes let's just remove these ones.



############ Basic analysis #########
mean(c_dat$travel_time[c_dat$start_time < BEFORE_CEVP_LIMIT])
mean(c_dat$travel_time[c_dat$start_time > AFTER_FULL_CEVP])
#right off the bat we see nothing. good start good start. 

#but now we can check the real travel time
mean(c_dat$real_travel_time[c_dat$start_time < BEFORE_CEVP_LIMIT])
mean(c_dat$real_travel_time[c_dat$start_time > AFTER_FULL_CEVP])
#so yeah there is a substantial drop of 53 seconds. 


print(t.test(c_dat$travel_time[c_fix$start_time < BEFORE_CEVP_LIMIT],
             c_dat$travel_time[c_fix$start_time > AFTER_FULL_CEVP]))



mean(c_fix$travel_time[c_fix$start_time < BEFORE_CEVP_LIMIT])
mean(c_fix$travel_time[c_fix$start_time > AFTER_FULL_CEVP])

#let's see if the last_time_diff is different between the two. 
print(t.test(c_fix$travel_time[c_fix$start_time < BEFORE_CEVP_LIMIT],
             c_fix$travel_time[c_fix$start_time > AFTER_FULL_CEVP]))
#yes. Substantial and significant. The difference is about 17 seconds, where last_time_diff after cevp is 17 seconds more on average than before. So yeah, fixing the data did have the impact we expected.  


#the overall time averages are basically the same. Interestingly, when fixing the travel time, we see that tt (travel_time) is less
#nowadays than it was back in the day.


###### Regression analysis ##########

#NOTE: c_fix is c_dat now that we focus on real_travel_time rather than travel_time. So there is no reason to not just use
#c_fix, which has the added benefit of removing the obs that are going to be removed because of missingness anyways.
#the main test here is to include the travel distance, the avg speed limit, the number of CEVP intersections:cevp_status
main.lm = lm(real_travel_time ~ full_dist + non_sj_ints + avg_speed_limit + num_veh + cevp_status:cevp_ints, 
             data = c_fix[c_fix$code==3,])
main_fix.lm = lm(real_travel_time ~ full_dist + cevp_status:non_sj_ints + avg_speed_limit + num_veh + cevp_status:cevp_ints, 
                 data = c_fix[c_fix$code==3,])
summary(main.lm)
summary(main_fix.lm)


#so you can break it down by county intersections, but it's not worth it. There is likely some seasonality issues going on here, but who knows. 
#for now let's just show standard regression 1, and removing outliers standard regression. No combo var for non_sj_ints.
install_and_load('car')
print(vif(main.lm))

#c_fix now is the same as c_dat. so instead we are going to make a print out of main.lm and main_fix_no_outliers.lm below.
#actually. Let's just instead print out two regressions. One which breaks non_sj_ints down by cevp_status and one that does not. 

#saving them to stargazer
install_and_load('stargazer')
stargazer(main.lm, main_fix.lm)




#let's see if any observations have a high outlier impact. 
#For this we will use Cook's distance metric
cooksd = cooks.distance(main.lm)
#creds to Selva Prabhakaran for this code on plotting cooks distance - http://r-statistics.co/Outlier-Treatment-With-R.html
# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#there is one observation that so substantially influences the results comapred to any other observation.
#let's just remove that observation. 


outliers = cooksd[which(cooksd >= 4*mean(cooksd, na.rm=T))]
print(length(outliers)) #68 outliers.

#let's take a look at the outlier
c_outliers = c_fix[which(cooksd >= 4*mean(cooksd, na.rm=T)),]
#it has some absurd distance traveled (473 kilometers). Let's just remove it.


#for this regression, we will remove the standard outliers based on the rule of an outlier having a cooks distance four times greather than the mean cooks distance.
main_fix_no_outliers.lm = lm(real_travel_time ~ full_dist + non_sj_ints + avg_speed_limit + num_veh + cevp_status:cevp_ints,
                             data = c_fix[which(cooksd < 4*mean(cooksd, na.rm=T) & c_fix$code == 3),])

summary(main_fix_no_outliers.lm)
cooksd_no_outliers = cooks.distance(main_fix_no_outliers.lm)
#much better.
# plot(cooksd_no_outliers, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance

#all told this gives us the result we would expect. 
#non_sj_ints have about the same impact on time as did cevp_ints before implementation.
#during implementation we see a slight drop in the time each intersection takes
#after implementation we see a larger drop in the time each intersection takes.
#good good. well done.

#in numbers, we see cevp intersections reduce travel time by roughly 3 seconds for each CEVP intersection that is passed through.


#saving them to stargazer
install_and_load('stargazer')
stargazer(main.lm, main_fix_no_outliers.lm)




#another complaint could be some seasonality thing that is going on. 
#Unfortunately we cannot check this with just the CAD-verified code 3 data. We 
#would need cad-verified code 2 data, so this is bunk for the time being. 
# #To check this we can see if the
# #trucks that were code 2 (so CEVP doesn't go off) saw a similar drop in time.
# main_2.lm = lm(real_travel_time ~ full_dist + non_sj_ints + avg_speed_limit + num_veh + cevp_status:cevp_ints, 
#                data = c_dat[c_dat$code==2,])
# main_2_fix.lm = lm(travel_time ~ full_dist + non_sj_ints + avg_speed_limit + num_veh + cevp_status:cevp_ints, 
#                    data = c_fix[c_fix$code==2,])
# summary(main_2.lm)
# summary(main_2_fix.lm)
# summary(main_fix.lm)

#while we do see a drop for code 2 trucks as well, it is not as much as the drop for code 3 trucks. We see a 2-second additional drop for code 3 trucks.

######### understanding the data a bit more ##########




#we want to be able to explain why we see a result in the regression, but not in the raw numbers. 

#seeing the average number of intersections a trip passes through
print(summary(c_fix$cevp_ints + c_fix$non_sj_ints, na.rm = TRUE))#the average trip passes through 4 intersections

#seeing number of intersections before cevp and after
print(summary(rowSums(c_fix[c_fix$start_time < BEFORE_CEVP_LIMIT,c('cevp_ints', 'non_sj_ints')])))
print(summary(rowSums(c_fix[c_fix$start_time > AFTER_FULL_CEVP,c('cevp_ints', 'non_sj_ints')])))

#the difference is significant
print(t.test(rowSums(c_fix[c_fix$start_time < BEFORE_CEVP_LIMIT,c('cevp_ints', 'non_sj_ints')]),
             rowSums(c_fix[c_fix$start_time > AFTER_FULL_CEVP,c('cevp_ints', 'non_sj_ints')])))

#So interestingly, trucks seem to have passed through more intersections on average (half an intersection more)
#after CEVP than before. 


#Let's see if there is any difference in distance traveled before and after cevp - nope.
print(summary(c_fix$full_dist[c_fix$start_time < BEFORE_CEVP_LIMIT]))
print(summary(c_fix$full_dist[c_fix$start_time > AFTER_FULL_CEVP]))

dist_before_cevp = c_fix$full_dist[c_fix$start_time < BEFORE_CEVP_LIMIT]
dist_after_cevp = c_fix$full_dist[c_fix$start_time > AFTER_FULL_CEVP]

print(t.test(dist_before_cevp, dist_after_cevp))#no significant difference between the two

#let's see if the number of non_sj_ints changes before and after cevp
print(summary(c_fix$non_sj_ints[c_fix$start_time < BEFORE_CEVP_LIMIT]))
print(summary(c_fix$non_sj_ints[c_fix$start_time > AFTER_FULL_CEVP]))
#small but significant (but small) difference between the two
print(t.test(c_fix$non_sj_ints[c_fix$start_time < BEFORE_CEVP_LIMIT],
             c_fix$non_sj_ints[c_fix$start_time > AFTER_FULL_CEVP]))


#let's see if the last_time_diff is different between the two. 
print(t.test(c_fix$last_time_diff[c_fix$start_time < BEFORE_CEVP_LIMIT],
             c_fix$last_time_diff[c_fix$start_time > AFTER_FULL_CEVP]))
#yes. Substantial and significant. The difference is about 17 seconds, where last_time_diff after cevp is 17 seconds more on average than before. So yeah, fixing the data did have the impact we expected.  



#just looking at all the variables now
print(summary(c_fix[c_fix$start_time < BEFORE_CEVP_LIMIT,]))
print(summary(c_fix[c_fix$start_time > AFTER_FULL_CEVP,]))


###### Making some desc stats for the paper ##########

#seeing how many observations were dropped following cleaning step 1) only using obsrevations with validated travel times.
print(num_rows - nrow(c_dat))
print((num_rows - nrow(c_dat))/num_rows)

#seeing how many observations were dropped following cleaning step 2) removing NA observations
print(nrow(c_dat) - nrow(c_fix))

sum_dat = summary(c_fix)

relevant_desc_cols = c('real_travel_time', 'num_veh', 'avg_speed_limit', 'full_dist', 'non_sj_ints', 'cevp_ints')

#grabbing only the relevant columns
sum_dat = sum_dat[,grep(paste0('^[[:space:]]*', relevant_desc_cols, collapse = '|'), colnames(sum_dat))]

#now we have to transpose the summary table.
t_sum_table = array(NA, dim = c(length(relevant_desc_cols), nrow(sum_dat)))
colnames(t_sum_table) = gsub('(^[^\\:]+)(\\:)([[:print:]]+)' , '\\1', sum_dat[,1]) %>% trimws()
rownames(t_sum_table) = relevant_desc_cols

for(n in seq_len(ncol(sum_dat))){
  row = gsub('(^[^\\:]+)(\\:)([[:print:]]+)' , '\\3', sum_dat[,n]) %>% trimws()
  t_sum_table[n,] = row
}
rownames(t_sum_table) = colnames(sum_dat)
stargazer(t_sum_table)

#looking at number of trips before, during, and after CEVP
stargazer(summary(c_fix$cevp_status))



######## Visually seeing all the trips ##########

c_fix = c_fix[c_fix$code == 3,]

#now is time to flex that data story muscle. 
#let's find a common route that includes many intersections and see how the overall travel time 
#to that spot has changed over time. 

#okay first we need to change the data into a spatial lines data frame,
#where each line is two points: the start point then the end point.

#given a row, makes the line for the row
make_line = function(row){
  #row = c_fix[34,]
  line = Line(matrix(data = c(row$start_lon, row$start_lat, row$end_lon, row$end_lat),
                nrow = 2, ncol = 2, byrow = TRUE))
  ret_line = Lines(slinelist = list(line), ID = row$full_id)
  return(ret_line)
}

sp_lines_list = list()
for(n in seq_len(nrow(c_fix))){
  sp_lines_list[[n]] = make_line(c_fix[n,])
}

sp_lines = SpatialLines(sp_lines_list, proj4string = PROJ_4_STRING)

rownames(c_fix) = c_fix$full_id
c_sldf = SpatialLinesDataFrame(sp_lines, data = c_fix)

#now that we have a spatiallinesdataframe, we can plot it over a map of san jose

#base map
map <- leaflet() %>% addTiles() %>%
  # add ocean basemap
  #addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  #addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  # focus map in a certain area / zoom level
  setView(lng = -121.88, lat = 37.32, zoom = 12)


fire_trips_map <- map %>% addPolylines(data = c_sldf[c_sldf$real_travel_time < 240,],
                                       weight = 1, color = '#000000')

#getting san Jose outline to show
install_and_load('tigris')
options(tigris_class = "sf", tigris_use_cache = TRUE)
sj_all <- places("CA", cb = TRUE) %>%
  filter(NAME == "San Jose") %>% as('Spatial') 
sj_poly = Polygons(list(sj_all@polygons[[1]]@Polygons[[3]]), 1)
sj_sp_poly = SpatialPolygons(list(sj_poly), proj4string = PROJ_4_STRING)
sj = sp::SpatialPolygonsDataFrame(sj_sp_poly,data = data.frame(id = seq_along(sj_sp_poly)))
options(tigris_class = "sp", tigris_use_cache = TRUE)


#making this map from scratch like this so that it matches the style of the other map shown in the paper. 
fire_trips_map_aesthetic <- leaflet() %>%
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = -121.88, lat = 37.32, zoom = 12) %>%
  addPolylines(data = c_sldf[sample(1:nrow(c_sldf@data), 5000),],
               weight = 1, color = '#000000') %>% 
  addPolygons(data = sj, fillOpacity = 0, stroke = TRUE, opacity = 1,
              color = '#000000')


fire_trips_4_min_map_aesthetic <- leaflet() %>%
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = -121.88, lat = 37.32, zoom = 12) %>%
  addPolylines(data = c_sldf[c_sldf$real_travel_time < 240,],
               weight = 1, color = '#000000') %>% 
  addPolygons(data = sj, fillOpacity = 0, color = '#000000', opacity = 1)

########### Making a case study ########


#now we need to make a bounding box that gathers the trips in the area that we want. 
install_and_load('geojsonio')
start_box = geojson_read('Case study maps/Start of trip.geojson', what = 'sp') %>% spTransform(PROJ_4_STRING)
end_box = geojson_read('Case study maps/End of trip.geojson', what = 'sp') %>% spTransform(PROJ_4_STRING)

#checking to make sure that the boxes do bound all of the trips
trip_with_box <- fire_trips_map %>% addPolygons(data = start_box) %>% addPolygons(data = end_box)#works

#getting just the case study trips - cs_dat
#marking which have a starting point in the cs firestation
c_start_spdf = SpatialPointsDataFrame(coords = cbind(lon = c_fix$start_lon, lat = c_fix$start_lat), 
                                      data = data.frame(full_id = c_fix$full_id, stringsAsFactors = FALSE), proj4string = PROJ_4_STRING)
c_end_spdf = SpatialPointsDataFrame(coords = cbind(lon = c_fix$end_lon, lat = c_fix$end_lat), 
                                      data = data.frame(full_id = c_fix$full_id, stringsAsFactors = FALSE), proj4string = PROJ_4_STRING)


cs_start_inds = which(!is.na(sp::over(c_start_spdf, start_box)))
cs_end_inds = which(!is.na(sp::over(c_end_spdf, end_box)))

cs_travel_inds = cs_start_inds[which(cs_start_inds %in% cs_end_inds)] #all of the trips that follow this pattern.

cs_trip_map_lines =  map %>% addPolylines(data = c_sldf[cs_travel_inds,],
                                    weight = 1, color = '#000000') #worked.

cs_dat = c_fix[cs_travel_inds,]
#it seems most of them passed through 6 intersections, so let's just keep those. 
#in other words, keep only the trips that passed through the median cevp intersections
cs_fix = cs_dat[cs_dat$cevp_ints == median(cs_dat$cevp_ints),]

#now to see the difference in time....
mean(cs_fix$real_travel_time[cs_fix$cevp_status == 'before'])
mean(cs_fix$real_travel_time[cs_fix$cevp_status == 'after'])
print(mean(cs_fix$real_travel_time[cs_fix$cevp_status == 'before']) - mean(cs_fix$real_travel_time[cs_fix$cevp_status == 'after']))
print(summary(cs_fix))
#IT WORKS!!! TOTAL TIME SAVED OF 107 SECONDS ON AVERAGE. THAT'S A LOT. THAT'S 1min, 47 SECONDS. WOOOOOOOOOOOOO.
#451 trips
#6 cevp intersections
#trips before - 35
#trips after - 58
#distance - around 2.4 kilometers
#average time saved - 107 seconds


#sanity check - did substracting the last time diff unfairly lower the travel time for the after trips? 
#If the 'after' last_time_diff is larger on average than the 'before' then we have some serious problems.
mean(cs_fix$last_time_diff[cs_fix$cevp_status == 'before'])
mean(cs_fix$last_time_diff[cs_fix$cevp_status == 'after'])
#no it did the opposite. So we are good to go. Still sane. Thank goodness.
#distances traveled are also basically the same, so we are fine in that regard too. 
mean(cs_fix$full_dist[cs_fix$cevp_status == 'before'])
mean(cs_fix$full_dist[cs_fix$cevp_status == 'after'])


#now time to get a pic of this journey.
#let's identify a day that this journey occured.
cs_samp = cs_fix[100,]#if everyone is not special, maybe you can be what you want to be.
print(cs_samp)
#this was on June 8th 2018. It has an ID F181599210
#let's pull that file...
CAD_FILE_DIRECTORY = 'CAD data/Trip Files'
file = grep(substr(as.character(cs_samp$start_time), 1, 10), list.files(CAD_FILE_DIRECTORY), value = TRUE)
cs_trip_file = read.csv(paste0(CAD_FILE_DIRECTORY, '/', file), stringsAsFactors = FALSE, header = TRUE) %>% select(-utc, -start_date, -date)

colnames(cs_trip_file) = c('date_time', 'veh_id', 'lat', 'lon', 'status', 'inc_id')

cs_trip_file$full_id = paste0(cs_trip_file$veh_id, '_', cs_trip_file$inc_id)

cs_trip = cs_trip_file[which(cs_trip_file$full_id %in% cs_samp$full_id),]

cs_trip_spdf = SpatialPointsDataFrame(coords = cbind(lon = cs_trip$lon, lat = cs_trip$lat), data = cs_trip)

#I am building this map from scratch like this so that it matches the style of the other map shown in the paper. 
cs_trip_map <- leaflet() %>%
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = -121.88, lat = 37.32, zoom = 12) %>%
  addCircleMarkers(data = cs_trip_spdf) %>% 
  addCircleMarkers(data = cs_trip_spdf[1,], color = '#f4426e', opacity = 1, fillOpacity = 0.8) %>% 
  addMarkers(data = cs_trip_spdf[nrow(cs_trip_spdf@data),])












#while the above is aesthetically pleasing, it doesn't help with figuring out where trucks are often going
#but, based on the above, we see that almost all journeys START at a fire station, and stations seem to mostly 
#go to mutually exclusive destinations. This means that an area which is the end point for a lot of journeys, 
#those journeys with a common ending likley have a common beginning. Let's just look at journey endings for a momnet.
#so turns out the endings just look... weird? It's like they actually are the starting point often times. Idk man. It's weird. The point is, let's focus on the above. 

# c_end_spdf = SpatialPointsDataFrame(coords = cbind(lon = c_fix$end_lon, lat = c_fix$start_lat),
#                                     data = c_fix, proj4string = PROJ_4_STRING)
# 
# 
# fire_end_map <- map %>% addCircleMarkers(data = c_end_spdf[sample(seq_len(nrow(c_end_spdf@data)), size = 50000),],
#                                          stroke = FALSE, weight = 1, radius = 5) 



















########### Doing some stuff for JOSE and Michael ####################
#creating a posix date time var from the date_time var
add_dt = function(cevp){
  date_time_reformat = gsub('([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([[:print:]]*)', 
                            '\\1-\\2-\\3 \\4:\\5:\\6', cevp$date_time)#formats date_time into a machine-readable format to convert into a date_time object
  dt = as.POSIXct(date_time_reformat)
  cevp$dt = dt
  return(cevp)
  
}

#Calculating the time (in seconds) from one record to the next for a vehicle 
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




#let's get 10 data examples for trips with a last_time_diff > 100. We want the full Log for each of these ten, not just thier aggregated data point
#NOTE
#NOTE: THIS REQUIRES PULLING c_fix FROM THE "Reformattting and Analysis.R" code. so run that along with this. 
#NOTE: AND DO NOT RUN THE remove() line at the top.
#NOTE


late_cevps = sample_dat(c_fix[c_fix$last_time_diff > 100 & c_fix$code == 3,])
print(late_cevps)

start_dates = substr(as.character(late_cevps$start_time), 1, 10) #getting the dates for each of these trips so that we can pull the files that they belong to
trip_ids = gsub('^[^_]+_', '', as.character(late_cevps$full_id)) #getting the trip ids so that these can be found within their files
veh_ids = late_cevps$veh_id #getting the veh_id so that these veh_trips can be found within their files
late_cevps_log_list = list() #this is where we will put each log. One list element per log
general_file_name_format = paste0(CAD_FILE_DIRECTORY, '/', 'trips starting ')#this is the common portion of the filenames. Append the date and '.csv' and you will have a full file name

#iterating through each, opening the file, adding in some useful variables using functions above, and putting each log into the list
for(i in seq_along(start_dates)){
  open_cevp_log = read.csv(paste0(general_file_name_format, start_dates[i], '.csv')) %>% select(-utc, -start_date, -date)
  colnames(open_cevp_log) = c('date_time', 'veh_id', 'lat', 'lon', 'status', 'inc_id')
  open_cevp_log$full_id = paste0(open_cevp_log$veh_id, '_', open_cevp_log$inc_id)
  
  late_cevp_log = open_cevp_log[open_cevp_log$veh_id == veh_ids[i] & open_cevp_log$inc_id == trip_ids[i],] %>% add_dt() %>% add_time_diffs()
  write.csv(late_cevp_log, file = paste0('Misfits/CEVP logs with large final time_diff/trip_', late_cevp_log$inc_id[1], '_on_', start_dates[i], '.csv'))
  late_cevps_log_list[[i]] = late_cevp_log %>%  select(-date_time, -full_id)
}



cevp = read.csv(paste0(CAD_FILE_DIRECTORY, '/', files[length(files)]), stringsAsFactors = FALSE, header = TRUE) %>% select(-utc, -start_date, -date)
colnames(cevp) = c('date_time', 'veh_id', 'lat', 'lon', 'status', 'inc_id')
cevp$full_id = paste0(cevp$veh_id, '_', cevp$inc_id)

# wrong_cevp_1 = cevp[cevp$full_id == 'E26_F191199067',] %>% add_dt() %>% add_time_diffs()
# write.csv(wrong_cevp_1, 'clipboard')
wrong_cevp_2 = cevp[cevp$full_id == 'E5_F191199095',] %>% add_dt %>% add_time_diffs()

write.csv(wrong_cevp_2, 'clipboard')
wrong_cevp_2_spdf = sp::SpatialPointsDataFrame(coords = data.frame(lng = wrong_cevp_2$lon, lat = wrong_cevp_2$lat),
                                               data = wrong_cevp_2, proj4string = PROJ_4_STRING)

#getting the x and y coordinates for dist calculations later. note that utm coords are in meters, so distance will also be in meters. yay.

wrong_cevp_2_spdf_xy = spTransform(wrong_cevp_2_spdf, CRS("+proj=utm +zone=10 +datum=WGS84"))
wrong_cevp_2$x_ll = wrong_cevp_2_spdf_xy@coords[,1]
wrong_cevp_2$y_ll = wrong_cevp_2_spdf_xy@coords[,2]
last_ind = nrow(wrong_cevp_2)
xy_dist = ((wrong_cevp_2$y_ll[-last_ind] - wrong_cevp_2$y_ll[-1])^2 + (wrong_cevp_2$x_ll[-last_ind] - wrong_cevp_2$x_ll[-1])^2) %>% sqrt() 
wrong_cevp_2$distance = c(0, xy_dist)

#Okay, so the massive time jumps might actually be due to poor signal while driving. This means that, like in the case above, large time_diffs are also related to large distances traveled. 
#removing the max time diff in the example above would be subtracting the time taken to travel 2km... it's inaccurate. The point is, we have a few options to determine whether or not we should subtract the 
#max time from a trip or not.
#1) instead of removing the max time, we just remove the last time, since that is assumed to be the time that would be them waiting for the truck to be back online. 
#2) see something about distance, if there was a massive time interval with a massive distance, then it makes sense and should not be removed. If it is little to no distance with a massive time interval, then remove it. 



#grabbing the three negative obs here. 
neg_c_dat = c_dat_warts_and_all[c_dat_warts_and_all$real_travel_time < 0,]

start_dates = substr(as.character(neg_c_dat$start_time), 1, 10) #getting the dates for each of these trips so that we can pull the files that they belong to
trip_ids = gsub('^[^_]+_', '', as.character(neg_c_dat$full_id)) #getting the trip ids so that these can be found within their files
veh_ids = neg_c_dat$veh_id #getting the veh_id so that these veh_trips can be found within their files
neg_c_dat_log_list = list() #this is where we will put each log. One list element per log
general_file_name_format = paste0(CAD_FILE_DIRECTORY, '/', 'trips starting ')#this is the common portion of the filenames. Append the date and '.csv' and you will have a full file name
for(i in seq_along(start_dates)){
  open_cevp_log = read.csv(paste0(general_file_name_format, start_dates[i], '.csv')) %>% select(-utc, -start_date, -date)
  colnames(open_cevp_log) = c('date_time', 'veh_id', 'lat', 'lon', 'status', 'inc_id')
  open_cevp_log$full_id = paste0(open_cevp_log$veh_id, '_', open_cevp_log$inc_id)
  
  late_cevp_log = open_cevp_log[open_cevp_log$veh_id == veh_ids[i] & open_cevp_log$inc_id == trip_ids[i],] %>% add_dt() %>% add_time_diffs()
  write.csv(late_cevp_log, file = paste0('Misfits/CEVP logs with negative travel time/trip_', late_cevp_log$inc_id[1], '_on_', start_dates[i], '_number_', i, '.csv'))
  neg_c_dat_log_list[[i]] = late_cevp_log %>%  select(-date_time, -full_id)
}

write.csv(neg_c_dat, file = 'clipboard')



#grabbing a couple of the nonexistent trips here.  
non_exist_dat = sample_dat(c_dat_warts_and_all[is.na(c_dat_warts_and_all$num_veh),])

start_dates = substr(as.character(non_exist_dat$start_time), 1, 10) #getting the dates for each of these trips so that we can pull the files that they belong to
trip_ids = gsub('^[^_]+_', '', as.character(non_exist_dat$full_id)) #getting the trip ids so that these can be found within their files
veh_ids = non_exist_dat$veh_id #getting the veh_id so that these veh_trips can be found within their files
non_exist_dat_log_list = list() #this is where we will put each log. One list element per log
general_file_name_format = paste0(CAD_FILE_DIRECTORY, '/', 'trips starting ')#this is the common portion of the filenames. Append the date and '.csv' and you will have a full file name
for(i in seq_along(start_dates)){
  open_cevp_log = read.csv(paste0(general_file_name_format, start_dates[i], '.csv')) %>% select(-utc, -start_date, -date)
  colnames(open_cevp_log) = c('date_time', 'veh_id', 'lat', 'lon', 'status', 'inc_id')
  open_cevp_log$full_id = paste0(open_cevp_log$veh_id, '_', open_cevp_log$inc_id)
  
  late_cevp_log = open_cevp_log[open_cevp_log$veh_id == veh_ids[i] & open_cevp_log$inc_id == trip_ids[i],] %>% add_dt() %>% add_time_diffs()
  write.csv(late_cevp_log, file = paste0('Misfits/Trips with no AVL Log data/trip_', late_cevp_log$inc_id[1], '_on_', start_dates[i], '_number_', i, '.csv'))
  non_exist_dat_log_list[[i]] = late_cevp_log %>%  select(-date_time, -full_id)
}

write.csv(non_exist_dat, file = 'clipboard')






