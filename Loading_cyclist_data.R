#geting all data using paste and assign command

#tidyverse, readxl, janitor, ggplot2, dplyr


loc_var_1 = 'C:/Users/risha/Documents/Cyclist dataset/'

loc_var_2 = c('Apr','May','June','July','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar')

loc_var_3 = c(rep('_2020/',9),rep('_2021/',3))

loc_var_4 = c(rep('20200',6),rep('2020',3),rep('20210',3))

loc_var_5 = c(4:12,1:3)

loc_var_6 = '-divvy-tripdata.xlsx'



#for loop to read all data at once and assign to a_1,a_2 ... a_12
a=0
for (i in 1:12){
  a <- read_xlsx(paste(loc_var_1,loc_var_2[i],loc_var_3[i],loc_var_4[i],loc_var_5[i],loc_var_6,sep=""))
  assign(paste0('a_',i),a)
}



#vertical join so all months are in one data_frame
table <- rbind(a_1,a_2,a_3,a_4,a_5,a_6,a_7,a_8,a_9,a_10,a_11,a_12)



#checking all data is in the table
sum1=0

sum1 = nrow(a_1)+nrow(a_2)+nrow(a_3)+nrow(a_4)+nrow(a_5)+nrow(a_6)+nrow(a_7)+nrow(a_8)+nrow(a_9)+nrow(a_10)+nrow(a_12)+nrow(a_11)



#separating the started_at and ended_at to date and time 
trimmed_table <- separate(table, started_at , into =c('day', 'start_time'), sep = " ")

trimmed_table <- separate(trimmed_table, day , into =c('start_year','start_month', 'start_day'), sep = "-", remove = FALSE   )

trimmed_table <- separate(trimmed_table, ended_at , into =c('end_day','end_time'), sep = " " )

trimmed_table <- separate(trimmed_table, end_day , into =c('end_year','end_month', 'end_day'), sep = "-" , remove = FALSE  )

#removing the rows with NA values in the start_time and end_time
trimmed_table <- trimmed_table[!is.na(trimmed_table$start_time),]

trimmed_table <- trimmed_table[!is.na(trimmed_table$end_time),]



trimmed_table <- trimmed_table %>% 
  mutate(end_year = as.integer(end_year), end_month = as.integer(end_month) ,end_day = as.integer(end_day),
         start_year = as.integer(start_year), start_month = as.integer(start_month) ,start_day = as.integer(start_day))

trimmed_table <-  unite(trimmed_table, 'start_date', start_year, start_month , start_day, sep = "-",  remove = FALSE)

trimmed_table <-  unite(trimmed_table, 'end_date', end_year, end_month , end_day, sep = "-",  remove = FALSE)

trimmed_table = select(trimmed_table, -3)

#names(trimmed_table)[names(trimmed_table) == "day"] <- "start_date"

#adding day of the year from 1 : 365
df1 = data.frame( month = c(rep(04,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),
                            rep(10,31),rep(11,30),rep(12,31),rep(1,31),rep(2,28),rep(3,31)),
                  day = c(1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31,1:31,1:28,1:31),
                  year= c(rep(2020,365-90),rep(2021,90)),
                  year_day = (c(1:365)) )

df1 <-  unite(df1, 'start_date', year, month , day, sep = "-",  remove = FALSE)

df1 <- df1 %>% 
  mutate(start_date=start_date) %>% 
  mutate(end_date=start_date)

year_start_day <- df1 %>% 
  select(year_day,start_date)

names(year_start_day)[names(year_start_day) == "year_day"] <- "start_year_day"

year_end_day <- df1 %>% 
  select( year_day,end_date)

names(year_end_day)[names(year_end_day) == "year_day"] <- "end_year_day"

trimmed_table <- merge(x = trimmed_table, y = year_start_day, by = "start_date", all = TRUE, sort = FALSE)

trimmed_table <- merge(x = trimmed_table, y = year_end_day, by = "end_date", all = TRUE, sort = FALSE)

trimmed_table <- trimmed_table[!is.na(trimmed_table$ride_id),]

trimmed_table <- trimmed_table[!is.na(trimmed_table$end_year_day),]

# creating a table to separate time into hr, min and secs for start_time and end_time

tri_tab_sorted <- trimmed_table %>%
  arrange(start_year_day)

duration_table <- tri_tab_sorted %>% 
  select(ride_id,start_month, start_year,start_time,start_year_day, end_year, end_month , end_day, end_time, end_year_day) %>% 
  separate(start_time, into =c('s_hr','s_min','s_sec', sep =":")) %>% 
  separate(end_time, into =c('e_hr','e_min','e_sec', sep =":")) 

sum(is.na(duration_table$s_hr))


#since we removed NA values in the trim table we don't need to do this again.
#duration_table <- duration_table[!is.na(duration_table$e_hr),]

#duration_table <- duration_table[!is.na(duration_table$s_hr),]



#changing 00 hrs (12 am) to 24 for calculating ride duration
#duration_table$e_hr[duration_table$e_hr == 0 ] <-24



#changing all time to INT for calculations
duration_table <- duration_table %>% 
  mutate(s_hr = as.integer(s_hr),s_min = as.integer(s_min),s_sec = as.integer(s_sec),
         e_hr = as.integer(e_hr),e_min = as.integer(e_min),e_sec = as.integer(e_sec))






#Calculating the ride duration and creating a new column in the duration_table
duration_table = duration_table %>% 
  mutate(ride_dur=(24*(end_year_day - start_year_day) + e_hr -s_hr )*60*60 +
                      (e_min-s_min)*60 + (e_sec- s_sec))
                     
                     


#adding the ride duration to trimmed_table
duration_table = duration_table %>% 
  mutate(ride_dur_hr=floor(ride_dur/(60*60))) %>% 
  mutate(ride_dur_min=floor(ride_dur%%(60*60)/60)) %>% 
  mutate(ride_dur_sec=floor(ride_dur%%(60*60)%%60))

duration_table <-  unite(duration_table, 'ride_dur_form', ride_dur_hr , ride_dur_min , ride_dur_sec, sep = ":",  remove = FALSE)

tri_tab_sorted <- tri_tab_sorted %>% 
  mutate(ride_dur = duration_table$ride_dur )


tri_tab_sorted <- tri_tab_sorted %>%
  filter(ride_dur > 0) 

duration_table <- duration_table %>%
  filter(ride_dur > 0) 

#tri_tab_sorted <-tri_tab_sorted  [,!names(tri_tab_sorted) %in% c("end_date", "start_date", "start_year_day","end_year_day")]



##converting date to INT for filtering
#trimmed_table = trimmed_table %>% 
  #mutate(start_year = as.integer(start_year),
        # start_month = as.integer(start_month),
        # start_day = as.integer(start_day))


#converting date to INT for filtering
#tri_tab_sorted <- tri_tab_sorted %>% 
  #mutate(start_year = as.integer(start_year),
        # start_month = as.integer(start_month),
         #start_day = as.integer(start_day))



#Creating a table for duplicate riders
duplicate_ride <- get_dupes(trimmed_table ,ride_id)

df4 <- tri_tab_sorted[sample(nrow(tri_tab_sorted ), 100), ]


#visualising the data
tri_tab_sorted <- trimmed_table %>%
  group_by(member_casual,start_year,start_month,start_day) %>%
  arrange(start_year,start_month,start_day) %>%
  summarise(total_dur = sum(ride_dur_hr))

temp_table <- trimmed_table %>%
  select(start_year,start_month,start_day, ride_dur_hr) %>%
  unite(col='Date', c('start_year','start_month','start_day'), sep=':')

tri_tab_sorted %>% ggplot(aes(x = start_month, y = total_dur)) +
  geom_col(aes(fill = total_dur)) 

tri_tab_sorted %>% ggplot(aes(x = start_month, y = total_dur)) +
  geom_col(aes(fill = total_dur))

new_labels1 = c("April","May","June","July","August","September","October","November",
                "December","January","February","March")

names(new_labels1) <- c(4,5,6,7,8,9,10,11,12,1,2,3)

df3 <- data.frame( start_month = c(4,5,6,7,8,9,10,11,12,1,2,3), month = c("April","May","June","July","August","September","October","November","December","January","February","March"))

tri_tab_sorted <- tri_tab_sorted %>% 
  mutate(start_month =duration_table$start_month)
                                                                                                                                               
tri_tab_sorted<- merge(x = tri_tab_sorted, y = df3 , by = "start_month", all = TRUE, sort = FALSE)

temp1 = select(temp1, -1)

tri_tab_sorted <- temp1

tri_tab_sorted %>% 
  filter(start_month >=1 & start_month <=3 & start_year==2021) %>% 
  #arrange(start_year,start_month,start_day) %>% 
  ggplot(aes(x = start_month, y = ride_dur/1000)) +
  geom_col(aes(fill = member_casual), position = "dodge") +
  scale_fill_manual(values=c("#10313E", 
                             "#54717D")) +
  labs(title = "Winter season ride duration Member's vs Casual riders", x = "Member's vs Casual riders duration",y = "Ride duration in 1000km ") + 
  facet_wrap(~factor(month, c("April","May","June","July","August","September","October","November",
                              "December","January","February","March") ), scales = "free")

tri_tab_sorted %>% 
  #filter(start_month >=1 & start_month <=3 & start_year==2021) %>% 
  #arrange(start_year,start_month,start_day) %>% 
  ggplot(aes(x = fct_relevel(month, "April","May","June","July","August","September","October","November","December","January","February","March"), y = ride_dur/1000)) +
  geom_col(aes(fill = member_casual)) +
  scale_fill_manual(values=c("darkgreen", "#54717D")) +
  labs(title = "Monthly ride duration Member's vs Casual riders from April 2020 to March 2021", x = "Member's vs Casual riders duration",y = "Ride duration in 1000km ")


temp_1 <- tri_tab_sorted

temp_1$month <- factor(temp_1$month,
                       levels = c("April","May","June","July","August","September","October","November",
                                  "December","January","February","March"))

temp_1 %>% 
  ggplot(aes(x = fct_relevel(month, "April","May","June","July","August","September","October","November","December","January","February","March"), y = ride_dur/1000)) +
  geom_col(aes(fill = month,position = "dodge")) +
  labs(title = "Monthly ride duration Member's vs Casual riders from April 2020 to March 2021", x = "Member's vs Casual riders duration",y = "Ride duration in 1000km ") + 
  facet_wrap(~member_casual)
  

tri_tab_sorted %>% 
  filter(start_month >=4 & start_month <=6 & start_year==2020) %>% 
  ggplot(aes(x = start_month, y = ride_dur/1000)) +
  geom_col(aes(fill = start_month),position = "dodge") +
  facet_wrap(~member_casual)

tri_tab_sorted %>% 
  #filter(start_month >=1 & start_month <=3 & start_year==2021) %>% 
  ggplot(aes(x = start_month, y = total_dur/1000)) +
  geom_col(aes(fill = start_day),position = "dodge",width = 0.5) +
  facet_wrap(~member_casual)
  

temp_table <- trimmed_table %>%
  select(start_year,start_month,start_day, ride_dur_hr)

temp_table = temp_table %>% 
  mutate(start_month=as.integer(start_month),
         start_day=as.integer(start_day))

temp_table %>% 
  filter(start_month ==4 & start_day <=14 & ride_dur_hr <0 ) %>% 
  ggplot(aes(x = start_day, y = ride_dur_hr)) +
  geom_col(aes(fill = ride_dur_hr)) 

temp_table1 <- tri_tab_sorted %>%
  filter(ride_dur <0)


df4 <- tri_tab_sorted[sample(nrow(tri_tab_sorted ), 3), ]
 
df4 %>% 
  leaflet(width = "100%") %>%
  addTiles() %>% 
  addMarkers(lat = ~start_lat,
             lng = ~start_lng,
             popup = df4$member_casual)

us_map <- get_stadiamap(
  bbox = c(left = -100 , bottom = 30 , right = -60 , top = 51 ),
  maptype = "stamen_terrain",
  zoom = 10
)


register_google(key = "AIzaSyBIm3NbMyFRXcriKm5rbMoTt3wxKUUJ3qg")

hdf <- get_map("houston, texas")
ggmap(hdf, extent = "normal")
ggmap(hdf) # extent = "panel", note qmap defaults to extent = "device"
ggmap(hdf, extent = "device")


Mean_dir = 0
Mean_dir <- df4 %>%
  summarise(s_mean_lat = mean(start_lat), s_mean_long = mean(start_lng), e_mean_lat = mean(end_lat,na.rm=TRUE),
            e_mean_long = mean(end_lng,na.rm=TRUE))

df_casual <- tri_tab_sorted %>% 
  filter(ride_dur >= 24*60*60, start_month >=4 & start_month <=6 ,member_casual == "casual",start_lat != end_lat,
         )

df_member <- tri_tab_sorted %>% 
  filter(ride_dur >= 24*60*60, start_month >=4 & start_month <=6 ,member_casual == "member",start_lat != end_lat,
  )

df_member_casual <-  tri_tab_sorted %>% 
  filter(ride_dur >= 24*60*60, start_month >=1 & start_month <=3 ,start_lat != end_lat,
  )

  #filter(start_lat == end_lat)

map_ride_start <- ggmap(get_map(location = c(lat = mean(Mean_dir$s_mean_lat,Mean_dir$e_mean_lat), lon = mean(Mean_dir$s_mean_long,Mean_dir$e_mean_long)),
                      zoom = 10,
                      maptype = "terrain",
                      color = "color")) +
  geom_point(data = df_casual, aes(y = start_lat+0.0001, x= start_lng+0.0001), 
             color = "red", size=1.5) + 
  geom_point(data = df_member, aes(y = end_lat-0.0001, x= end_lng-0.0001),
             color = "blue", size=1.5) +
  labs(title = "Spring Season ride start location for hire duration more than 24 hr for casual riders and members",
       x = "Longitude", y = "Latitude") 
#%>%  
  #addPolylines(lat = c(df4$start_lat, df4$end_lat ), lng = c(df4$start_lng, df4$end_lng))

map_ride_start <- ggmap(get_map(location = c(lat = mean(Mean_dir$s_mean_lat,Mean_dir$e_mean_lat), lon = mean(Mean_dir$s_mean_long,Mean_dir$e_mean_long)),
                                zoom = 9,
                                maptype = "terrain",
                                color = "color")) +
  geom_point(data = tri_tab_sorted, aes(y = start_lat+0.0001, x= start_lng+0.0001,color = member_casual), 
             size=1.5) + 
  labs(title = "Season rides more that 24hrs",
       x = "Longitude", y = "Latitude") +
  annotate("text",x=-87.53,y=41.88,label=paste("Member's count = ",sum(df_member_casual$member_casual=="member")),
           fontface="bold",size=3) +
  annotate("text",x=-87.53,y=41.87,label=paste("Casual User count = ",sum(df_member_casual$member_casual=="casual")),
           fontface="bold",size=3) +
  theme(plot.title = element_text(size=16))

print(map_ride_start) 


start_locations <- tri_tab_sorted %>% 
  distinct(tri_tab_sorted$start_station_id , .keep_all = TRUE) %>% 
  select(start_station_id,start_station_name,start_lat,start_lng,member_casual)

Mean_dir = 0
Mean_dir <- start_locations %>%
  summarise(s_mean_lat = mean(start_lat), s_mean_long = mean(start_lng))
  
  
map_ride_start <- ggmap(get_map(location = c(lat = 41.86, lon = Mean_dir$s_mean_long),
                                zoom = 11,
                                maptype = "terrain",
                                color = "color")) +
  geom_point(data = start_locations, aes(y = start_lat, x= start_lng), 
               color = "darkgreen", size=1.5) + 
  labs(title = "Start locations",
       x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(size=16))

print(map_ride_start) 

temp_station_summary <- tri_tab_sorted %>% 
  select(start_station_name,start_station_id,ride_dur,member_casual,start_lat,start_lng) %>% 
  group_by(start_station_id) %>% 
  summarise(ride_dur_total = sum(ride_dur),count = n())
  
temp_table <- tri_tab_sorted %>% 
  distinct(start_station_id, .keep_all = TRUE)  %>% 
  select(start_station_name,start_station_id,member_casual,start_lat,start_lng)

temp_table <- temp_table %>% 
  mutate(ride_dur_total = temp_station_summary$ride_dur_total) %>%
  mutate(number_rides = temp_station_summary$count) %>% 
  arrange(-ride_dur_total)

top_10_station_id <- temp_table %>% top_n(10,ride_dur_total)

names(top_10_station_id)[names(top_10_station_id) == "start_station_name"] <- "StationNames"

start_station_name

Mean_dir = 0
Mean_dir <- top_10_station_id %>%
  summarise(s_mean_lat = mean(start_lat), s_mean_long = mean(start_lng))



map_ride_start <- ggmap(get_map(location = c(lat = Mean_dir$s_mean_lat, lon = Mean_dir$s_mean_long),
                                zoom = 10,
                                maptype = "terrain",
                                color = "color")) +
  geom_point(data = top_10_station_id, aes(y = start_lat, x= start_lng, color = StationNames), 
             size=5) + 
  geom_text(data = top_10_station_id, aes(y = start_lat, x= start_lng,label = StationNames,
                                           hjust = -0.2, vjust = -0.1, color = fct_relevel(StationNames,"S Wentworth Ave & W 111th St",
                                                                                           "Cottage Grove Ave & Oakwood Blvd",
                                                                                           "Lincoln Ave & Sunnyside Ave",
                                                                                           "Lincoln Ave & Diversey Pkwy",
                                                                                           "Seeley Ave & Garfield Blvd",
                                                                                           "Western Ave & Lunt Ave",
                                                                                           "Greenwood Ave & 47th St",
                                                                                           "Sheffield Ave & Waveland Ave",
                                                                                           "California Ave & North Ave",
                                                                                           "1Dauphin Ave & 103rd St"))) +
  labs(title = "Top 10 start location with maximum ride duration for April 2020 to March 2021",
       x = "Longitude", y = "Latitude") +
  #scale_color_grey() +
  theme(plot.title = element_text(size=16)) +
  theme(legend.title = element_blank())
  


print(map_ride_start) 

Bar_plot <- top_10_station_id %>% 
  #filter(start_month >=1 & start_month <=3 & start_year==2021) %>% 
  ggplot(aes(x = fct_relevel(StationNames,"S Wentworth Ave & W 111th St",
                             "Cottage Grove Ave & Oakwood Blvd",
                             "Lincoln Ave & Sunnyside Ave",
                             "Lincoln Ave & Diversey Pkwy",
                             "Seeley Ave & Garfield Blvd",
                             "Western Ave & Lunt Ave",
                             "Greenwood Ave & 47th St",
                             "Sheffield Ave & Waveland Ave",
                             "California Ave & North Ave",
                             "1Dauphin Ave & 103rd St") , y =ride_dur_total  /1000)) +
  geom_col(aes(fill = fct_relevel(StationNames,"S Wentworth Ave & W 111th St",
                                  "Cottage Grove Ave & Oakwood Blvd",
                                  "Lincoln Ave & Sunnyside Ave",
                                  "Lincoln Ave & Diversey Pkwy",
                                  "Seeley Ave & Garfield Blvd",
                                  "Western Ave & Lunt Ave",
                                  "Greenwood Ave & 47th St",
                                  "Sheffield Ave & Waveland Ave",
                                  "California Ave & North Ave",
                                  "1Dauphin Ave & 103rd St") ),position = "dodge",width = 0.5)+
  theme(legend.title = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=16)) +
  labs(title = "Top 10 start location with maximum ride duration for April 2020 to March 2021", x = "Start Stations",y = "Ride duration in 1000km ")
  
 
print(Bar_plot) 

grid.arrange(map_ride_start,Bar_plot,ncol = 2)

# bottom 10 
# ---------------------------------------------------------------------

top_10_station_id <- temp_table %>% top_n(-10,ride_dur_total)

names(top_10_station_id)[names(top_10_station_id) == "start_station_name"] <- "StationNames"

start_station_name

Mean_dir = 0
Mean_dir <- top_10_station_id %>%
  summarise(s_mean_lat = mean(start_lat), s_mean_long = mean(start_lng))

map_ride_start <- ggmap(get_map(location = c(lat = Mean_dir$s_mean_lat, lon = Mean_dir$s_mean_long),
                                zoom = 10,
                                maptype = "terrain",
                                color = "color")) +
  geom_point(data = top_10_station_id, aes(y = start_lat, x= start_lng, color = StationNames), 
             size=5) + 
  geom_text(data = top_10_station_id, aes(y = start_lat, x= start_lng,label = StationNames,
                                          hjust = -0.2, vjust = -0.1, color = fct_relevel(StationNames,"Wentworth Ave & Cermak Rd (Temp)",
                                                                                          "Washtenaw Ave & Ogden Ave",
                                                                                          "Delano Ct & Roosevelt Rd" ,
                                                                                          "Michigan Ave & 18th St" ,         
                                                                                          "Cannon Dr & Fullerton Ave", 
                                                                                          "Shedd Aquarium",
                                                                                          "Leavitt St & Lawrence Ave",
                                                                                          "Shields Ave & 31st St",
                                                                                          "Kilbourn Ave & Milwaukee Ave",
                                                                                          "Warren Park East"))) +
  labs(title = "Lowest 10 start location with minimum ride duration for April 2020 to March 2021",
       x = "Longitude", y = "Latitude") +
  #scale_color_grey() +
  theme(plot.title = element_text(size=16)) +
  theme(legend.title = element_blank())



print(map_ride_start) 

Bar_plot <- top_10_station_id %>% 
  #filter(start_month >=1 & start_month <=3 & start_year==2021) %>% 
  ggplot(aes(x = fct_relevel(StationNames,"Wentworth Ave & Cermak Rd (Temp)",
                             "Washtenaw Ave & Ogden Ave",
                             "Delano Ct & Roosevelt Rd" ,
                             "Michigan Ave & 18th St" ,         
                             "Cannon Dr & Fullerton Ave", 
                             "Shedd Aquarium",
                             "Leavitt St & Lawrence Ave",
                             "Shields Ave & 31st St",
                             "Kilbourn Ave & Milwaukee Ave",
                             "Warren Park East") , y =ride_dur_total)) +
  geom_col(aes(fill = fct_relevel(StationNames,"Wentworth Ave & Cermak Rd (Temp)",
                                  "Washtenaw Ave & Ogden Ave",
                                  "Delano Ct & Roosevelt Rd" ,
                                  "Michigan Ave & 18th St" ,         
                                  "Cannon Dr & Fullerton Ave", 
                                  "Shedd Aquarium",
                                  "Leavitt St & Lawrence Ave",
                                  "Shields Ave & 31st St",
                                  "Kilbourn Ave & Milwaukee Ave",
                                  "Warren Park East") ),position = "dodge",width = 0.5)+
  theme(legend.title = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=16)) +
  labs(title = "Lowest 10 start location with minimum ride duration for April 2020 to March 2021", x = "Start Stations",y = "Ride duration in kms ",
      caption = "Since the maximum ride duration is 3491 secs, this plot is showing the time in seconds. "  )
  


print(Bar_plot) 

grid.arrange(map_ride_start,Bar_plot,ncol = 2)
