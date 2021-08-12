Google Data Analytics professional Certificate Case Study - Cysclistic

--Loading packages

library(tidyverse)
library(lubridate)
library(ggplot2)

##Importing and Reading Files
may_2020 <- read_csv("E:/Case study/202005-divvy-tripdata.csv")
jun_2020 <- read_csv("E:/Case study/202006-divvy-tripdata.csv")
jul_2020 <- read_csv("E:/Case study/202007-divvy-tripdata.csv")
aug_2020 <- read_csv("E:/Case study/202008-divvy-tripdata.csv")
sep_2020 <- read_csv("E:/Case study/202009-divvy-tripdata.csv")
oct_2020 <- read_csv("E:/Case study/202010-divvy-tripdata.csv")
nov_2020 <- read_csv("E:/Case study/202011-divvy-tripdata.csv")
dec_2020 <- read_csv("E:/Case study/202012-divvy-tripdata.csv")
jan_2021 <- read_csv("E:/Case study/202101-divvy-tripdata.csv")
feb_2021 <- read_csv("E:/Case study/202102-divvy-tripdata.csv")
mar_2021 <- read_csv("E:/Case study/202103-divvy-tripdata.csv")
apr_2021 <- read_csv("E:/Case study/202104-divvy-tripdata.csv")


#Checking for inconsistencies in data frames
str(may_2020)
str(jun_2020)
str(jul_2020)
str(aug_2020)
str(sep_2020)
str(oct_2020)
str(nov_2020)
str(dec_2020)
str(jan_2021)
str(feb_2021)
str(mar_2021)
str(apr_2021)


##Changing start_station_id and end_station_id to character
may_2020 <- mutate(may_2020, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
jun_2020 <- mutate(jun_2020, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
jul_2020 <- mutate(jul_2020, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
aug_2020 <- mutate(aug_2020, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
sep_2020 <- mutate(sep_2020, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
oct_2020 <- mutate(oct_2020, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
nov_2020 <- mutate(nov_2020, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
dec_2020 <- mutate(dec_2020, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
jan_2021 <- mutate(jan_2021, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
feb_2021 <- mutate(feb_2021, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
mar_2021 <- mutate(mar_2021, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
apr_2021 <- mutate(apr_2021, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))

##Stacking 12 data frames into a single data drame
all_trips <- rbind(may_2020, jun_2020, jul_2020, aug_2020, sep_2020, oct_2020,
                   nov_2020, dec_2020, jan_2021, feb_2021, mar_2021, apr_2021)

##Removing lat and lng
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))


##Checking new table
colnames(all_trips)
str(all_trips)
head(all_trips)
summary(all_trips)
dim(all_trips)
nrow(all_trips)

##Number of observation under each type
table(all_trips$member_casual)

##Creating separated columns - date, month, and year for each ride
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%a")

##checking unique values
unique(all_trips$month)
unique(all_trips$day_of_week)


##Adding new column and calculating ride length
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at,
                                  units = "mins")

##Checking structure of columns
str(all_trips)

##Converting 'ride_length' to numeric
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric((all_trips$ride_length))

##Removing ride_length <= 0 and ride_length > 1440 mins (24 * 60)
all_trips_v2 <- all_trips[!(all_trips$ride_length >1440 | 
                              all_trips$ride_length <=0),]

##Combining start and end station
## Removing empty entries
##Separating data by rider_type
all_stations <- bind_rows(data.frame("stations" = all_trips_v2$start_station_name, 
                                     "member_casual" = all_trips_v2$member_casual),
                          data.frame("stations" = all_trips_v2$end_station_name,
                                     "member_casual" = all_trips_v2$member_casual))
all_stations_v2 <- all_stations[!(all_stations$stations == " " | 
                                    is.na(all_stations$stations)),]
all_stations_member <- all_stations_v2[all_stations_v2$member_casual == "member",]
all_stations_casual <- all_stations_v2[all_stations_v2$member_casual == "casual",]


##Top 10 stations used by members and casual riders
top_10_stations <- all_stations_v2 %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)

##Top 10 station used by members
top_10_station_member <- all_stations_member %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)


##Top 10 stations used by Casual riders
top_10_station_casual <- all_stations_casual %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)


#Analysis of ride_length
summary(all_trips_v2$ride_length)

##Comparing ride_length of member and casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

##Analysis by Day of week
all_trips_v2$day_of_week <- paste(strftime(all_trips_v2$ended_at, "%a")) 
unique(all_trips_v2$day_of_week)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

##Putting days of week in order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = median)

##Number of rides by casual and members for each day of week
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  arrange(day_of_week)

#Analysis by Month
all_trips_v2$month <- paste(strftime(all_trips_v2$started_at, "%b"),
                                  sep = " ")
unique(all_trips_v2$month)

all_trips_v2$month <- ordered(all_trips_v2$month, 
                      levels = c( "May", "Jun", "Jul", "Aug", "Sep", "Oct", 
                                  "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$month, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$month, FUN = median)

##Number of rides by member and casual for each month
all_trips_v2 %>% 
group_by(member_casual, month) %>% 
  summarise(no_of_rides = n(), .groups = 'drop') %>% 
  arrange(month)

##Comparing bike type preference by member and casual riders
all_trips_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_rides = n(), .groups = 'drop')

##Comparing number of docked bike used by member and casual for each day of week
all_trips_v2 %>% 
  filter(rideable_type == 'docked_bike') %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)

##Docked_bike rides by casual riders for each day of week
all_trips_v2 %>% 
  filter(rideable_type == 'docked_bike', member_casual == 'casual') %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop')


#Share

##By Rider type
all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarize(average_duration = mean(ride_length)) %>% 
  ggplot(aes(x = member_casual, y = average_duration, fill = member_casual)) + 
  geom_col(position = 'dodge') +
  labs(x = "", y = "Average Duration (min)",
title = "Average Duration vs Rider Type")



##Average riding duration for member and casual riders for each day of week
options(repr.plot.width = 10, repr.plot.height = 8)
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length), .groups = "drop") %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = 'dodge') + 
  labs(x = 'Day of Week', y = 'Average Duration (min)', fill = 'Member/Casual',
       title = 'Average Riding Duration by Day: Member vs. Casual Riders')


##Average number of rides for member and casual members for each day of week
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides,
             fill = member_casual)) +
  geom_col(position = 'dodge') +
  labs(x = "Day of Week", y = 'Number of Rides', 
       fill = 'Member/Casual',
       title = 'Average Number of Rides by Day: Member vs. Casual Riders')
  
##Average number of rides for member and casual riders by month
options(repr.plot.width = 10, repr.plot.height = 8)
all_trips_v2 %>% 
  group_by(month, member_casual) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Month", y = "Number of Rides",
  fill = "Member/Casual", 
  title = "Average Number of Rides by Month")

##Average number of rides for casual riders by month
options(repr.plot.width = 10, repr.plot.height = 8)
all_trips_v2 %>% 
  group_by(month, member_casual) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  filter(member_casual == "casual") %>% 
  drop_na() %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Month", y = "Number of Rides",
       fill = "Casual", 
       title = "Average Number of Rides by Month: Casual Riders")

##Average number of rides for casual riders by hour 

str(all_trips_v2)
all_trips_v2$start_hour <- as.POSIXct(all_trips_v2$started_at, "%Y-%m-%d %H:%M%S")
str(all_trips_v2)

options(repr.plot.width = 12, repr.plot.height = 8)
all_trips_v2 %>%
  filter(member_casual == 'casual') %>% 
  group_by(hour_of_day = hour(round_date(start_hour, 'hour'))) %>% 
  group_by(hour_of_day, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(-number_of_rides) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                18,19,20,21,22,23)) +
  labs(x = "Time of Day(H)", y = "Number of Rides",
       fill = 'Member/Casual',
       title = "Average Number of Rides by Hour: Casual Riders")


##Top 10 stations used by members

options(repr.plot.width = 10, repr.plot.height = 6)

ggplot(data = top_10_station_member) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count),
           fill = 'cornflowerblue') +
  labs(title = 'Top 10 Stations Used by Members', y = 'Number of Rides',
       x = " ") +
  scale_y_continuous(labels = scales::comma) + coord_flip() +
  theme_minimal()

##Top 10 stations used by casual riders

options(repr.plot.width = 10, repr.plot.height = 6)

ggplot(data = top_10_station_casual) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count),
           fill = 'thistle') +
  labs(title = 'Top 10 Stations Used by Casual Riders', y = 'Number of Rides',
       x = " ") +
  scale_y_continuous(labels = scales::comma) + coord_flip() +
  theme_minimal()

##usage pf different bikes by rider type
options(repr.plot.width = 12, repr.plot.height = 8)

all_trips_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na %>% 
  ggplot(aes(x = member_casual, y = number_of_rides,
             fill = member_casual)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~rideable_type) +
  labs(fill = 'Member/Casual', x = " ", y = 'Number of Rides',
       title = 'Usage of Different Rides: Members vs. Casual Riders')

##Usage of different bike type by month
options(repr.plot.width = 14, repr.plot.height = 10)

all_trips_v2 %>% 
  group_by(month, member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~rideable_type) +
  labs(x = 'Month', y = 'Number of Rides',
       fill = 'Member/Casual',
       title = "Bike Usage Between member and Casual Riders by Month",
       fill = 'Member/Casual') +
  theme(axis.text.x = element_text(angle = 90))

##Usage of different bike type by day of week
options(repr.plot.width = 26, repr.plot.height = 10)

all_trips_v2 %>% 
  group_by(month, day_of_week, member_casual) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~month) +
  labs(x = 'Day of Week', y = 'Number of Rides',
       fill = 'Member/Casual',
       title = "Bike Usage Between member and Casual Riders by Day of Week",
       fill = 'Member/Casual') +
  theme(axis.text.x = element_text(angle = 90))
