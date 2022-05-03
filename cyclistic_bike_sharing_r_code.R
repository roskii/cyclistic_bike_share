## 2) Prepare data

## 2.1) Combining all 12 data (.csv) files into a single file

data_unclean <- list.files(path = "C://Users/Public/r_studio/google_capstone/source_data/02_05_2022_data_bar_all.csv", 
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              
  bind_rows



## 3) Process (cleaning) data 

## 3.1) Installing packages

install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("data.table")
install.packages("stringr")
install.packages("lubridate")
install.packages("viridis")
install.packages("scales")

## 3.2) Loading libraries

library("ggplot2")
library("plyr")
library("dplyr")
library("readr")
library("tidyr")
library("data.table")
library("stringr") 
library("lubridate")
library("viridis")
library("scales")

## 3.3) Checking data types, formats, and looking for errors such as duplication

glimpse(data_unclean) # check colnames, typeof

dim(data_unclean[duplicated(data_all$ride_id),])[1] # check if any duplicate ride_id (as they should be unique)

unique(data_unclean$rideable_type) # confirming unique values ("electric_bike" "classic_bike"  "docked_bike")
unique(data_unclean$member_casual) # confirming unique values ("member" "casual")

mutate(data_unclean, 
       end_station_id = as.character(end_station_id), 
       start_station_id = as.character(start_station_id)) # ensure all station_id's are characters

## 3.4) Removing NA values and rides with errors

data_clean <- data_unclean %>% 
  drop_na(start_lat) %>%
  data_all[!duplicated(data_all$ride_id), ] %>%
  filter(!(ride_length < 0)) # remove NA from end_lat

data_clean <- data_clean %>% # remove trips with a ride length of less than 0
  filter(!(ride_length < 0))

## 3.5) Transforming data

data_clean$date <-as.Date(data_clean$started_at)
data_clean$year <- format(as.Date(data_clean$date), "%Y") # create column for year
data_clean$month <- format(as.Date(data_clean$date), "%m") # create column for month
data_clean$week <- format(as.Date(data_clean$date), "%W") # create column for week
data_clean$day_of_week <- format(as.Date(data_clean$date), "%A") # create column for day of week
data_clean$yyyy_mm_dd <- format(as.Date(data_clean$date), "%Y-%m-%d") # create column for date

data_clean$day_of_week <- as.factor(data_clean$day_of_week) # convert to factor
data_clean$day_of_week <- ordered(data_clean$day_of_week, 
                                  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) # set order for days of week

data_clean$ride_length_seconds <- difftime(data_clean$ended_at, data_clean$started_at) # create column for ride length calculated in seconds

data_clean$started_at_time <- format(as.POSIXct(
  data_clean$started_at), format = "%H:%M:%S") # create column with ride start times

data_clean$ended_at_time <- format(as.POSIXct(
  data_clean$ended_at), format = "%H:%M:%S") # create column with ride end times

data_clean$ride_length_seconds <- as.numeric(str_extract(data_clean$ride_length_seconds, "[0-9]+")) # remove 'secs' from ride_length_seconds

data_clean$ride_length_minutes <- (data_clean$ride_length / 60) # create column with ride length in minutes

## 4) Share data

## 4.1) Map viz 

data_map_all <- data_clean %>%
  select(start_station_name, end_station_name, start_lat, start_lng, end_lat, end_lng, started_at_time, member_casual) %>%
  group_by(start_station_name, member_casual) %>%
  mutate(num_trips=n()) %>%
  distinct(start_station_name, member_casual,.keep_all=TRUE)

## 4.2) Heat map viz - "Member vs Casual Riders Number of Weekly Trips per year"

data_heat_all <- data_clean %>%
  select(yyyy_mm_dd, day_of_week, week, member_casual) %>%
  group_by(yyyy_mm_dd, member_casual) %>%
  mutate(num_trips=n()) %>%
  distinct(yyyy_mm_dd, member_casual,.keep_all=TRUE) # group table by number of trips by day of the year

data_heat_mem_only <- data_heat_all %>%
  filter(member_casual == "member") # filter table with only member riders

data_heat_cas_only <- data_heat_all %>%
  filter(member_casual == "casual") # filter table with only casual riders

viz_2_heat_mem <- ggplot(data_heat_mem_only, aes(as.numeric(week),day_of_week, fill=num_trips))+
  geom_tile(color="white", na.rm=FALSE)+
  scale_y_discrete(limits = rev)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1, 52, length=12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(title="Total Member Riders Trips per Day")+
  scale_fill_viridis(option="mako", direction=-1,name="Number of Trips") # create heat map viz for member riders

viz_2_heat_cas <- ggplot(data_heat_cas_only, aes(as.numeric(week),day_of_week, fill=num_trips))+
  geom_tile(color="white", na.rm=FALSE)+
  scale_y_discrete(limits = rev)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1, 52, length=12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(title="Total Casual Riders Trips per Day")+
  scale_fill_viridis(option="mako", direction=-1,name="Number of Trips") # create heat map viz for casual riders

viz_2_heat_mem_cas <- ggarrange(viz_2_heat_mem, 
                                viz_2_heat_cas, 
                                ncol = 1, nrow = 2, common.legend = TRUE, legend = "right") # group both heat maps into one grid

print(viz_2_heat_mem_cas)

## 4.3) Bar chart viz - "Total Average Number of Daily Rides Started During Each Hour of the Day"

data_bar_all <- data_clean %>%
  select(ride_id, member_casual, started_at, yyyy_mm_dd, day_of_week) %>%
  mutate(hours_start=hour(data_clean$started_at)) # group table by riders start and end times by hour of the day

data_bar_group <- data_bar_all %>%
  distinct(ride_id, hours_start, member_casual) %>%
  group_by(member_casual, hours_start) %>%
  summarise("num_rides_in_hour" = n())

hr <- c(0:24) # set 24 hr scale on x axis

viz_3_bar <- ggplot(data_bar_all, aes(hours_start, fill=member_casual))+
  geom_bar(position="dodge")+
  scale_y_continuous(labels=function(x) format(x / 365, digits=3))+
  scale_x_continuous(name="Hour of Day", labels=as.character(hr), breaks=hr)+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(title="Total Average Number of Daily Rides Started During Each Hour of the Day")+
  scale_fill_viridis_d(option="mako", direction=-1, begin=0.2, end=0.8, name="Customer Type") # create bar chart viz 

print(viz_3_bar)

## 4.4) Scatter plot viz - "Average Daily Ride Length"

data_scatter_all <- data_clean %>%
  group_by(month, day_of_week, member_casual) %>%
  summarise(ave_ride_length_minutes = mean(ride_length_minutes, na.rm = TRUE)) # group table by average ride length in minutes for each day of the week

rename_months <- as_labeller(c(`01` = "January", `02` = "February", `03` = "March", `04` = "April", 
                               `05` = "May", `06` = "June", `07` = "July", `08` = "August", 
                               `09` = "September", `10` = "October", `11` = "November", `12` = "December")) # rename individual chart titles to correct months

viz_4_scatter <- ggplot(data_scatter_all, aes(day_of_week, ave_ride_length_minutes, color=member_casual))+
  geom_point(size=2)+
  facet_wrap(~month, labeller = rename_months)+
  theme(axis.text.x=element_text(angle=45),axis.title.x = element_blank(), axis.title.y = element_blank())+
  scale_x_discrete(labels=c("Sunday"="Sun","Monday"="Mon","Tuesday"="Tues","Wednesday"="Wed","Thursday"="Thurs","Friday"="Fri","Saturday"="Sat"))+
  labs(title="Average Daily Ride Length")+
  scale_color_viridis_d(option="mako", direction=-1,begin=0.3, end=0.8, name="Customer Type") # create scatter plot viz
  
print(viz_4_scatter)

## 5) Saving data

## 5.1) Entire clean dataset

fwrite(data_clean,"C://Users/Public/r_studio/google_capstone/02_05_2022_data_clean.csv", col.names = TRUE, row.names = FALSE)

## 5.2) Map data (for Excel analysis & Tableau viz)

fwrite(data_map_all,"C://Users/Public/r_studio/google_capstone/02_05_2022_data_map_all.csv", col.names = TRUE, row.names = FALSE)

## 5.3) Heat map data (for Excel analysis)

fwrite(data_heat_all,"C://Users/Public/r_studio/google_capstone/02_05_2022_data_heat_all.csv", col.names = TRUE, row.names = FALSE)

fwrite(data_heat_mem_only,"C://Users/Public/r_studio/google_capstone/02_05_2022_data_heat_mem_only.csv", col.names = TRUE, row.names = FALSE)

fwrite(data_heat_cas_only,"C://Users/Public/r_studio/google_capstone/02_05_2022_data_heat_cas_only.csv", col.names = TRUE, row.names = FALSE)

## 5.4) Bar chart data (for Excel analysis)

fwrite(data_bar_all,"C://Users/Public/r_studio/google_capstone/02_05_2022_data_bar_all.csv", col.names = TRUE, row.names = FALSE)

## 5.5) Scatter plot data (for Excel analysis)

fwrite(data_scatter_all,"C://Users/Public/r_studio/google_capstone/02_05_2022_data_scatter_all.csv", col.names = TRUE, row.names = FALSE)
