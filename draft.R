#Import data
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
q1 <- read.csv("C:/Users/Vinky/Desktop/Bike Share Toronto Ridership_Q1 2018.csv")
q2 <- read.csv("C:/Users/Vinky/Desktop/Bike Share Toronto Ridership_Q2 2018.csv")
q3 <- read.csv("C:/Users/Vinky/Desktop/Bike Share Toronto Ridership_Q3 2018.csv")
q4 <- read.csv("C:/Users/Vinky/Desktop/Bike Share Toronto Ridership_Q4 2018.csv")
total <- rbind(q1,q2,q3,q4)
head(total)
glimpse(total)

#Recode variable
trip_start_time <- force_tz(mdy_hm(total$trip_start_time), tzone="EST")
total$trip_start_time = trip_start_time
trip_stop_time <- force_tz(mdy_hm(total$trip_stop_time), tzone="EST")
total$trip_stop_time = trip_stop_time


#proportion of membership
annual_user_count = count(total, user_type == "Annual Member")[2,2]
casual_user_count = count(total, user_type == "Annual Member")[1,2]
annual_prop = as.numeric(annual_user_count/(length(total$user_type))) #~ 81% users are annual
casual_prop = as.numeric(casual_user_count/(length(total$user_type))) #~18% of users are casual
#bar chart


#Do some data exploration

#explore time related characteristics 
#monthly use
month = month(trip_start_time)

monthly_user <- data.frame(user_type = total$user_type, month = unlist(month))
ggplot(monthly_user,aes(x=month,group=user_type,fill=user_type))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

#see that bikeshare is most popular during the summer months and least during the winter months among both user types 
#most popular month is july and least is january
##reasonable as Toronto does have four distinct seasons


#weekly use
weekday = wday(trip_start_time)
weekly_user <- data.frame(user_type = total$user_type, week = unlist(weekday))
ggplot(weekly_user,aes(x=week,group=user_type,fill=user_type))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()


#where 1=sunday and 7 = saturday
#annual members are more likely to ride during the weekdays whereas casual members are more likely to ride during the weekends
#note that promotion of Free Ride Day Wednesday for month of June and hos

## overwhelmingly amount of annual members than casual
## reasonable to believe that annual members are purchased by those who are residents of Toronto or are frequently in the city
## and perhaps used for commute to work/school (regular weekday schedules)



# daily use/time of day
hour_of_day_start = hour(trip_start_time)
daily_user_start <- data.frame(user_type = total$user_type, daily = unlist(hour_of_day_start))
ggplot(daily_user_start,aes(x=daily,group=user_type,fill=user_type))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
#for both user types-highest use at 5pm and lowest at 4am 
#for annual members- we see actually see two distinct peaks near 8 am and 5pm
## perhaps again, used for those to commute to work/school (regular working hours)
## for casual members, don't see the same peak 

daily_user_stop <- data.frame(user_type = total$user_type, daily = unlist(hour_of_day_stop))
ggplot(daily_user_stop,aes(x=daily,group=user_type,fill=user_type))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

#follows the same distribution as above in terms of spread and across users
##reasonable as rides are limited to 30 minute lengths with a fee charged for overages
## would expect that most rides are within an hourly increments

#visualize as time series


#duration
out <- boxplot(trip_duration_seconds~user_type, data=total)
out


#where time is measured in seconds
#extreme of lower whisker, lower hinge, median, upper hinge, upper whisker

#annual member:
#60, 390, 600, 905, 1677
#causual member
#60, 786, 1220, 1804, 3331


# the max duration for annual is 1677s and for casual is 3331s
# aside from the common minimum value, we see that the summary statistic of duration length for casual members is almost double that of annual
q1 <- as.integer(quantile(total$trip_duration_seconds, 0.25))
q3 <- as.integer(quantile(total$trip_duration_seconds, 0.75))
iqr <- q3-q1
ylims = c(q1,q3)


out <-
  total%>%
  filter(trip_duration_seconds >q1-1.5*iqr & total$trip_duration_seconds <q3+1.5*iqr) 

ggplot(out, aes(x = user_type, y = trip_duration_seconds)) + 
  geom_violin() + geom_boxplot(outlier.shape=NA)

#for annual member: higher prob density near q1-median, increasingly lower prob density beyond q3
#for casual member: fairly symmetric prob density on either side of median



#station
##kable the output and make it look neater
station_user_start <- 
  total %>% 
  group_by(user_type, from_station_name) %>%
  summarise(count = length(from_station_name))

station_user_stop <- 
  total %>% 
  group_by(user_type, to_station_name) %>%
  summarise(count = length(to_station_name))

#starting stations
station_user_start %>%
  group_by(user_type) %>% filter(count == max(count)) 
#mostpopular starting station for annual member is sherbourne (with count=17999), for casual member is bay st (with count=11329)

station_user_start %>%
  group_by(user_type) %>% filter(count == min(count))
#lease popular starting station for annual member is wolpack (with count=1), for casual member is d'arcy st (with count=3) or Erskine ave (count = 3)

#stopping stations
station_user_stop %>%
  group_by(user_type) %>% filter(count == max(count)) 
#mostpopular stopping station for annual member is union station (with count=23129), for casual member is bay st/queens quay w (with count=11249)

station_user_stop %>%
  group_by(user_type) %>% filter(count == min(count)) 
#least popular stopping station for annual member is wolfpack (with count=3), for casual member is wolfpack (with count=1)


#let's see how effective that promotion/free bike wednesday was
##can compare wednesday of June to tuesday/thursday and that to May/July




## nice, so we have some idea regarding users' time of use, duration, and most/least frequent stopping/starting stations
## based on the available measurements from the ridership data 
##let's bring in some external info to get more insight on users 

#latitude, longitude of station information accessed through api
library(httr)
library(jsonlite)
res = GET("https://tor.publicbikesystem.net/ube/gbfs/v1/en/station_information")
rawToChar(res$content)
data = fromJSON(rawToChar(res$content))

stat_id = as.numeric(data[["data"]][["stations"]][["station_id"]])
lat = as.numeric(data[["data"]][["stations"]][["lat"]])
lon = as.numeric(data[["data"]][["stations"]][["lon"]])
geo = as.data.frame(cbind(stat_id, lat, lon))

library(geosphere)

z = merge(total, geo, by.x= "from_station_id", by.y="stat_id") 
s = merge(z, geo, by.x = "to_station_id", by.="stat_id")

s <- 
  s %>%
  rename(
    lat_start = lat.x, 
    lon_start = lon.x,
    lat_stop = lat.y,
    lon_stop = lon.y
  )


s <- mutate(s, Distance=distHaversine(cbind(s$lon_start, s$lat_start),cbind(lon_stop,lat_stop)))


dis_out <- boxplot(Distance~user_type, data=s)
dis_out

# Annual    Casual
# 0.0000    0.0000
# 959.3489  879.5609
# 1515.0107 1663.3363
# 2310.7381 2649.4983
# 4337.7109 5304.2989

ggplot(s, aes(x = user_type, y = Distance)) + 
  geom_boxplot(outlier.shape=NA)

#comparison of distance and duration between users



#factor due to weather?
#import data from (website); contains hourly measurements of temperature
#look at hourly temp bcuz toronto tends to have large variation between min and max daily temps
weather <- read.csv("C:/Users/Vinky/Desktop/hourly weather 2018.csv")

hourly_temp <- weather %>%
  select(daily = Date.Time, temp = Temp..Â.C.)


hourly_temp$daily= force_tz(ymd_hm(hourly_temp$daily), tzone="EST")


#need to merge on closest hour value
#should go back and use total data frame 
#s dataframe lost some observation/data upon merging with lat/long info
## should note that in above section

hr <- as.numeric(hour_of_day_start)
total$hour < data.frame(hour_of_day_start)
a = merge(s$hour(trip_start_time), hourly_temp$hour(daily), by.x = "trip_start_time", by.y="daily")


#########others ##########
#possible factors determining bikeshare ridership 
## weather
### temperature (hourly- since the temperatures tends to vary drastically throughout the day in Toronto)

weather <- read.csv("C:/Users/Vinky/Desktop/hourly weather 2018.csv")
#extract columns of interest and assign easier to work with names

weather <- weather %>%
  select(time = Date.Time, temp = Temp..Â.C.)

weather$time= force_tz(ymd_hm(weather$time), tzone="EST")


#nearest hour
nearest_hour <- as.data.frame(round(s$trip_start_time, "hours"))
colnames(nearest_hour)[1] <- "hourly"

station_df <- nearest_hour %>% 
  group_by(hourly) %>% summarise(frequency = n())

weather_df = merge(station_df, weather, by.x = "hourly", by.y="time", all.x = TRUE)

#check if a linear relationship
hist(weather_df$temp)

plot(frequency~temp, data=weather_df)




