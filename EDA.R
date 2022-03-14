# Use package rio to import data into R directly from the web
library(rio)
library(lubridate)
library(ggplot2)
library(dplyr)
library(psych)
library(forecast)

url=c("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/11/MetroBikeShare_2016_Q3_trips-2.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/09/metro-bike-share-trips-2016-q4.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/11/la_metro_gbfs_trips_Q1_2017-2.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2017/07/la_metro_gbfs_trips_Q2_2017.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2016/10/metro-bike-share-trips-2017-q3.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/02/metro-bike-share-trips-2017-q4-v2.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/04/metro-bike-share-trips-2018-q1.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/08/metro-bike-share-trips-2018-q2.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/10/metro-bike-share-trips-2018-q3.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/01/metro-bike-share-trips-2018-q4.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/04/metro-bike-share-trips-2019-q1.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/08/metro-bike-share-trips-2019-q2.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/10/metro-bike-share-trips-2019-q3-1.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/01/metro-bike-share-trips-2019-q4.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/04/metro-bike-share-trips-2020-q1.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/03/metro-trips-2020-q2-v2.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/10/metro-trips-2020-q3.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/01/metro-trips-2020-q4.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/04/metro-trips-2021-q1-1.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/07/metro-trips-2021-q2.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/10/metro-trips-2021-q3.zip"
)

# 2016 Q3
head(import(url[1]),2)
# 2016 Q4
head(import(url[2]),2)
# 2017 Q1
head(import(url[3]),2)
# 2017 Q2
head(import(url[4]),2)
# 2017 Q3
head(import(url[5]),2)
# 2017 Q4
head(import(url[6]),2)
# 2018 Q1
head(import(url[7]),2)
# 2018 Q2
head(import(url[8]),2)
# 2018 Q3
head(import(url[9]),2)
# 2018 Q4
head(import(url[10]),2)
# 2019 Q1
head(import(url[11]),2)
# 2019 Q2
head(import(url[12]),2)
# 2019 Q3
head(import(url[13]),2)
# 2019 Q4
head(import(url[14]),2)
# 2020 Q1
head(import(url[15]),2)
# 2020 Q2
head(import(url[16]),2)
# 2020 Q3
head(import(url[17]),2)
# 2020 Q4
head(import(url[18]),2)
# 2021 Q1
head(import(url[19]),2)
# 2021 Q2
head(import(url[20], which=2),2)
# 2021 Q3
head(import(url[21], which=2),2)

#loop
data = import(url[1])
#data$bike_type=NA
str(data)

data$start_time = mdy_hm(data$start_time)
data$end_time = mdy_hm(data$end_time)
str(data)

FirstQuarterNames = names(data)

for (i in 2:(length(url))){
  print(i)
  if (i<20){
  quarter_i=import(url[i])}
  else{  quarter_i=import(url[i],which=2)}
  colnames(quarter_i)=FirstQuarterNames
  if(i %in% c(2,4,6,7,8,9,10,11,12,14)){
    quarter_i$start_time=ymd_hms(quarter_i$start_time)
    quarter_i$end_time=ymd_hms(quarter_i$end_time)
  }else{
    quarter_i$start_time=mdy_hm(quarter_i$start_time)
    quarter_i$end_time=mdy_hm(quarter_i$end_time)
  }
  common_cols <- intersect(colnames(quarter_i), FirstQuarterNames)
  data=rbind(data[common_cols ], quarter_i[common_cols ])
}


dim(data)
str(data)

head(data,2)
tail(data,2)


########################### EDA ########################### 
summary(data)
data$new_startmonth<- format(as.Date(data$start_time), "%Y-%m")
data$new_startyear<- format(as.Date(data$start_time), "%Y")
data$new_startday<- format(as.Date(data$start_time), "%Y-%m-%d")
data$start_DayOfWeek=wday(data$start_time,label= T, abbr=F)
data$new_starthour = format(as.POSIXct(data$start_time), format = "%Y-%m-%d %H")


# Check which day of the week has the most demand
#-> Fridays seem to have the most demand, and Monday has the lowest demand.
trips_weekday= data %>%                              
  group_by(start_DayOfWeek) %>%
  summarise(unique_trip= n_distinct(trip_id ))

ggplot(trips_weekday, aes(x=start_DayOfWeek, y=unique_trip, group=1)) + 
  theme_bw() +
  geom_point() + geom_line() +
  #geom_bar(stat="identity", width=0.7) + # for bar plot
  labs( y="Unique trip", x="Start Day of Week", title="Unique trip per start week of day")


# check daily, monthly and annual demand (count of unique trip)  
# Daily demand
# -> observe some cyclical and seasonal trend
trips_daily =data %>%                              
  group_by(new_startday) %>%
  summarise(unique_trip= n_distinct(trip_id ))

# transfer data in ts object and use autoplot
trips_daily$new_startday = as.Date(trips_daily$new_startday, format = "%Y-%m-%d")

startday = as.numeric(format(trips_daily$new_startday[1], "%j")) 
# 2016-07-07 is the 189th day in 2016

trips_daily.ts = ts(trips_daily$unique_trip, 
   start=c(2016,startday), 
   frequency=365.25)
autoplot(trips_daily.ts) + theme_bw() +
  labs( y="Unique trips", title="Daily unique trips")

# use ggplot
ggplot(trips_daily, aes(x=new_startday, y=unique_trip)) + 
  theme_bw() +
  geom_point() + labs( y="Unique trips", x="Time", title="Daily unique trips")

# Monthly demand
# -> observe monthly seasonality
# -> July ~ October seem to have the highest demand for 2016-2019
# -> Starting 02/2020, demand decreases drastically, maybe due to COVID.
trips_month =data %>%                              
  group_by(new_startmonth) %>%
  summarise(unique_trip= n_distinct(trip_id ))

# transfer data in ts object and use autoplot
trips_month.ts = ts(trips_month$unique_trip, start=c(2016,7), frequency=12)
autoplot(trips_month.ts) + theme_bw() + labs(y="Unique trips", title="Monthly unique trips")

# using ggplot
ggplot(trips_month, aes(x=new_startmonth, y=unique_trip)) + 
  theme_bw() +
  geom_point() +labs(y="Unique trips", x="Time", title="Monthly unique trips")

# Yearly demand
# -> The demand in 2020 and 2021 seems to be lower than 2018 and 2019
# -> Demand increases from 2016 to 2018, and then decreases. 2018 has the largest demand.
trips_year=data %>%                              
  group_by(new_startyear) %>%
  summarise(unique_trip= n_distinct(trip_id ))

ggplot(trips_year, aes(x=new_startyear, y=unique_trip, group=1)) + 
  theme_bw() + geom_point() + geom_line()
  labs( y="unique trip", x="Time", title="Annual unique trip")


# Check average trip duration 
# Monthly average trip duration
# -> There is no clear pattern in monthly duration.
# -> Starting 05/2020, average duration for each month increases. Average duration is longer than 40 mins.

duration_monthly=summarise(
  group_by(data, new_startmonth),
  avg_duration = mean(duration))

# using autoplot
duration_monthly.ts = ts(duration_monthly$avg_duration, 
                         start=c(2016,7), frequency=12)
autoplot(duration_monthly.ts) + theme_bw() +
  labs(y="Average duration", title="Monthly average trip duration")

# using ggplot
ggplot(duration_monthly, aes(x=new_startmonth, y=avg_duration)) + 
  theme_bw() +
  geom_point() + labs( y="Average duration", x="Time", title="Monthly average trip duration")

# Yearly average trip duration
# -> 2021 has the longest average duration. Average duration for each year increases from 2019 to 2021. 2018 also has really high average duration of 42 mins per trip.

duration_yearly=summarise(
  group_by(data, new_startyear),
  avg_duration = mean(duration))

ggplot(duration_yearly, aes(x=new_startyear, y=avg_duration, group=1)) + 
  theme_bw() + geom_point() + geom_line() +
  labs( y="Average duration", x="Time", title="Yearly average duration")

# Average duration for each day of week
#-> Weekends have much longer duration than week days. 
# ->Average duration for trips on weekends are larger then 45 mins, while average duration for weekday trips are less than 35 mins.  

duration_dayofweek=summarise(
  group_by(data, start_DayOfWeek),
  avg_duration = mean(duration))

ggplot(duration_dayofweek, aes(x=start_DayOfWeek, y=avg_duration, group=1)) + 
  theme_bw() +geom_point() + geom_line()
  labs( y="Average duration", x="Day of Week", title="Average duration for day of week")


# check monthly unique start station  
# -> Users in 2020 rent bikes from much more different start stations than users in previous years. 
# -> It infers that more stations were built in the past two years.

startstation_month=data %>%                              
  group_by(new_startmonth) %>%
  summarise(unique_startstaion = n_distinct(start_station_id))

startstation_month %>% head
startstation_month.ts = ts(startstation_month$unique_startstaion,
                           start=c(2016,7), frequency=12)
autoplot(startstation_month.ts) + theme_bw() +
  labs( y="Unique start staions", title="Monthly unique start stations")

ggplot(startstation_month, aes(x=new_startmonth, y=unique_startstaion)) + 
  theme_bw() +
  geom_point() + labs( y="Unique start staions", x="Time", title="Monthly unique start stations")


# Route types
# -> Most of trips are one-way.
ggplot(data, aes(x=trip_route_category, fill =trip_route_category )) + 
  theme_bw() +
  geom_bar()+ labs( y="Number of trips", x="trip route category", title="Distribution of trip route category")

# Passholder types
# -> Most popular passholders' plans are Monthly Pass, and Walk-up.
ggplot(data, aes(x=passholder_type, fill =passholder_type)) + 
  theme_bw() + geom_bar()


# Unique bike ids
# Bikes used daily
# -> there are some seasonal and cyclical patterns 
bikes_daily =data %>%                              
  group_by(new_startday) %>%
  summarise(unique_bikes= n_distinct(bike_id ))

bikes_daily$new_startday = as.Date(bikes_daily$new_startday, format = "%Y-%m-%d")
bikes_daily.ts = ts(bikes_daily$unique_bikes, 
                    start=c(2016,startday), 
                    frequency=365.25)
autoplot(bikes_daily.ts) + theme_bw() +
  labs( y="Unique bikes", title="Daily unique bikes")

# Bikes used monthly
# -> There are no stable seasonal patterns
# -> The number of bikes in use has went to two sharp increases, 
# one in the end of 2017 and the other in the beginning of 2019.
bikes_monthly =data %>%                              
  group_by(new_startmonth) %>%
  summarise(unique_bikes= n_distinct(bike_id ))

bikes_monthly %>% head
bikes_monthly.ts = ts(bikes_monthly$unique_bikes, start=c(2016,7), frequency=12)
autoplot(bikes_monthly.ts) + theme_bw() +
  labs( y="Unique bikes", title="Monthly unique bikes")

# Bikes used yearly 
# bikes in used have increased dramatically in 2017 and 2019, but starts dropping after then.
bikes_yearly =data %>%                              
  group_by(new_startyear) %>%
  summarise(unique_bikes= n_distinct(bike_id ))

ggplot(bikes_yearly, aes(x=new_startyear, y=unique_bikes, group=1)) +
  geom_point() + geom_line() + theme_bw()
  labs( y="Unique bikes", title="Yearly unique bikes")
  

##############################################################################
# Hourly demand
data %>% head

trips_hourly = data %>% group_by(new_starthour) %>% 
  summarise(uniqe_trips = n_distinct(trip_id))

trips_hourly %>% head
starthour = as.numeric(ymd_hms("2016-07-07 04:00:00") - ymd_hms("2016-01-01 00:00:00"))*24 
starthour
trips_hourly.ts = ts(trips_hourly$uniqe_trips, start=c(2016,starthour),
                     frequency=365.25*24)

autoplot(trips_hourly.ts) + theme_bw() +
    labs( y="Unique trips", title="Hourly unique trips")

# Hourly demand in 2016
trips_hourly_2016 = window(trips_hourly.ts, end=c(2017,0))
autoplot(trips_hourly_2016) + theme_bw() +
  labs( y="Unique trips", title="Hourly unique trips in 2016")

# Hourly demand in 2017
trips_hourly_2017 = window(trips_hourly.ts, start=c(2017,0), end=c(2018,0))
autoplot(trips_hourly_2017) + theme_bw() +
  labs( y="Unique trips", title="Hourly unique trips in 2017")

# Hourly demand in 2018
trips_hourly_2018 = window(trips_hourly.ts, start=c(2018,0), end=c(2019,0))
autoplot(trips_hourly_2018) + theme_bw() +
  labs( y="Unique trips", title="Hourly unique trips in 2018")

# Hourly demand in 2019
trips_hourly_2019 = window(trips_hourly.ts, start=c(2019,0), end=c(2020,0))
autoplot(trips_hourly_2019) + theme_bw() +
  labs( y="Unique trips", title="Hourly unique trips in 2019")

# Hourly demand in 2020
trips_hourly_2020 = window(trips_hourly.ts, start=c(2020,0), end=c(2021,0))
autoplot(trips_hourly_2020) + theme_bw() +
  labs( y="Unique trips", title="Hourly unique trips in 2020")

# Hourly demand in 2021
trips_hourly_2021 = window(trips_hourly.ts, start=c(2021,0))
autoplot(trips_hourly_2021) + theme_bw() +
  labs( y="Unique trips", title="Hourly unique trips in 2021")



############################################################
# Import & merge station data

#setwd("C:/Users/Lin Liu/Desktop/522Final Project")
station_data  = read.csv("metro-bike-share-stations-2021-10-01.csv",header=FALSE)
colnames(station_data)=c('station_id','station_name','golive_date','region','status')


alldata = merge(data, station_data, by.x="start_station_id", by.y="station_id",all.x=TRUE)
alldata = alldata[order(alldata$start_time),]
alldata %>% head()


#check station status
# 85% of the stations are active, and 15% of the stations are inactive
alldata%>% count(status)%>%  mutate(prop = n / sum(n))
ggplot(alldata, aes(x=status, fill = status)) + theme_bw() + geom_bar()

# alldata = alldata %>% filter(status=="Active")

##############################################################
# Check locations
dtla=alldata %>% filter(region=='DTLA')
pasadena=alldata %>% filter(region=='Pasadena')
northhollywood=alldata %>% filter(region=='North Hollywood')
westside=alldata %>% filter(region=='Westside')

#############################################
#########################################
# Check which day of the week has the most demand
#-> For DTLA, weekdays seem to have much more demand than weekends.
#-> For Pasadena, Tuesdays have the most demand, and Thursdays have the least demand.
#-> For North Hollywood, weekends seem to have more demand than weekdays. Mondays have the least demand.
#-> For Westside, weekends have much more demand than weekdays. 
dtla %>% head()

# DTLA
trips_weekday_dtla= dtla  %>%
  group_by(start_DayOfWeek) %>%
  summarise(unique_trip= n_distinct(trip_id ))

ggplot(trips_weekday_dtla, aes(x=start_DayOfWeek, y=unique_trip, group=1)) + 
  theme_bw() +
  geom_point() + geom_line() +
  #geom_bar(stat="identity", width=0.7) + # for bar plot
  labs( y="Unique trip", x="Start Day of Week", title="DTLA unique trip per start week of day")

# Pasadena
trips_weekday_pasadena= pasadena  %>%   
  group_by(start_DayOfWeek) %>%
  summarise(unique_trip= n_distinct(trip_id ))

ggplot(trips_weekday_pasadena, aes(x=start_DayOfWeek, y=unique_trip, group=1)) + 
  theme_bw() +
  geom_point() + geom_line() +
  #geom_bar(stat="identity", width=0.7) + # for bar plot
  labs( y="Unique trip", x="Start Day of Week", title="Pasadena unique trip per start week of day")

# North Hollywood
trips_weekday_northhollywood= northhollywood  %>%   
  group_by(start_DayOfWeek) %>%
  summarise(unique_trip= n_distinct(trip_id ))

ggplot(trips_weekday_northhollywood, aes(x=start_DayOfWeek, y=unique_trip, group=1)) + 
  theme_bw() +
  geom_point() + geom_line() + 
  #geom_bar(stat="identity", width=0.7) + # for bar plot
  labs( y="Unique trip", x="Start Day of Week", title="North Hollywood unique trip per start week of day")


# Westside
trips_weekday_westside= westside  %>%                              
  group_by(start_DayOfWeek) %>%
  summarise(unique_trip= n_distinct(trip_id ))

ggplot(trips_weekday_westside, aes(x=start_DayOfWeek, y=unique_trip, group=1)) + 
  theme_bw() +
  geom_point() + geom_line() +
  #geom_bar(stat="identity", width=0.7) + # for bar plot
  labs( y="Unique trip", x="Start Day of Week", title="Westside unique trip per start week of day")



#############################################
#########################################
# Demand for different regions 
# Daily
# -> DTLA has the most demand, but the overall demand level starts decreasing dramatically starting from Q2 of 2020
# -> there are some seasonal trends in the daily data
trips_daily_dtla =dtla %>% group_by(new_startday) %>% summarise(unique_trip= n_distinct(trip_id ))
trips_daily_dtla.ts = ts(trips_daily_dtla$unique_trip, start=c(2016,startday), frequency=365.25)

trips_daily_pasadena =pasadena %>%group_by(new_startday) %>%summarise(unique_trip= n_distinct(trip_id ))
trips_daily_pasadena.ts = ts(trips_daily_pasadena$unique_trip, start=c(2016,startday), frequency=365.25)

trips_daily_northhollywood =northhollywood %>%group_by(new_startday) %>%summarise(unique_trip= n_distinct(trip_id ))
trips_daily_northhollywood.ts = ts(trips_daily_northhollywood$unique_trip, start=c(2016,startday), frequency=365.25)

trips_daily_westside =westside %>%group_by(new_startday) %>%summarise(unique_trip= n_distinct(trip_id ))
trips_daily_westside.ts = ts(trips_daily_westside$unique_trip, start=c(2016,startday), frequency=365.25)

autoplot(trips_daily_dtla.ts, series='DTLA') + theme_bw() + 
  autolayer(trips_daily_pasadena.ts,series='Pasadena') + 
  autolayer(trips_daily_northhollywood.ts,series='North Hollywood') +
  autolayer(trips_daily_westside.ts,series='Westside')+
  xlab("Time") + ylab("Trips") + ggtitle("Daily Demand in different regions")


# Monthly
# -> DTLA has the highest demand, while North Hollywood has the lowest demand. 
# -> Starting 02/2020, demand decreases drastically, maybe due to COVID.
trips_month_dtla =dtla %>% group_by(new_startmonth) %>% summarise(unique_trip= n_distinct(trip_id ))
trips_month_dtla.ts = ts(trips_month_dtla$unique_trip, start=c(2016,7), frequency=12)

trips_month_pasadena =pasadena %>%group_by(new_startmonth) %>%summarise(unique_trip= n_distinct(trip_id ))
trips_month_pasadena.ts = ts(trips_month_pasadena$unique_trip, start=c(2016,7), frequency=12)

trips_month_northhollywood =northhollywood %>%group_by(new_startmonth) %>%summarise(unique_trip= n_distinct(trip_id ))
trips_month_northhollywood.ts = ts(trips_month_northhollywood$unique_trip, start=c(2016,7), frequency=12)

trips_month_westside =westside %>%group_by(new_startmonth) %>%summarise(unique_trip= n_distinct(trip_id ))
trips_month_westside.ts = ts(trips_month_westside$unique_trip, start=c(2016,7), frequency=12)

autoplot(trips_month_dtla.ts, series='DTLA') + theme_bw() + 
  autolayer(trips_month_pasadena.ts,series='Pasadena') + 
  autolayer(trips_month_northhollywood.ts,series='North Hollywood') +
  autolayer(trips_month_westside.ts,series='Westside')+
  xlab("Time") + ylab("Trips") + ggtitle("Monthly Demand in different regions")



#############################################
#########################################

# Check average trip duration 
# -> Trips in Westside and North Hollywood have longer duration than trips in DTLA.

duration_monthly_dtla=summarise(
  group_by(dtla, new_startmonth),
  avg_duration = mean(duration))

duration_monthly_dtla.ts = ts(duration_monthly_dtla$avg_duration, 
                         start=c(2016,7), frequency=12)


duration_monthly_pa=summarise(group_by(pasadena, new_startmonth),
  avg_duration = mean(duration))

duration_monthly_pa.ts = ts(duration_monthly_pa$avg_duration, 
                         start=c(2016,7), frequency=12)

duration_monthly_northhollywood=summarise(group_by(northhollywood, new_startmonth),
  avg_duration = mean(duration))

duration_monthly_northhollywood.ts = ts(duration_monthly_northhollywood$avg_duration, 
                            start=c(2016,7), frequency=12)

duration_monthly_westside=summarise(group_by(westside, new_startmonth),
                                          avg_duration = mean(duration))
duration_monthly_westside.ts = ts(duration_monthly_westside$avg_duration, 
                                        start=c(2016,7), frequency=12)


autoplot(duration_monthly_pa.ts,series='Pasadena') + theme_bw() +
  autolayer(duration_monthly_dtla.ts,series='DTLA')+
  autolayer(duration_monthly_northhollywood.ts,series='North Hollywood') + 
  autolayer(duration_monthly_westside.ts,series='Westside')+
  xlab("Time") + ylab("Trips duration (mins)") + ggtitle("Average Trip Durations (monthly) in different regions")

#############################################
#########################################

# check monthly unique start station  
# -> DTLA has more unique start stations, and significantly more new stations were built starting 2018.
# -> We notice there was a sharp decrease of unique start station in Westside in 2019 3rd quarter.
# -> There are only few stations in Pasadena and Westside, which aligns with the demands

startstation_month_dtla=dtla %>% group_by(new_startmonth) %>%
  summarise(unique_startstaion = n_distinct(start_station_id))

startstation_month_dtla.ts = ts(startstation_month_dtla$unique_startstaion,
                           start=c(2016,7), frequency=12)

startstation_month_pasadena=pasadena %>% group_by(new_startmonth) %>%
  summarise(unique_startstaion = n_distinct(start_station_id))

startstation_month_pasadena.ts = ts(startstation_month_pasadena$unique_startstaion,
                                start=c(2016,7), frequency=12)
startstation_month_northhollywood=northhollywood %>% group_by(new_startmonth) %>%
  summarise(unique_startstaion = n_distinct(start_station_id))

startstation_month_northhollywood.ts = ts(startstation_month_northhollywood$unique_startstaion,
                                    start=c(2016,7), frequency=12)

startstation_month_westside=westside %>% group_by(new_startmonth) %>%
  summarise(unique_startstaion = n_distinct(start_station_id))

startstation_month_westside.ts = ts(startstation_month_westside$unique_startstaion,
                                         start=c(2016,7), frequency=12)


autoplot(startstation_month_dtla.ts,series='DTLA') + theme_bw() + 
  autolayer(startstation_month_pasadena.ts,series='Pasadena')+
  autolayer(startstation_month_northhollywood.ts,series='North Hollywood') + 
  autolayer(startstation_month_westside.ts,series='Westside')+
  labs( y="Unique start staions", title="Monthly unique start stations")+
  xlab("Time")  + ggtitle("Unique start stations (monthly) in different regions")


