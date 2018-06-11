
library(lubridate)

weather <- read.csv("data/first_weather_dat.csv")

require(chron)

weather$STATION_NAME
weather$DATE
weather$HOURLYDRYBULBTEMPC


weather$DATE<- mdy_hm(weather$DATE,format = "%m/%d/%Y %H:%M")
tm2.c <- as.chron(weather$DATE,format = "%m/%d/%Y %H:%M")

?as.POSIXct

x <- c("2010-04-14-04-35-59", "2010-04-01-12-00-00")
ymd_hms(x)
x <- c("2011-12-31 12:59:59", "2010-01-01 12:00:00")
ymd_hms(x)

