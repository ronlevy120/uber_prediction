######## Final Project - Forecasting Uber rentals in NYSE area, September 2014 ########
###Lahav Ezer & Ron Levy###
####### PART 1 #######
#1. Rearrangment of our data set
setwd("~/Downloads/uber_train")
uber_train_jul <- read.csv("uber-raw-data-jul14.csv", sep = ",")
uber_train_aug <- read.csv("uber-raw-data-aug14.csv", sep = ",")
uber_train_sep2w <- read.csv("uber-raw-data-sep14-first-2-weeks.csv", sep = ",")

# Adding the Wall St. coardinates to the monthly data sets
uber_train_jul$center_lat <- 40.706913
uber_train_aug$center_lat <- 40.706913
uber_train_sep2w$center_lat <- 40.706913
uber_train_jul$center_lon <- -74.01132
uber_train_aug$center_lon <- -74.01132
uber_train_sep2w$center_lon <- -74.01132

# Calculating the distance between each Uber order to the center point
library(geosphere)
library(dplyr)
library(lubridate)
uber_train_jul$Dist = distHaversine(uber_train_jul[,2:3],uber_train_jul[,5:6])
uber_train_aug$Dist = distHaversine(uber_train_aug[,2:3],uber_train_aug[,5:6]) 
uber_train_sep2w$Dist = distHaversine(uber_train_sep2w[,2:3],uber_train_sep2w[,5:6]) 

# Clearing all the results which are farther than 1000m from the center point
uber_train_jul <- uber_train_jul[uber_train_jul$Dist<=1000,]
uber_train_aug <- uber_train_aug[uber_train_aug$Dist<=1000,]
uber_train_sep2w <- uber_train_sep2w[uber_train_sep2w$Dist<=1000,]

# Changing our train data set to match our test data set
uber_train_jul$Date.Time=as.POSIXct(uber_train_jul$Date.Time,tz="America/New_York",format = "%Y-%m-%d%H:%M:%S")
uber_train_jul$quart_time <- lubridate::floor_date(uber_train_jul$Date.Time, unit= "15 minutes")
uber_train_aug$Date.Time=as.POSIXct(uber_train_aug$Date.Time,tz="America/New_York",format = "%Y-%m-%d%H:%M:%S")
uber_train_aug$quart_time <- lubridate::floor_date(uber_train_aug$Date.Time, unit= "15 minutes")
uber_train_sep2w$Date.Time=as.POSIXct(uber_train_sep2w$Date.Time,tz="America/New_York",format = "%Y-%m-%d%H:%M:%S")
uber_train_sep2w$quart_time <- lubridate::floor_date(uber_train_sep2w$Date.Time, unit= "15 minutes")


uber_train_aug <- uber_train_aug %>%
  group_by(quart_time) %>%summarize(Count = n())
uber_train_jul <- uber_train_jul %>%
  group_by(quart_time) %>%summarize(Count = n())
uber_train_sep2w <- uber_train_sep2w %>%
  group_by(quart_time) %>%summarize(Count = n())

colnames(uber_train_aug)[1] = "Time_Interval"
colnames(uber_train_jul)[1] = "Time_Interval"
colnames(uber_train_sep2w)[1] = "Time_Interval"

# Merging all three data sets into 1 data set
uber_train <- rbind(uber_train_jul, uber_train_aug, uber_train_sep2w)
colnames(uber_train)[2] = "number_of_pickups"

# Adding Weekdays and Weekends to our data table
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
uber_train$wDay <- factor((weekdays(uber_train$Time_Interval) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))


# Defining the holidays in our time period (4th of July and Labor Day) as weekends
uber_train$new_date <- format(uber_train$Time_Interval, "%Y%m%d")
for (i in 1:nrow(uber_train)){
  if(uber_train[i, 4]=="20140704"){
    uber_train[i, 3] ="weekend"
  }
  if(uber_train[i, 4] =="20140901"){
    uber_train[i, 3] = "weekend"
  }
  
}

uber_train$new_date <- NULL
# Adding External Data

# Bike Rental Data
bikes_jul <- read.csv("2014-07 - Citi Bike trip data.csv", sep = ",")
bikes_aug <- read.csv("2014-08 - Citi Bike trip data.csv", sep = ",")
bikes_sep <- read.csv("201409-citibike-tripdata.csv", sep = ",")

# July
bikes_jul$center_lat <- 40.706913
bikes_jul$center_lon <- -74.01132
bikes_jul$Dist = distHaversine(bikes_jul[,6:7],bikes_jul[,16:17])
bikes_jul <- bikes_jul[bikes_jul$Dist<=1000,]
bikes_jul$starttime=as.POSIXct(bikes_jul$starttime,tz="America/New_York",format = "%Y-%m-%d%H:%M:%S")
bikes_jul$quart_time <- lubridate::floor_date(bikes_jul$starttime, unit= "15 minutes")
bikes_jul <- bikes_jul %>%
  group_by(quart_time) %>%summarize(Count = n())

# August
bikes_aug$center_lat <- 40.706913
bikes_aug$center_lon <- -74.01132
bikes_aug$Dist = distHaversine(bikes_aug[,6:7],bikes_aug[,16:17])
bikes_aug <- bikes_aug[bikes_aug$Dist<=1000,]
bikes_aug$starttime=as.POSIXct(bikes_aug$starttime,tz="America/New_York",format = "%Y-%m-%d%H:%M:%S")
bikes_aug$quart_time <- lubridate::floor_date(bikes_aug$starttime, unit= "15 minutes")
bikes_aug <- bikes_aug %>%
  group_by(quart_time) %>%summarize(Count = n())

# September
bikes_sep$center_lat <- 40.706913
bikes_sep$center_lon <- -74.01132
bikes_sep$Dist = distHaversine(bikes_sep[,6:7],bikes_sep[,16:17])
bikes_sep <- bikes_sep[bikes_sep$Dist<=1000,]
bikes_sep$starttime = mdy_hms(bikes_sep$starttime, tz = "America/New_York")
bikes_sep$starttime=as.POSIXct(bikes_sep$starttime,tz="America/New_York",format = "%Y-%m-%d%H:%M:%S")
bikes_sep$quart_time <- lubridate::floor_date(bikes_sep$starttime, unit= "15 minutes")
bikes_sep <- bikes_sep %>%
  group_by(quart_time) %>%summarize(Count = n())
lastday <- as.POSIXct("2014-09-17 00:00:00",format="%Y-%m-%d%H:%M:%S", tz = "America/New_York")
bikes_sep <- bikes_sep[bikes_sep$quart_time < lastday,]

# Merging Bikes data to our Uber rentals dataset
bikes_train <- rbind(bikes_jul, bikes_aug, bikes_sep)
mean(bikes_train$Count)
colnames(bikes_train)[1] = "Time_Interval"
uber_train <- left_join(uber_train,bikes_train, by = "Time_Interval")
colnames(uber_train)[4] = "bikes_pickups"

replace.na=function(vec){
  vec[is.na(vec)]=mean(vec,na.rm = T)
  return(vec)
}
uber_train$bikes_pickups <- replace.na(uber_train$bikes_pickups)
uber_train$bikes_pickups <- round(uber_train$bikes_pickups, 0)


###### Rain Forecast Data ######
weather1 <- read.csv("weather_description.csv", sep = ",")
weather1[ ,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)] <- list(NULL)
weather1[ ,c(3,4,5,6,7,8,9,10)] <- list(NULL)

weather1$datetime = as.POSIXct(weather1$datetime, tz ="America/New_York" ,format = c("%Y-%m-%d%H:%M:%S"))
firstday <- as.POSIXct("2014-09-16 00:00:00",format="%Y-%m-%d%H:%M:%S", tz = "America/New_York")
lastday <- as.POSIXct("2014-09-30 00:00:00",format="%Y-%m-%d%H:%M:%S", tz = "America/New_York")
weather1 <- weather1[weather1$datetime >= firstday,]
weather1 <- weather1[weather1$datetime <= lastday,]
weather1$na_time <- is.na(weather1$datetime)
weather1$na_data <- is.na(weather1$New.York)
summary(weather1$na_time)
rows_to_keep = c(weather1$na_time == FALSE)
weather1 <- weather1[rows_to_keep,]
weather1$na_time <- NULL
weather1$na_data <-NULL
colnames(weather1)[2] = "Rain"
levels(weather1$Rain)
library(plyr)

weather1$Rain <- revalue(weather1$Rain, c("broken clouds" = 0, "drizzle" = 1, "dust" = 0,
                                          "few clouds" = 0,"fog" = 0, "freezing rain" = 1,
                            "haze" = 0, "heavy intensity drizzle" = 1, "heavy intensity rain" = 1,
                            "heavy snow" = 0, "heavy thunderstorm" = 1,
                            "light intensity drizzle" = 1, "light intensity shower rain" = 1,
                            "light rain" = 1, "light rain and snow" = 1,
                            "light snow" = 1, "mist" = 0, "moderate rain" = 1, 
                            "overcast clouds" = 0, "proximity thunderstorm" = 1,
                            "proximity thunderstorm with drizzle" = 1,
                            "proximity thunderstorm with rain" = 1, "sand" = 0,
                            "sand/dust whirls" = 0, "scattered clouds" = 0,
                            "shower rain" = 1, "sky is clear" = 0, "smoke" = 0,
                            "snow" = 1, "squalls" = 1, "thunderstorm" = 1, 
                            "thunderstorm with heavy rain" = 1, "thunderstorm with light drizzle" = 1,
                            "thunderstorm with light rain" = 1, "thunderstorm with rain" = 1, "very heavy rain" = 1))

colnames(weather1)[1] = "Time_Interval"
uber_t$Time_Interval <- as.POSIXct(uber_train$Time_Interval, tz ="America/New_York" ,format = c("%Y-%m-%d%H:%M:%S"))
uber_train$hour_interval <- lubridate::floor_date(uber_train$Time_Interval, unit= "60 minutes")
colnames(weather1)[1] = "hour_interval"

uber_train <- left_join(uber_train, weather1, by = "hour_interval")
uber_train$hour_interval <- NULL
uber_train$Subway.Count <- NULL
uber_train$X.1 <- NULL
uber_train$X <- NULL
uber_train$Rain <- factor(uber_train$Rain)
# Rain Data attached to our Uber Train Data

##### Part 2 - Descriptive Data #####

###uber_train summary##
summary(uber_train)

####how many days of rain. 0 means no rain####
by(uber_train$number_of_pickups, uber_train$Rain,sum)

### the correlation between bikes count and uber count##
cor(uber_train$number_of_pickups, uber_train$bikes_pickups)


####האם יש הבדל בין יום חול לסופש?####
uber_train$wday_bin = ifelse(uber_train$wDay=="weekend", 0,1)
t.test(number_of_pickups~wday_bin,uber_train)

#### ההבדל בין יום חול לסופש מובהק####

#####create weekend data###
uber_train_weekend = uber_train[which(uber_train$wDay=='weekend'),]

####create weekday data####
uber_train_weekday = uber_train[which(uber_train$wDay=='weekday'),]


####השכרות אופניים לפי שעה###
bikes= rbind(by(uber_train_weekday$bikes_pickups, uber_train_weekday$hour, mean),
             by(uber_train_weekend$bikes_pickups, uber_train_weekend$hour, mean))

barplot(bikes,beside=T,col=c("green"
                             ,"blue"), main = "השכרות אופניים לפי שעה")
legend("topleft",c("יום חול", "סופש"),
       fill = c("green","blue"))


######שימוש לפי שעה- uber#####
uber_use= rbind(by(uber_train_weekday$number_of_pickups, uber_train_weekday$hour, mean),
                by(uber_train_weekend$number_of_pickups, uber_train_weekend$hour, mean))

barplot(uber_use,beside=T,col=c("green"
                                ,"blue"), main = "uber- שימוש לפי שעה")
legend("topleft",c("יום חול", "סופש"),
       fill = c("green","blue"))



library(lubridate)
###### Part 3 - Model estimation ######
uber_train <- read.csv("uber_train.csv", sep = ",")
uber_train$X <- NULL
#### We will now check few optional models with our variables 
uber_train$hour <- format(as.POSIXct(uber_train$Time_Interval), format = "%H:%M")
set.seed(101)
sample <- sample.int(n = nrow(uber_train), size = floor(0.7*nrow(uber_train)), replace = F)
uber_train_t <- uber_train[sample, ]
uber_train_v <- uber_train[-sample, ]

# Model 1 #
model1 <- lm(number_of_pickups~factor(hour)+
               wDay+
               bikes_pickups+
               Rain, uber_train_t
               )
summary(model1)

# Model 2 #
model2 <- lm(number_of_pickups~factor(hour)*wDay+
               factor(hour)+
               wDay+
               bikes_pickups+
               Rain, uber_train_t
)
summary(model2)

# Model 2.3 #
model2.3 <- lm(number_of_pickups~factor(hour)*wDay+
               factor(hour)+
               wDay+
               Rain, uber_train_t
)
summary(model2.3)


# Model 3 #
model3 <- lm(number_of_pickups~factor(hour)+
               wDay+
               bikes_pickups^2+
               bikes_pickups+
               Rain, uber_train_t
)
summary(model3) # No change from first Model

# Model 4 #
model4 <- lm(number_of_pickups~hour(Time_Interval)+
               wDay+
               bikes_pickups+
               Rain, uber_train_t
)
summary(model4)

#It seems that models #1, #3 and #4 give us the same R-squared, therefore we will check accuracy beween models #1 and #2 which.
uber_train_v1 <- uber_train_v
uber_train_v2 <- uber_train_v

uber_train_v1$predictTest = predict(model1, newdata=uber_train_v1)
uber_train_v2$predictTest = predict(model2, newdata=uber_train_v2)

uber_train_v1$predictTest[uber_train_v1$predictTest<=0] <- 0
uber_train_v1$predictTest <- round(uber_train_v1$predictTest)

uber_train_v2$predictTest[uber_train_v2$predictTest<=0] <- 0
uber_train_v2$predictTest <- round(uber_train_v2$predictTest)

# After completeing our prediction with both models,
#on two different validate sets, we will check the accuracy of each model
mse_val1 <- mean((uber_train_v1$number_of_pickups-uber_train_v1$predictTest)^2)
mse_val2 <- mean((uber_train_v2$number_of_pickups-uber_train_v2$predictTest)^2)

rmse1 <- sqrt(mse_val1)
rmse2 <- sqrt(mse_val2)

mean1 <- mean(uber_train_v1$number_of_pickups)
mean2 <- mean(uber_train_v2$number_of_pickups)

# Accuracy  = 1 - (RMSE /  Mean hourly demand)
acc1 <- (1-(rmse1/mean1))
acc2 <- (1-(rmse2/mean2))

acc1
acc2

# We would also like to check log models inorder to avoid negative results in our data
# Model 1.2 #
model1.2 <- lm(log(number_of_pickups)~factor(hour)+
               wDay+
               bikes_pickups+
               Rain, uber_train_t
)
summary(model1.2)

# Model 2.2 #
model2.2 <- lm(log(number_of_pickups)~factor(hour)*wDay+
               factor(hour)+
               wDay+
               bikes_pickups+
               Rain, uber_train_t
)
summary(model2.2)

uber_train_v1.2 <- uber_train_v
uber_train_v2.2 <- uber_train_v

uber_train_v1.2$predictTest = predict(model1.2, newdata=uber_train_v1.2)
uber_train_v2.2$predictTest = predict(model2.2, newdata=uber_train_v2.2)

uber_train_v1.2$predictTest <- exp(uber_train_v1.2$predictTest)
uber_train_v2.2$predictTest <- exp(uber_train_v2.2$predictTest)

mse_val1.2 <- mean((uber_train_v1.2$number_of_pickups-uber_train_v1.2$predictTest)^2)
mse_val2.2 <- mean((uber_train_v2.2$number_of_pickups-uber_train_v2.2$predictTest)^2)

rmse1.2 <- sqrt(mse_val1.2)
rmse2.2 <- sqrt(mse_val2.2)

mean1.2 <- mean(uber_train_v1.2$number_of_pickups)
mean2.2 <- mean(uber_train_v2.2$number_of_pickups)

acc1.2 <- (1-(rmse1.2/mean1.2))
acc2.2 <- (1-(rmse2.2/mean2.2))

acc1
acc1.2

acc2
acc2.2

# We would also like to check another model which is
#not including our bike pick up data

model2.3 <- lm(number_of_pickups~factor(hour)*wDay+
                 factor(hour)+
                 wDay+
                 Rain, uber_train_t
)
summary(model2.3)

uber_train_v2.3 <- uber_train_v


uber_train_v2.3$predictTest = predict(model2.3, newdata=uber_train_v2.3)
uber_train_v2.3$predictTest[uber_train_v2.3$predictTest<=0] <- 0
uber_train_v2.3$predictTest <- round(uber_train_v2.3$predictTest)

mse_val2.3 <- mean((uber_train_v2.3$number_of_pickups-uber_train_v2.3$predictTest)^2)

rmse2.3 <- sqrt(mse_val2.3)

mean2.3 <- mean(uber_train_v2.3$number_of_pickups)

acc2.3 <- (1-(rmse2.3/mean2.3))

acc2.3

uber_test <- read.csv("uber_test.csv", sep = ",")
bike = read.csv("bike.csv")
uber_test$Time_Interval = as.POSIXct(uber_test$Time_Interval, tz ="America/New_York" ,format = c("%Y-%m-%d%H:%M:%S"))

# Adding Weekdays and Weekends to our data test table
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
uber_test$wDay <- factor((weekdays(uber_test$Time_Interval) %in% weekdays1), 
                          levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))


uber_train$new_date <- NULL

# Addint Rain forecast data for the test dataset dates
weather1 = read.csv( "weather_description.csv")
colnames(weather1)[1] = "hour_interval"
firstdaytest <- as.POSIXct("2014-09-24 00:00:00",format="%Y-%m-%d%H:%M:%S", tz = "America/New_York")
lastdaytest <- as.POSIXct("2014-09-30 00:00:00",format="%Y-%m-%d%H:%M:%S", tz = "America/New_York")
weather1 = weather1[, c("hour_interval", "New.York")]
weather1 <- weather1[as.POSIXct(weather1$hour_interval) >= firstdaytest,]
weather1 <- weather1[as.POSIXct(weather1$hour_interval) <= lastdaytest,]
colnames(weather1)[1] = "Time_Interval"
weather1$Time_Interval <- as.POSIXct(weather1$Time_Interval, tz ="America/New_York" ,format = c("%Y-%m-%d%H:%M:%S"))
weather1$quart_time <- lubridate::floor_date(weather1$Time_Interval, unit= "15 minutes")
library(lubridate)
weather1 <- weather1 %>%
  group_by(quart_time)
uber_test$hour_interval <- lubridate::floor_date(uber_test$Time_Interval, unit= "15 minutes")
library(tidyverse)
uber_test <- tibble::rowid_to_column(uber_test, "ID")
weather1 = tibble::rowid_to_column(weather1, "ID")
uber_test<- merge(uber_test, weather1, by = "ID")
uber_test = uber_test[,c("hour_interval.y", "Rain")]
colnames(uber_test)[1] ="Time_Interval"
# Setting the prediction 
uber_test$Rain = as.numeric(uber_test$Rain)
uber_test$hour <- format(as.POSIXct(uber_test$Time_Interval), format = "%H:%M")
uber_test$predictTest = predict(model2.3, newdata = uber_test)
colnames(uber_test)[5]= "number_of_pickups"
uber_test = uber_test[,c("Time_Interval", "number_of_pickups")]
write.csv(uber_test,"uber_test.csv")
#### we chose this model- number 2###

model2 <- lm(number_of_pickups~factor(hour)*wDay+
               factor(hour)+
               wDay+
               bikes_pickups+
               Rain, uber_train_t
)
summary(model2)
