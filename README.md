# **Taxi Trip Duration**
*Predict New York City Taxi Trip Duration*

## Table of contents

- [Introduction](#introduction)
- [Preparation](#preparation)
- [Prediction](#prediction)



## Introduction
This is a competion that gives me a chance to win 30,000 USD if I be in a first place. There is no missing values; however, there are more than 2million observations so it seems like data cleaning will play pivotal role in creating robust model.
I will create new variables and use XGBOOST to predict train data set.

## Preparation
#### Initial works
```
library(caret)
library(dplyr)
library(lubridate)
library(xgboost)
library(hydroGOF)
library(ggplot2)
library(geosphere)
library(tibble)
library(Matrix)
library(data.table)
library(Metrics)
```
```
setwd('c:/kaggle/taxi')
#retrieve train and test
train <- read.csv('train.csv', na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv('test.csv', na.strings = c("", "NA"), stringsAsFactors = F)
#combine train and test
total <- bind_rows(train, test)

#check duplicate
nrow(train) - nrow(unique(train))
```
There are no duplicates.

#### Create new variables
We have pickup locations and dropoff locations. I will calculate actual distance from this information.

```
total <- mutate(total, dist= distHaversine(matrix(c(pickup_longitude, pickup_latitude), ncol = 2),matrix(c(dropoff_longitude,dropoff_latitude), ncol = 2))/1000)
```
From the pickup datetime column, I can retrive useful information. I will create some columns from the datetime information.
```
#Create pickup_hour column
total$pickup_hour <- hour(total$pickup_datetime)
#Create pickup_week column
total$pickup_week <- week(total$pickup_datetime)
#Create pickup_month column
total$pickup_month <- month(total$pickup_datetime)
#Create pickup_weekdays column
total$pickup_weekdays <- weekdays(as.Date(total$pickup_datetime))
```

Area should play important role in the trip duration. I will devide New York City into 100 different area and I will distribute this almost equally.
```
#Devide area with pickup longitude
total$plong <- total$pickup_longitude
total$plong[total$pickup_longitude < quantile(total$pickup_longitude,0.1)]<- 'a'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.1) & total$pickup_longitude < quantile(total$pickup_longitude,0.2)]<- 'b'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.2) & total$pickup_longitude < quantile(total$pickup_longitude,0.3)]<- 'c'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.3) & total$pickup_longitude < quantile(total$pickup_longitude,0.4)]<- 'd'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.4) & total$pickup_longitude < quantile(total$pickup_longitude,0.5)]<- 'e'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.5) & total$pickup_longitude < quantile(total$pickup_longitude,0.6)]<- 'f'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.6) & total$pickup_longitude < quantile(total$pickup_longitude,0.7)]<- 'g'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.7) & total$pickup_longitude < quantile(total$pickup_longitude,0.8)]<- 'h'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.8) & total$pickup_longitude < quantile(total$pickup_longitude,0.9)]<- 'i'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.9) & total$pickup_longitude < quantile(total$pickup_longitude,1)]<- 'j'

#Devide area with pickup latitude
total$plat <- total$pickup_latitude
total$plat[total$pickup_latitude < quantile(total$pickup_latitude,0.1)]<- '1'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.1) & total$pickup_latitude < quantile(total$pickup_latitude,0.2)]<- '2'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.2) & total$pickup_latitude < quantile(total$pickup_latitude,0.3)]<- '3'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.3) & total$pickup_latitude < quantile(total$pickup_latitude,0.4)]<- '4'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.4) & total$pickup_latitude < quantile(total$pickup_latitude,0.5)]<- '5'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.5) & total$pickup_latitude < quantile(total$pickup_latitude,0.6)]<- '6'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.6) & total$pickup_latitude < quantile(total$pickup_latitude,0.7)]<- '7'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.7) & total$pickup_latitude < quantile(total$pickup_latitude,0.8)]<- '8'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.8) & total$pickup_latitude < quantile(total$pickup_latitude,0.9)]<- '9'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.9) & total$pickup_latitude < quantile(total$pickup_latitude,1)]<- '10'

#Devide area with dropoff longitude
total$dlong <- total$dropoff_longitude
total$dlong[total$dropoff_longitude < quantile(total$dropoff_longitude,0.1)]<- 'a'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.1) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.2)]<- 'b'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.2) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.3)]<- 'c'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.3) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.4)]<- 'd'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.4) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.5)]<- 'e'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.5) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.6)]<- 'f'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.6) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.7)]<- 'g'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.7) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.8)]<- 'h'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.8) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.9)]<- 'i'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.9) & total$dropoff_longitude < quantile(total$dropoff_longitude,1)]<- 'j'

#Devide area with dropoff latitude
total$dlat <- total$dropoff_latitude
total$dlat[total$dropoff_latitude < quantile(total$dropoff_latitude,0.1)]<- '1'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.1) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.2)]<- '2'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.2) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.3)]<- '3'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.3) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.4)]<- '4'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.4) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.5)]<- '5'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.5) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.6)]<- '6'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.6) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.7)]<- '7'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.7) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.8)]<- '8'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.8) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.9)]<- '9'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.9) & total$dropoff_latitude < quantile(total$dropoff_latitude,1)]<- '10'
```
I will change strings to factors
```
total <- as.data.frame(unclass(total))
```
Finally I will combine those 4 columns(plong,  plat, dlong, dlat) into one column so that new column will contain travel information.
```
total$travel <- with(total, interaction(plong,  plat, dlong, dlat))
```

Now, I will separate them back to train and test data set.
```
train <- total[1:1458644,]
test <- total[1458645:2083778,]
```

#### Outlier handling
I need a visualization to see outliers.
```
train %>%
  ggplot(aes(dist, trip_duration)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")
```
![Alt text](https://github.com/ur4me/Taxi-Trip-Duration/blob/master/Duration%20vs%20distance.png)

I can see that there are many outliers near distance = 0 and trip duration = 1e+05. I will remove those outliers
```
#remove dist outliers
upper_outlier <- quantile(train$dist,0.99)
lower_outlier <- quantile(train$dist,0.01) 
index_outlier <- which(lower_outlier > train$dist, upper_outlier < train$dist)
train <- train[-index_outlier,]

#remove trip_duration outliers
upper_outlier <- quantile(train$trip_duration,0.98)
lower_outlier <- quantile(train$trip_duration,0.01) 
index_outlier <- which(lower_outlier > train$trip_duration, upper_outlier < train$trip_duration)
train <- train[-index_outlier,]
```

I will remove outliers in the longitude and latitude columns for robust model.
```
#remove pickup_longitude outliers
upper_outlier <- quantile(train$pickup_longitude,0.99)
lower_outlier <- quantile(train$pickup_longitude,0.01) 
index_outlier <- which(lower_outlier > train$pickup_longitude, upper_outlier < train$pickup_longitude)
train <- train[-index_outlier,]

#remove pickup_latitude outliers
upper_outlier <- quantile(train$pickup_latitude,0.99)
lower_outlier <- quantile(train$pickup_latitude,0.01) 
index_outlier <- which(lower_outlier > train$pickup_latitude, upper_outlier < train$pickup_latitude)
train <- train[-index_outlier,]

#remove dropoff_longitude outliers
upper_outlier <- quantile(train$dropoff_longitude,0.99)
lower_outlier <- quantile(train$dropoff_longitude,0.01) 
index_outlier <- which(lower_outlier > train$dropoff_longitude, upper_outlier < train$dropoff_longitude)
train <- train[-index_outlier,]

#remove dropoff_latitude outliers
upper_outlier <- quantile(train$dropoff_latitude,0.99)
lower_outlier <- quantile(train$dropoff_latitude,0.01) 
index_outlier <- which(lower_outlier > train$dropoff_latitude, upper_outlier < train$dropoff_latitude)
train <- train[-index_outlier,]
```

I will reduce variables.
```

train1 <- train %>% select(vendor_id, passenger_count, store_and_fwd_flag, trip_duration, dist, pickup_hour, pickup_week, pickup_month, pickup_weekdays, travel)

test1 <- test %>% select(id, vendor_id, passenger_count, store_and_fwd_flag, dist, pickup_hour, pickup_week, pickup_month, pickup_weekdays, travel)
```

## Prediction
I will predict the test data using XGBOOST. Before the prediction, I need to do some preparations.


```
#preparations
train1[] <- lapply(train1, as.numeric)
test1[]<-lapply(test1, as.numeric)

foo <- train1 %>% select(-trip_duration)

dtrain <- xgb.DMatrix(as.matrix(foo),label = train1$trip_duration)
dtest <- xgb.DMatrix(as.matrix(test1))

```


```
#xgboost parameters
xgb_params <- list(colsample_bytree = 0.5, #variables per tree 
                   subsample = 0.8, #data subset per tree 
                   booster = "gbtree",
                   max_depth = 12, #tree levels
                   eta = 0.02, #shrinkage
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   seed = 4321
```
```
#checking iterations
set.seed(4321)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 5, nrounds=200)
```
61 was my best iteration.



#### Predict and save
```
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   print_every_n = 5,
                   nrounds = 61)

```
```
prediction <- predict(gb_dt,dtest)
solution <- data.frame(id = test$id, trip_duration = prediction)
write.csv(solution, file = 'xgb_Sol1.csv', row.names = F)
```

Finally, I will check which variables influenced the trip_duration values. 
```
#Check importance
imp_matrix <- as.tibble(xgb.importance(feature_names = colnames(train1 %>% select(-trip_duration)), model = gb_dt))

imp_matrix %>%
  ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Features", y = "Importance")
```
![Alt text](https://github.com/ur4me/Taxi-Trip-Duration/blob/master/Importance.png)
