# **Taxi Trip Duration version2**
*Predict New York City Taxi Trip Duration*

## Table of contents

- [Introduction](#introduction)
- [Preparation](#preparation)
- [Prediction](#prediction)
- [Conclusion](#conclusion)



## Introduction
My score for previous work was 0.57034 (RMSLE) and I wanted to improve my score. This time I used Open Source Routing Machine called OSRM to get useful variables. The data set is provided by Oscalreo and it contains important information such as estimated shortest distance and duration between two points and sequence of travels steps such as turns or entering a highway. Most of the process is similar to previous work.

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



#### Retrieve data from Open Source Routing Machine called OSRM
```
ftrain1 <- as.tibble(fread('fastest_routes_train_part_1.csv', na.strings = c("", "NA"), stringsAsFactors = F))
ftrain2 <- as.tibble(fread('fastest_routes_train_part_2.csv', na.strings = c("", "NA"), stringsAsFactors = F))
ftrain <- bind_rows(ftrain1, ftrain2)
ftest <- as.tibble(fread('fastest_routes_test.csv', na.strings = c("", "NA"), stringsAsFactors = F))
```
I need to use merge function which is similar to VLOOKUP in excel in order to impute right data
```
train1 <- merge(train, ftrain[, c(1, 4, 5, 6)], by = "id", all.x = T, sort = F)
test1 <- merge(test, ftest[, c(1, 4, 5, 6)], by = "id", all.x = T, sort = F)
```
```
#combine train and test
total <- bind_rows(train1, test1)

#check duplicate
nrow(total) - nrow(unique(total))
```
There are no duplicates.

#### Create new variables
We have pickup locations and dropoff locations. I will calculate actual distance from this information.

```
total <- mutate(total, dist= distHaversine(matrix(c(pickup_longitude, pickup_latitude), ncol = 2),matrix(c(dropoff_longitude,dropoff_latitude), ncol = 2))/1000)
```
I can retrive useful information from the pickup datetime column.  I will create some new columns from the datetime information.
```
#Create pickup_hour column
total$pickup_hour <- hour(total$pickup_datetime)
#Create pickup_week column
total$pickup_week <- week(total$pickup_datetime)
#Create pickup_month column
total$pickup_month <- month(total$pickup_datetime)
#Create pickup_days column
total$pickup_days <- weekdays(as.Date(total$pickup_datetime))
```

Area should play important role in the trip duration. I will devide New York City into 100 different areas.
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
I will change strings to factors.
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

I will remove outliers in the longitude and latitude columns in order to make robust model.
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

I will drop off some variables that are no longer needed.
```
#remove some variables
train1 <- train %>% select(vendor_id, passenger_count, dist, trip_duration, pickup_hour, pickup_week, pickup_month, pickup_days, travel, total_distance, total_travel_time, number_of_steps)

test1 <- test %>% select(vendor_id, passenger_count, dist, pickup_hour, pickup_week, pickup_month, pickup_days, travel, total_distance, total_travel_time, number_of_steps)

```
```
#Find NA rows that happened when I merged OSRM data
train1[which(is.na(train1$total_distance)),]
```
I found out that there are some missing values in row 1458644.
```
#remove row 1458644
train1 <- train1[-1458644,]
```

## Prediction
I will predict the test data using XGBOOST. Before the prediction, I need to do some preparations.


```
#preparations
train1[] <- lapply(train1, as.numeric)
test1[]<-lapply(test1, as.numeric)

#Change trip_duration as the evaluation metric for this competition is Root Mean Squared Logarithmic Error.
train1 <- train1 %>% mutate(trip_duration = log(trip_duration + 1))
```

I will spit train1 in order to make robust model by checking RMSLE each time I put different parameters
```
#split train
set.seed(54321)
outcome <- train1$trip_duration

partition <- createDataPartition(y=outcome,
                                 p=.7,
                                 list=F)
training <- train1[partition,]
testing <- train1[-partition,]

#xgb matrix
withoutRV <- training %>% select(-trip_duration)
dtrain <- xgb.DMatrix(as.matrix(withoutRV),label = training$trip_duration)
dtest <- xgb.DMatrix(as.matrix(testing))
```


```
#xgboost parameters
#xgboost parameters
xgb_params <- list(colsample_bytree = 0.8, #variables per tree 
                   subsample = 0.8, #data subset per tree 
                   booster = "gbtree",
                   max_depth = 3, #tree levels
                   eta = 0.05, #shrinkage
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   gamma=0)
                 
```
```
#cross-validation and checking iterations
set.seed(4321)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 5, print_every_n = 5, nrounds=300, nthread=6)

```
61 was my best iteration. I played around with the figures in parameters by using confusion matrix but omitted to state here as it was quite long process. Above figures gave me the best accuracy so far but I need to keep working on it to make best model.

```
#### Real Prediction
#predict the model
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   verbose = 1, maximize =F,
                   nrounds = 300, nthread=6)

prediction <- predict(gb_dt,dtest)

#Check RMSE
rmse(testing$trip_duration, prediction)
```

#predict with real test data

withoutRV <- train1 %>% select(-trip_duration)

dtrain1 <- xgb.DMatrix(as.matrix(withoutRV),label = train1$trip_duration)
dtest1 <- xgb.DMatrix(as.matrix(test1))


gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain1,
                   verbose = 1, maximize =F,
                   nrounds = 210, nthread=6)

prediction <- predict(gb_dt,dtest1)
solution <- data.frame(id = test$id, trip_duration = exp(prediction)-1)

#check negative value
a <- which(solution$trip_duration < 0)

#save
write.csv(solution, file = 'xgb_Sol10.csv', row.names = F)
          
## Conclusion
This time I got 0.45701 RMSLE which is great improvement. Finally, I will check whether the variables from OSRM influenced the response variable (trip_duration). 
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

![Alt text](https://github.com/ur4me/Taxi-Trip-Duration/blob/master/importance2.png)

