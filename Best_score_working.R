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

setwd('c:/kaggle/taxi')
#retrieve train and test
train <- read.csv('train.csv', na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv('test.csv', na.strings = c("", "NA"), stringsAsFactors = F)


ftrain1 <- as.tibble(fread('fastest_routes_train_part_1.csv', na.strings = c("", "NA"), stringsAsFactors = F))
ftrain2 <- as.tibble(fread('fastest_routes_train_part_2.csv', na.strings = c("", "NA"), stringsAsFactors = F))
ftrain <- bind_rows(ftrain1, ftrain2)
ftest <- as.tibble(fread('fastest_routes_test.csv', na.strings = c("", "NA"), stringsAsFactors = F))


train1 <- merge(train, ftrain[, c(1, 4, 5, 6)], by = "id", all.x = T, sort = F)
test1 <- merge(test, ftest[, c(1, 4, 5, 6)], by = "id", all.x = T, sort = F)


#combine train and test
total <- bind_rows(train1, test1)

#check duplicate
nrow(total) - nrow(unique(total))


total <- mutate(total, dist= distHaversine(matrix(c(pickup_longitude, pickup_latitude), ncol = 2),matrix(c(dropoff_longitude,dropoff_latitude), ncol = 2))/1000)




#Create pickup_hour column
total$pickup_hour <- hour(total$pickup_datetime)
#Create pickup_week column
total$pickup_week <- week(total$pickup_datetime)
#Create pickup_month column
total$pickup_month <- month(total$pickup_datetime)
#Create pickup_dayname column
total$pickup_dayname <- weekdays(as.Date(total$pickup_datetime))
#Create pickup_days column
total$pickup_days <- day(total$pickup_datetime)
#combine pickup_days and pickup_month
total$date <- with(total, interaction(pickup_days, pickup_month))


#Devide area with pickup longitude
total$plong[total$pickup_longitude < quantile(total$pickup_longitude,0.1)]<- 'a'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.1) & total$pickup_longitude < quantile(total$pickup_longitude,0.2)]<- 'b'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.2) & total$pickup_longitude < quantile(total$pickup_longitude,0.3)]<- 'c'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.3) & total$pickup_longitude < quantile(total$pickup_longitude,0.4)]<- 'd'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.4) & total$pickup_longitude < quantile(total$pickup_longitude,0.5)]<- 'e'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.5) & total$pickup_longitude < quantile(total$pickup_longitude,0.6)]<- 'f'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.6) & total$pickup_longitude < quantile(total$pickup_longitude,0.7)]<- 'g'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.7) & total$pickup_longitude < quantile(total$pickup_longitude,0.8)]<- 'h'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.8) & total$pickup_longitude < quantile(total$pickup_longitude,0.9)]<- 'i'
total$plong[total$pickup_longitude >= quantile(total$pickup_longitude,0.9) & total$pickup_longitude <= quantile(total$pickup_longitude,1)]<- 'j'

#Devide area with pickup latitude
total$plat[total$pickup_latitude < quantile(total$pickup_latitude,0.1)]<- '1'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.1) & total$pickup_latitude < quantile(total$pickup_latitude,0.2)]<- '2'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.2) & total$pickup_latitude < quantile(total$pickup_latitude,0.3)]<- '3'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.3) & total$pickup_latitude < quantile(total$pickup_latitude,0.4)]<- '4'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.4) & total$pickup_latitude < quantile(total$pickup_latitude,0.5)]<- '5'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.5) & total$pickup_latitude < quantile(total$pickup_latitude,0.6)]<- '6'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.6) & total$pickup_latitude < quantile(total$pickup_latitude,0.7)]<- '7'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.7) & total$pickup_latitude < quantile(total$pickup_latitude,0.8)]<- '8'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.8) & total$pickup_latitude < quantile(total$pickup_latitude,0.9)]<- '9'
total$plat[total$pickup_latitude >= quantile(total$pickup_latitude,0.9) & total$pickup_latitude <= quantile(total$pickup_latitude,1)]<- '10'

#Devide area with dropoff longitude
total$dlong[total$dropoff_longitude < quantile(total$dropoff_longitude,0.1)]<- 'a'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.1) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.2)]<- 'b'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.2) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.3)]<- 'c'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.3) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.4)]<- 'd'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.4) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.5)]<- 'e'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.5) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.6)]<- 'f'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.6) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.7)]<- 'g'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.7) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.8)]<- 'h'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.8) & total$dropoff_longitude < quantile(total$dropoff_longitude,0.9)]<- 'i'
total$dlong[total$dropoff_longitude >= quantile(total$dropoff_longitude,0.9) & total$dropoff_longitude <= quantile(total$dropoff_longitude,1)]<- 'j'

#Devide area with dropoff latitude
total$dlat[total$dropoff_latitude < quantile(total$dropoff_latitude,0.1)]<- '1'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.1) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.2)]<- '2'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.2) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.3)]<- '3'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.3) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.4)]<- '4'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.4) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.5)]<- '5'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.5) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.6)]<- '6'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.6) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.7)]<- '7'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.7) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.8)]<- '8'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.8) & total$dropoff_latitude < quantile(total$dropoff_latitude,0.9)]<- '9'
total$dlat[total$dropoff_latitude >= quantile(total$dropoff_latitude,0.9) & total$dropoff_latitude <= quantile(total$dropoff_latitude,1)]<- '10'


total <- as.data.frame(unclass(total))

total$travel <- with(total, interaction(plong,  plat, dlong, dlat))
total$pickup_location <- with(total, interaction(plong,  plat))
total$dropoff_location <- with(total, interaction(dlong, dlat))


#bearing
pick_coord <- total %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- total %>%
  select(dropoff_longitude, dropoff_latitude)
total$bearing <- bearing(pick_coord, drop_coord)



train <- total[1:1458644,]
test <- total[1458645:2083778,]
train1 <-train
test1 <- test


#remove row 1458644
train1 <- train1[-1458644,]



#remove trip_duration outliers
plot(train1$trip_duration)
which(train1$trip_duration > 1500000)
train1 <- train1[-which(train1$trip_duration > 1500000),]


#remove dist outliers
plot(train1$dist)
which(train1$dist > 400)
train1 <- train1[-which(train1$dist > 400),]


#change to numeric
train1[] <- lapply(train1, as.numeric)
test1[]<-lapply(test1, as.numeric)


mod <- lm(trip_duration ~ ., data=train1)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


which(cooksd >0.02)
train1 <- train1[-c(114377, 275644, 377067, 644163, 900378),]

train2 <- train1
test2 <- test1

#remove some variables
train1 <- train2 %>% select(date, dist, trip_duration, pickup_hour, pickup_week, pickup_dayname, travel, total_distance, total_travel_time, number_of_steps, bearing, pickup_location, dropoff_location)

test1 <- test2 %>% select(date, dist, pickup_hour, pickup_week, pickup_dayname, travel, total_distance, total_travel_time, number_of_steps, bearing,pickup_location, dropoff_location)


#convert trip_duration
train1 <- train1 %>% mutate(trip_duration = log(trip_duration + 1))





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
withoutRV1 <- testing %>% select(-trip_duration)
dtest <- xgb.DMatrix(as.matrix(withoutRV1))





#xgboost parameters
xgb_params <- list(colsample_bytree = 0.7, #variables per tree 
                   subsample = 0.8, #data subset per tree 
                   booster = "gbtree",
                   max_depth = 10, #tree levels
                   eta = 0.12, #shrinkage
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   gamma=0)       


#cross-validation and checking iterations
set.seed(4321)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 3, print_every_n = 5, nrounds=1000)


#predict the model
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   verbose = 1, maximize =F,
                   nrounds = 192)

prediction <- predict(gb_dt,dtest)

#Check RMSE
rmse(testing$trip_duration, prediction)




#predict with real test data

withoutRV <- train1 %>% select(-trip_duration)

dtrain1 <- xgb.DMatrix(as.matrix(withoutRV),label = train1$trip_duration)
dtest1 <- xgb.DMatrix(as.matrix(test1))


#cross-validation and checking iterations
set.seed(4321)
xgb_cv <- xgb.cv(xgb_params,dtrain1,early_stopping_rounds = 10, nfold = 3, print_every_n = 5, nrounds=1000)



gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain1,
                   verbose = 1, maximize =F,
                   nrounds = 224)

prediction <- predict(gb_dt,dtest1)

#save the file (Need to use exp and -1 to change it back)
solution <- data.frame(id = test$id, trip_duration = exp(prediction)-1)

#check negative value just in case
which(solution$trip_duration < 0)

#save
write.csv(solution, file = 'xgb_Sol21.csv', row.names = F)


#Check importance
imp_matrix <- as.tibble(xgb.importance(feature_names = colnames(train1 %>% select(-trip_duration)), model = gb_dt))

imp_matrix %>%
  ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Features", y = "Importance")
