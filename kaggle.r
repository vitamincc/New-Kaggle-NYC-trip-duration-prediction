library(caret)
library(e1071)
library(AppliedPredictiveModeling)
library("lubridate")
library(geosphere)
library(corrplot)


data1 = read.csv("/Users/xucc/Documents/GMU/OR568/project/train.csv", header = TRUE)
data2 = read.csv("/Users/xucc/Documents/GMU/OR568/project/test.csv", header = TRUE)
data <- data1
rows <- nrow(data)

#too huge data, just select half of the dataset
#set.seed(1)
#index1 = sample(rows, 0.5*rows, replace = FALSE)
#data = data[index1,]

#calculate distance between pick-up point and drop-off point in miles
data$distance <- distHaversine(data[,6:7], data[,8:9], r=6378.137/1.6)
data$pickup_day <- as.Date(data$pickup_datetime,format='%Y-%m-%d')
data$pickup_weekday <- weekdays(as.Date(data$pickup_datetime,format='%Y-%m-%d'))

pickup_datetime = lubridate::ymd_hms(data$pickup_datetime)
pickup_time = format(pickup_datetime, '%H')
pickup_date = format(pickup_datetime, '%Y:%m:%d')

dropoff_datetime = lubridate::ymd_hms(data$dropoff_datetime,tz="America/New_york")
dropoff_time = format(dropoff_datetime, '%H')
dropoff_date = format(dropoff_datetime, '%Y:%m:%d')
pickup_weekday = weekdays(pickup_datetime)


data$pickup_time = pickup_time
data$pickup_date = pickup_date
data$dropoff_time = dropoff_time
data$dropoff_date = dropoff_date
data$pickup_weekday = pickup_weekday

data$Mon = ifelse(data$pickup_weekday == "Monday", 1, 0)
data$Tue = ifelse(data$pickup_weekday == "Tuesday", 1, 0)
data$Wed = ifelse(data$pickup_weekday == "Wednesday", 1, 0)
data$Thu = ifelse(data$pickup_weekday == "Thursday", 1, 0)
data$Fri = ifelse(data$pickup_weekday == "Friday", 1, 0)
data$Sat = ifelse(data$pickup_weekday == "Saturday", 1, 0)
data$Sun = ifelse(data$pickup_weekday == "Sunday", 1, 0)

#write.csv(data, "/Users/xucc/Documents/GMU/OR568/project/convertedData.csv",row.names = F)


#delete useless columns
new_data <- data
new_data$pickup_datetime <- NULL
new_data$dropoff_date <- NULL
new_data$dropoff_datetime <- NULL
new_data$store_and_fwd_flag <- NULL
new_data$dropoff_time <- NULL
new_data$pickup_latitude <- NULL
new_data$pickup_longitude <- NULL
new_data$dropoff_longitude <- NULL
new_data$dropoff_latitude <- NULL
new_data$pickup_date <- NULL
new_data$pickup_day <- NULL
head(new_data)

#rename columns name: 
colnames(new_data) <- c("Id","VendorId","PassengerNum","TripDuration","Distance",
              "PickupWeekday","PickupHour","Mon","Tue","Wed","Thu","Fri","Sat","Sun")
cols <- colnames(new_data)
head(new_data)

#write.csv(new_data, "/Users/xucc/Documents/GMU/OR568/project/firstCleanedData.csv",row.names = F)

#----------------------test data
#calculate distance between pick-up point and drop-off point in miles
data2$distance <- distHaversine(data2[,5:6], data2[,7:8], r=6378.137/1.6)
data2$pickup_day <- as.Date(data2$pickup_datetime,format='%Y-%m-%d')
data2$pickup_weekday <- weekdays(as.Date(data2$pickup_datetime,format='%Y-%m-%d'))

pickup_datetime = lubridate::ymd_hms(data2$pickup_datetime)
pickup_time = format(pickup_datetime, '%H')
pickup_date = format(pickup_datetime, '%Y:%m:%d')

dropoff_datetime = lubridate::ymd_hms(data2$dropoff_datetime,tz="America/New_york")
dropoff_time = format(dropoff_datetime, '%H')
dropoff_date = format(dropoff_datetime, '%Y:%m:%d')
pickup_weekday = weekdays(pickup_datetime)


data2$pickup_time = pickup_time
data2$pickup_date = pickup_date
data2$dropoff_time = dropoff_time
data2$dropoff_date = dropoff_date
data2$pickup_weekday = pickup_weekday

data2$Mon = ifelse(data2$pickup_weekday == "Monday", 1, 0)
data2$Tue = ifelse(data2$pickup_weekday == "Tuesday", 1, 0)
data2$Wed = ifelse(data2$pickup_weekday == "Wednesday", 1, 0)
data2$Thu = ifelse(data2$pickup_weekday == "Thursday", 1, 0)
data2$Fri = ifelse(data2$pickup_weekday == "Friday", 1, 0)
data2$Sat = ifelse(data2$pickup_weekday == "Saturday", 1, 0)
data2$Sun = ifelse(data2$pickup_weekday == "Sunday", 1, 0)

#write.csv(data, "/Users/xucc/Documents/GMU/OR568/project/convertedData.csv",row.names = F)


#delete useless columns
new_data2 <- data2
new_data2$pickup_datetime <- NULL
new_data2$dropoff_date <- NULL
new_data2$dropoff_datetime <- NULL
new_data2$store_and_fwd_flag <- NULL
new_data2$dropoff_time <- NULL
new_data2$pickup_latitude <- NULL
new_data2$pickup_longitude <- NULL
new_data2$dropoff_longitude <- NULL
new_data2$dropoff_latitude <- NULL
new_data2$pickup_date <- NULL
new_data2$pickup_day <- NULL
head(new_data2)

#rename columns name: 
colnames(new_data2) <- c("Id","VendorId","PassengerNum","Distance",
                        "PickupWeekday","PickupHour","Mon","Tue","Wed","Thu","Fri","Sat","Sun")
cols <- colnames(new_data2)
head(new_data2)




#--------------------box plot of some features----------------------------


#select features for visulization
myvals = c(1:3,5,7:14,4)
predData <- new_data[,myvals]
head(predData)

#options(digits=3)
#predData$pickup_time <- as.double(predData$pickup_time)



#------------------delete outliers-------------
attach(predData)
new_predData <- predData[which(TripDuration < 3600 & TripDuration > 120 & Distance < 60 
                               & Distance > 0.3 & PassengerNum <= 5),]


detach(predData)

xyplot(new_predData$Distance~ new_predData$TripDuration, type = c("p", "g"), 
       xlab = "Trip Duration", ylab = "Distance",main = "Distance VS Trip Duration")

#delete data with speed high than 60mile/h 
new_predData <- new_predData[which(new_predData$Distance/new_predData$TripDuration < 1/50), ]
xyplot(new_predData$Distance~ new_predData$TripDuration, type = c("p", "g"), 
       xlab = "Trip Duration", ylab = "Distance", main = "Distance VS Trip Duration")

write.csv(new_predData, "/Users/xucc/Documents/GMU/OR568/project/new_predData.csv",row.names = F)

#calculate skewness for numeric features
skewness(new_predData$PassengerNum)    #2.28
skewness(new_predData$Distance)        #2.91

histogram(log(new_predData$Distance), xlab = "Log Units", type = "count", 
          main = "Log Transform for Distance")

skewness(log(new_predData$Distance))


#hist(predData$Distance, main = "Histogram of Distance",breaks=c(10, 20, 30, 40))
hist(new_predData$PassengerNum, xlab = "Passenger Number", ylab = "Frequency", 
     col = "light blue", main = "Histogram of Passenger Number")
hist(new_predData$Distance, xlab = "Distance", ylab = "Frequency", xlim = c(0,20), 
     ylim = c(0, 500000), col = "light blue", main = "Histogram of Distance")

barplot(table(new_predData$PickupHour), xlab = "Hours", ylim = c(0, 50000),
        col = "light blue",main = "Frequency at Different Pick-up Hours")
barplot(table(new_predData$PickupWeekday), xlab = "Weekdays", col = "light blue",
        ylim = c(0, 120000), main = "Frequency at Different Weekdays")




#----------------split predData to X,y ----------
#convert pickuptime from string to number
options(digits=3)
new_predData$PickupHour <- as.double(new_predData$PickupHour)
new_data2$PickupHour <- as.double(new_data2$PickupHour)

new_predDataPP <- preProcess(new_predData, method = "BoxCox")
new_predDataTrans <- predict(new_predDataPP, new_predData)
new_predDataPP$bc$Distance #-0.3
histogram(log(new_predData$Distance), xlab = "Transformed Data", type = "count", 
          col = "light blue",main = "Boxcox Transform for Distance", tl.cex = 0.5)
skewness(log(new_predData$Distance)) #0.54
skewness(new_predData$Distance)
skewness(new_predDataTrans$Distance)
new_predDataTransX <- new_predDataTrans[, c(2:12)]
segCorr <- cor(new_predDataTransX)

corrplot(segCorr, order = "original", tl.cex = 1)
highCorr <- findCorrelation(segCorr, cutoff = .75)
highCorr # integer(0) , do not need to filter



set.seed(12345)
# rows <- nrow(new_predData)
# index = sample(rows, 0.8*rows, replace = FALSE)
# trainData = new_predData[index,]
# testData = new_predData[-index,]

trainData <- new_predData[,-1]
prediction <- 'TripDuration'
predictors <- names(trainData)[names(trainData) != prediction]
trainX <- trainData[, predictors]
trainY <- trainData[,prediction]

testData <- new_data2[,-1]
#testX<- testData[, predictors]
#testY<- testData[,13]



#---------------linear regression------------------
### Create a control function that will be used across models. 
## 10 folds CV
ctrl <- trainControl(method = "cv", number = 10)


library(car)
library(Metrics)
library(MASS)
#boxcox transform for the orginial data
new_trainData = trainData[,-1]  #delete ID column 
box = boxcox(trainData$TripDuration~., data = new_trainData)
lamda = box$x
lik = box$y
bc = cbind(lamda, lik)
bc[order(-lik),]
y_lamda = 0.3434

#inverse Boxcox
invBoxCox <- function(x, lambda){
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)}



#validate effect of boxcox transform 
boxcox_lm = lm(trainData$TripDuration^y_lamda~. , data = trainData)
boxcox_lm = lm(new_predDataTrans[,13]~., data = new_predDataTrans[,-1])
par(mfrow=c(1,2))
hist(boxcox_lm$resid, xlim = c(-6,6), col = "light blue")
qqnorm(boxcox_lm$resid)


#prediction for testdata
lm_pred_DT_boxcox <- predict(boxcox_lm, testX)
lm_result_boxcox <- data.frame(new_data2$Id,round(lm_pred_DT_boxcox^(1/y_lamda)))
colnames(lm_result_boxcox) <- c('id','trip_duration')
write.csv(lm_result_boxcox, "/Users/xucc/Documents/GMU/OR568/project/lm_result_bc.csv",row.names = F)
nrow(lm_result_boxcox)


#---------------regression tree
library(rpart)
regtree_model <- rpart(trainData$TripDuration~., trainData, method="anova") #,control=rpart.control(cp=0.01,maxdepth=10)
regtree_model
regtree_model$cptable

plotcp(regtree_model) #plot cross validataion results
regtree_model_DT <- predict(regtree_model, testData)
regtree_obs_pred <- data.frame(new_data2$Id,regtree_model_DT)
colnames(regtree_obs_pred) <- c('id','trip_duration')
write.csv(regtree_obs_pred, "/Users/xucc/Documents/GMU/OR568/project/regtree_result.csv",row.names = F)
nrow(regtree_obs_pred)
regtreePR = postResample(pred=regtree_model_DT, obs=testY)
regtreePR   #       RMSE=363.36   Rsquared=0.61



#=====================================SVM=======
svmmodel <- svm(trainData$TripDuration~., trainData)
svm_model_dt <- predict(svmmodel, testData)
svm_obs_pred <- data.frame(testData$Id,testData$TripDuration,svm_model_dt)
