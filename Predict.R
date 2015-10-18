setwd("C:\\Users\\Girish\\Desktop\\Rossman")
library(data.table)  

#install.packages("h2o")
library(h2o)
library(gbm)

cat("reading the train and test data (with data.table) \n")
train <- fread("train.csv",stringsAsFactors = F)
test  <- fread("test.csv",stringsAsFactors = F)
store <- fread("store.csv",stringsAsFactors = F)
train <- train[Sales > 0,]  ## We are not judged on 0 sales records in test set
## See Scripts discussion from 10/8 for more explanation.
train <- merge(train,store,by="Store")
test <- merge(test,store,by="Store")

cat("train data column names and details\n")
summary(train)
cat("test data column names and details\n")
summary(test)

## more care should be taken to ensure the dates of test can be projected from train
## decision trees do not project well, so you will want to have some strategy here, if using the dates
train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

# seperating out the elements of the date column for the train set
train[,month:=as.factor(as.integer(format(Date, "%m")))]
train[,year:=as.factor(as.integer(format(Date, "%y")))]
train[,Store:=as.factor(as.numeric(Store))]

test[,month:=as.integer(format(Date, "%m"))]
test[,year:=as.integer(format(Date, "%y"))]
test[,Store:=as.factor(as.numeric(Store))]

## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
##     if the data spans an order of magnitude, consider a log transform
train[,logSales:=log1p(Sales)]

## Use H2O's random forest
## Start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='4G')
## Load data into cluster from R
trainHex<-as.h2o(train)
## Set up variable to use all features other than those specified here
features<-colnames(train)[!(colnames(train) %in% c("Id","Date","Sales","logSales","Customers"))]
## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="logSales", 
                          ntrees = 100,
                          max_depth = 30,
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=trainHex)

trainStores <- unique(train$Store)
testStores <- unique(test$Store)

table(trainStores %in% testStores)
finalStores <- trainStores[which(trainStores %in% testStores == TRUE)]

trainf <- train[train$Store %in% finalStores,]
logSales <- trainf$logSales
trainf$logSales <- NULL

str(trainf)

table(testStores %in% trainStores)
table(trainf$Store)
trainf$Store <- factor(trainf$Store)
table(trainf$PromoInterval)
trainf$StateHoliday <- as.factor(ifelse(trainf$StateHoliday == 'a',1,0))
trainf$SchoolHoliday <- as.factor(as.numeric(trainf$SchoolHoliday))
trainf$StoreType <- as.factor(trainf$StoreType)
trainf$Assortment <- as.factor(trainf$Assortment)
trainf$PromoInterval <- as.factor(trainf$PromoInterval)
trainf$DayOfWeek <- as.factor(trainf$DayOfWeek)
trainf$Promo <- as.factor(trainf$Promo)
trainf$Promo2 <- as.factor(trainf$Promo2)

trainf$Date <- NULL
trainf$Open <- NULL

test$StateHoliday <- as.factor(ifelse(test$StateHoliday == 'a',1,0))
test$SchoolHoliday <- as.factor(as.numeric(test$SchoolHoliday))
test$StoreType <- as.factor(test$StoreType)
test$Assortment <- as.factor(test$Assortment)
test$PromoInterval <- as.factor(test$PromoInterval)
test$DayOfWeek <- as.factor(test$DayOfWeek)
test$Promo <- as.factor(test$Promo)
test$Promo2 <- as.factor(test$Promo2)

test$Date <- NULL

rfHex <- gbm.fit( trainf,
              logSales, 
              distribution = "gaussian",
              n.trees = 800,
              interaction.depth = 5
            )

summary(rfHex)
cat("Predicting Sales\n")
## Load test data into cluster from R
testHex<-as.h2o(test)

## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(predict(rfHex,testHex))
## Return the predictions to the original scale of the Sales data
pred <- expm1(predictions[,1])
summary(pred)
submission <- data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write.csv(submission, "h2o_rf.csv",row.names=F)