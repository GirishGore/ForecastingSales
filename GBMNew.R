setwd("C:\\Users\\Girish.Gore\\Desktop\\RossMan\\ForecastingSales")
library(gbm)
library(readr)
library(randomForest)
library(lubridate)
library(dplyr)
library(caTools)

load("train.Rdata")
load("test.Rdata")

#set.seed(1000)
#split = sample.split(train$Store, SplitRatio = 0.75)

#train = subset(train, split==TRUE)
#train1 = subset(train, split==FALSE)

library(caret)
myTuneGrid <- expand.grid(n.trees = c(501,601),interaction.depth = 3:5,shrinkage = c(0.1,0.01) , n.minobsinnode = 10)

fitControl <- trainControl(method = "repeatedcv", number = 3,repeats = 1, verboseIter = FALSE,returnResamp = "all")



myModel <- train(train[,feature.names],
                 log(train$Sales+1),
                 method = "gbm",
                 trControl = fitControl,
                 tuneGrid = myTuneGrid)

