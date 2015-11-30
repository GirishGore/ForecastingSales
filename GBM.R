setwd("C:\\Users\\Girish.Gore\\Desktop\\RossMan\\ForecastingSales")
library(gbm)
library(readr)
library(randomForest)
library(lubridate)
library(dplyr)
set.seed(616)

cat("reading the train and test data\n")
train <- read_csv("train.csv")
test  <- read_csv("test.csv")
store <- read_csv("store.csv")

train <- merge(train,store)
test <- merge(test,store)

# There are some NAs in the integer columns so conversion to zero
train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

# looking at only stores that were open in the train set
# may change this later
#train <- train[ which(train$Open=='1'),]

# seperating out the elements of the date column for the train set
train$month <- as.factor(month(as.Date(train$Date)))
train$year <- as.factor(year(as.Date(train$Date)))

train$Week <- as.factor(week(as.Date(train$Date)))
train$DayOfWeek <- as.factor(train$DayOfWeek)

names(train)
weeklysales <- train %>%
  group_by(Store, Week) %>%
  summarise(weeklysales = mean(Sales))

weeklysales

train <- merge(train,weeklysales)
test <- merge(test,weeklysales)


DOWcustomers <- train %>%
  group_by(Store, DayOfWeek) %>%
  summarise(DOWcustomers = mean(Customers))

DOWcustomers

train <- merge(train,DOWcustomers)
test <- merge(test,DOWcustomers)

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
#train <- train[,-c(3)]

# seperating out the elements of the date column for the test set
test$month <- as.factor(month(as.Date(test$Date)))
test$year <- as.factor(year(as.Date(test$Date)))
test$Week <- as.factor(week(as.Date(test$Date)))
test$DayOfWeek <- as.factor(test$DayOfWeek)

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
#test <- test[,-c(4)]

names(train)
names(test)
str(train)
str(test)
head(train)
head(test)
summary(train)


train$Open <- as.factor(train$Open)
train$Promo <- as.factor(train$Promo)
train$Promo2 <- as.factor(train$Promo2)
train$StateHoliday <- as.factor(train$StateHoliday)
train$SchoolHoliday <- as.factor(train$SchoolHoliday)
train$StoreType <- as.factor(train$StoreType)
train$Assortment <- as.factor(train$Assortment)

test$Open <- as.factor(test$Open)
test$Promo <- as.factor(test$Promo)
test$Promo2 <- as.factor(test$Promo2)
test$StateHoliday <- as.factor(test$StateHoliday)
test$SchoolHoliday <- as.factor(test$SchoolHoliday)
test$StoreType <- as.factor(test$StoreType)
test$Assortment <- as.factor(test$Assortment)

names(train)
names(test)
str(train)
feature.names <- names(train)[c(1,2,6:19,21,22,23)]
cat("Feature Names\n")
feature.names

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

##### PREPROCESSING FOR MISSING VALUES ######

store13 <- subset(train, Store==13)
ggplot(store13, aes(Date,Sales)) +
  geom_line() +
  geom_smooth() +
  ggtitle("Revenue for Store 13 over time")

all_stores <- unique(train$Store)
stores_reporting <- train$Store[train$Date == as.Date("2014-7-1")]
missing_stores <- all_stores[!(all_stores %in% stores_reporting)]
missing_stores

for (date in seq(as.Date("2014-7-2"),as.Date("2014-12-31"),by="day")) {
  stores_reporting <- train$Store[train$Date == date]
  missing_on_date <- all_stores[!(all_stores %in% stores_reporting)]
  if (length(setdiff(missing_on_date,missing_stores)) > 0) {
    cat("Date:",date," Difference in missing stores",setdiff(missing_on_date,missing_stores))
  }
}

stores_reporting <- train$Store[train$Date == as.Date("2013-1-1")]
additional_missing_store <- all_stores[!(all_stores %in% stores_reporting)]
additional_missing_store

date <- as.Date("2013-1-1")
day_of_week <- unique(train$DayOfWeek[train$Date == date])
sales <- as.numeric(names(which.max(table(train$Sales[train$Date == date]))))
customers <- as.numeric(names(which.max(table(train$Customers[train$Date == date]))))
open <- as.numeric(names(which.max(table(train$Open[train$Date == date]))))
promo <- as.numeric(names(which.max(table(train$Promo[train$Date == date]))))
state_holiday <- names(which.max(table(train$StateHoliday[train$Date == date])))
school_holiday <- as.numeric(names(which.max(table(train$SchoolHoliday[train$Date == date]))))

missing_row <- data.frame(Store = additional_missing_store,
                          DayOfWeek = day_of_week,
                          Date = date,
                          Week = week(date),
                          Month = month(date),
                          Year = year(date),
                          Sales = sales,
                          Customers = customers,
                          Open = open,
                          Promo = promo,
                          StateHoliday = state_holiday,
                          SchoolHoliday = school_holiday)
train <- rbind(train,missing_row)


###################STEP 2 ##########################

train$logSales <- log(train$Sales+1)

gap <- seq(as.Date("2014-7-1"),as.Date("2014-12-31"),by="day")
n_missing <- length(gap)*length(missing_stores)
missing_df <- data.frame(Store = integer(n_missing),
                         DayOfWeek = integer(n_missing),
                         Date = rep(gap,length(missing_stores)),
                         Sales = integer(n_missing),
                         Customers = integer(n_missing),
                         Open = integer(n_missing),
                         Promo = integer(n_missing),
                         StateHoliday = character(n_missing),
                         SchoolHoliday = integer(n_missing),
                         logSales = numeric(n_missing),
                         stringsAsFactors=FALSE)

for (date in gap) {
  missing_df$Store[missing_df$Date == date] <- missing_stores

  day_of_week <- unique(train$DayOfWeek[train$Date == date])
  missing_df$DayOfWeek[missing_df$Date == date] <- rep(day_of_week, length(missing_stores))

  missing_df$Sales[missing_df$Date == date] <- rep(NA, length(missing_stores))

  missing_df$Customers[missing_df$Date == date] <- rep(NA, length(missing_stores))

  open <- as.numeric(names(which.max(table(train$Open[train$Date == date]))))
  missing_df$Open[missing_df$Date == date] <- rep(open, length(missing_stores))

  promo <- as.numeric(names(which.max(table(train$Promo[train$Date == date]))))
  missing_df$Promo[missing_df$Date == date] <- rep(promo, length(missing_stores))

  state_holiday <- names(which.max(table(train$StateHoliday[train$Date == date])))
  missing_df$StateHoliday[missing_df$Date == date] <- rep(state_holiday, length(missing_stores))

  school_holiday <- as.numeric(names(which.max(table(train$SchoolHoliday[train$Date == date]))))
  missing_df$SchoolHoliday[missing_df$Date == date] <- rep(school_holiday, length(missing_stores))

  missing_df$logSales[missing_df$Date == date] <- rep(NA, length(missing_stores))

}

names(missing_df)
names(train)

missing_df$year <- year(missing_df$Date)
missing_df$month <- month(missing_df$Date)
missing_df$Week <- week(missing_df$Date)

missing_df <- merge(missing_df,store)

missing_df$Open <- as.factor(missing_df$Open)
missing_df$Promo <- as.factor(missing_df$Promo)
missing_df$Promo2 <- as.factor(missing_df$Promo2)
missing_df$StateHoliday <- as.factor(missing_df$StateHoliday)
missing_df$SchoolHoliday <- as.factor(missing_df$SchoolHoliday)
missing_df$StoreType <- as.factor(missing_df$StoreType)
missing_df$Assortment <- as.factor(missing_df$Assortment)
missing_df$week <- NULL


train_filled_gap <- rbind(train,missing_df)
train_filled_gap <- train_filled_gap[order(train_filled_gap$Date),]

train_filled_gap <- train_filled_gap %>%
  group_by(Store, DayOfWeek, Open, Promo) %>%
  mutate(Sales = as.integer(ifelse(is.na(Sales),
                                   ifelse(Open == 0,
                                          0,
                                          median(Sales, na.rm=T)),
                                   Sales))) %>%
  mutate(Customers = as.integer(ifelse(is.na(Customers),
                                       ifelse(Open == 0,
                                              0,
                                              median(Customers, na.rm=T)),
                                       Customers))) %>%
  mutate(logSales = ifelse(is.na(logSales),
                           ifelse(Open == 0,
                                  0,
                                  median(logSales, na.rm=T)),
                           logSales))


train <- train_filled_gap



head(train)
train$Sales <- exp(train$logSales)
##############VALIDITY ###########3
store13 <- subset(train, Store==13)
ggplot(store13, aes(Date,Sales)) +
  geom_line() +
  geom_smooth() +
  ggtitle("Revenue for Store 13 over time")

summary(train)
####################################################
str(train)
cat("checking all stores are accounted for\n")
length(unique(train$Store))

train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

cat("train data column names after slight feature engineering\n")
names(train)
cat("test data column names after slight feature engineering\n")
names(test)

#train$DayOfWeek <- as.factor(train$DayOfWeek)

#p = 0.15

#dsample <- data.frame()

#for(i in levels(train$DayOfWeek)) {
#  dsub <- subset(train, train$DayOfWeek == i)
#  B = ceiling(nrow(dsub) * p)
#  dsub <- dsub[sample(1:nrow(dsub), B), ]
#  dsample <- rbind(dsample, dsub)
#  }

#cat("dimensions of stratified sample set with every day of the week included\n")
#dim(dsample)

#dsample$DayOfWeek <- as.integer(dsample$DayOfWeek)
#cat("check structire of stratified sample\n")
#str(dsample)
summary(train)
summary(log(train$Sales + 1))

for (f in feature.names) {

replace(train$f, is.na(train$f) | is.nan(train$f) | is.infinite(train$f), 0)
}

train <- train[,feature.names]
test <- test[,feature.names]

save(train , file="train.Rdata")
save(test , file="test.Rdata")

set.seed(1000)
library(caTools)
split = sample.split(train$Store, SplitRatio = 0.75)

# Split up the data using subset
train = subset(train, split==TRUE)
train1 = subset(train, split==FALSE)


trainclf <- gbm.fit(train[,feature.names],
                    log(train$Sales+1),
                    distribution = "gaussian",
                    n.trees = 501,
                    interaction.depth = 3,
                    n.minobsinnode = 10,
                    shrinkage = 0.01,
                    bag.fraction = 0.5,
                    keep.data = TRUE,
                    verbose = TRUE,
                    var.names = NULL,
                    response.name = "y",
                    group = NULL)

predictTest = predict(trainclf, type="response", newdata=train1 , n.trees = 501)

  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  epred<-epreds[elab!=0]
  elab<-elab[elab!=0]
  err <- sqrt(mean(((elab-epred)/elab)^2))
  return(list(metric = "RMPSE", value = err))
}


library(caret)
myTuneGrid <- expand.grid(n.trees = c(501,601),interaction.depth = 3:5,shrinkage = c(0.1,0.01) , n.minobsinnode = 10)

fitControl <- trainControl(method = "repeatedcv", number = 3,repeats = 1, verboseIter = FALSE,returnResamp = "all")



myModel <- train(train[,feature.names],
                 log(train$Sales+1),
                 method = "gbm",
                 trControl = fitControl,
                 tuneGrid = myTuneGrid)

summary(trainclf)
clf <- trainclf
cat("model stats\n")
clf
cat("print model\n")
print(clf)
cat("Importance 1\n")
importance(clf)
cat("Permutation Importance Unscaled\n")
importance(clf, type = 1)
cat("GINI Importance\n")
importance(clf, type = 2)
cat("Plot Model\n")
plot(clf)
cat("Plot Importance\n")
plot(importance(clf), lty=2, pch=16)

str(train)
str(test)
feature.names
levels(test$year) <- levels(train$year)

cat("Predicting Sales\n")
predSales <- predict(trainclf, test , n.trees = 1001)
pred <- exp() -1
submission <- data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write_csv(submission, "gbm.csv")



#############################################3

predtrain <- exp(predict(clf, train)) -1
library(caret)
library(ggplot2)

R2(train$Sales , predtrain)
RMSE(predtrain, train$Sales)
cor(predtrain, train$Sales)

head(predtrain)
head(train$Sales)

qplot(y = predtrain , x = train$Sales ,
      xlab = "Predicted" , ylab = "Actual")

residuals <- train$Sales - predtrain


plot(x = predtrain , y = residuals ,
     xlab = "Predicted" , ylab = "Residuals")
