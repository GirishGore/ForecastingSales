setwd("C:\\Users\\Girish.Gore\\Desktop\\RossMan")

train <- read.csv("train.csv")
store <- read.csv("store.csv")
submission <- read.csv("sample_submission.csv")
test <- read.csv("test.csv")

str(train)
str(store)
str(submission)
str(test)

#########################################################################
# ADDING COLUMNS JUST TO CHECK
##########################################################################
summary(train)

train[, Date := as.Date(Date)]
test[, Date := as.Date(Date)]
store
train <- train[order(Date)]
test <- test[order(Date)]
summary(train)
summary(test)
test[is.na(test$Open), ] # Only store 622
test$Open[test$Store == 622]


test[is.na(test)] <- 1
 

train[, lapply(.SD, function(x) length(unique(x)))]
test[, lapply(.SD, function(x) length(unique(x)))]
# All test stores are also in the train data
sum(unique(test$Store) %in% unique(train$Store)) 
# 259 train stores are not in the test data
sum(!(unique(train$Store) %in% unique(test$Store))) 
table(train$Open) / nrow(train) # Percent Open Train
table(test$Open) / nrow(test) # Percent Open Test 
table(train$Promo) / nrow(train) # Percent of the time promo in train
table(test$Promo) / nrow(test) # Percent of the time promo in test
table(train$StateHoliday) / nrow(train) # Percent of the time holiday in train
table(test$StateHoliday) / nrow(test) # no b and c = no easter holiday and no christmas
table(train$SchoolHoliday) / nrow(train) # Percent of the time school holiday in train
table(test$SchoolHoliday) / nrow(test) # Percent of the time school holiday in test
 

#There are no obvious breaks in the data.
#The test period ranges from 2015-08-01 to 2015-09-17, so the task is to predict 48 days.
#The train period ranges from 2013-01-01 to 2015-07-31.

plot(train$Date, type = "l")
plot(test$Date, type = "l")
# As expected all 856 stores to be predicted daily
all(table(test$Date) == 856) 
 

#Let's look at the columns that are unique to the train set:

hist(train$Sales, 100)
hist(aggregate(train[Sales != 0]$Sales, 
by = list(train[Sales != 0]$Store), mean)$x, 100, 
main = "Mean sales per store when store was not closed")

hist(train$Customers, 100)
hist(aggregate(train[Sales != 0]$Customers, 
by = list(train[Sales != 0]$Store), mean)$x, 100,
main = "Mean customers per store when store was not closed")
ggplot(train[Sales != 0], aes(x = factor(SchoolHoliday), y = Sales)) +
geom_jitter(alpha = 0.1) +
geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)
ggplot(train[train$Sales != 0 & train$Customers != 0],
aes(x = log(Customers), y = log(Sales))) + 
geom_point(alpha = 0.2) + geom_smooth()
ggplot(train[train$Sales != 0 & train$Customers != 0],
aes(x = factor(Promo), y = Sales)) + 
geom_jitter(alpha = 0.1) +
geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)
ggplot(train[train$Sales != 0 & train$Customers != 0],
aes(x = factor(Promo), y = Customers)) + 
geom_jitter(alpha = 0.1) +
geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)
 

#Note: I chose to not plot that data including days with 0 sales 
#or customers because that would have biased the boxplots.

#Sales is as expected strongly correlated with the number of customers.
#It looks like the Boxplots of customers overlap a little more than the boxplots
#of sales. This would mean that the promos are not mainly attracting more customers
#but make customers spend more. The mean amount spent per customer is about one
#Euro higher:

with(train[train$Sales != 0 & train$Promo == 0], mean(Sales / Customers))
with(train[train$Sales != 0 & train$Promo == 1], mean(Sales / Customers))
 

#There are sometimes promos while the respective store is closed and there are promos
#45% of the time:

table(ifelse(train$Sales != 0, "Sales > 0", "Sales = 0"),
ifelse(train$Promo, "Promo", "No promo"))
 

#At least there are no sales when the stores are closed but there are some stores
#that, according to the data, made no sales although they were opened even if 
#they had some customers. These observations *may* be errors in the data / outliers:
table(ifelse(train$Open == 1, "Opened", "Closed"),
ifelse(train$Sales > 0, "Sales > 0", "Sales = 0"))

# That tends to happen on consecutive days. Some stores even had customers
# (who bought nothing?)

train[Open == 1 & Sales == 0]
 

#The stores have different amounts of days with zero sales.
#There are spikes in the sales before the stores close and after the reopen:

zerosPerStore <- sort(tapply(train$Sales, list(train$Store), function(x) sum(x == 0)))
hist(zerosPerStore,100)
# Stores with the most zeros in their sales:
tail(zerosPerStore, 10)
# Some stores were closed for some time, some of those were closed multiple times
plot(train[Store == 972, Sales], ylab = "Sales", xlab = "Days", main = "Store 972")
plot(train[Store == 103, Sales], ylab = "Sales", xlab = "Days", main = "Store 103")
plot(train[Store == 708, Sales], ylab = "Sales", xlab = "Days", main = "Store 708")
 

#There are also stores that have *no* zeros in their sales. These are the exception
#since they are opened also on sundays / holidays. The sales of those stores
#on sundays are particularly high:

ggplot(train[Store == 85], 
aes(x = Date, y = Sales, 
color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + 
geom_point(size = 3) + ggtitle("Sales of store 85 (True if sunday)")
ggplot(train[Store == 262], 
aes(x = Date, y = Sales, 
color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + 
geom_point(size = 3) + ggtitle("Sales of store 262 (True if sunday)")
 

#That is not true in general. The variability of sales on sundays is quite high
#while the median is not:
ggplot(train[Sales != 0],
aes(x = factor(DayOfWeek), y = Sales)) + 
geom_jitter(alpha = 0.1) + 
geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

 

#The `store` file contains information about the stores that can be linked to 
#`train` and `test` via the store ID.**

summary(store)
table(store$StoreType)
table(store$Assortment)
# There is a connection between store type and type of assortment
table(data.frame(Assortment = store$Assortment, StoreType = store$StoreType))
hist(store$CompetitionDistance, 100)
# Convert the CompetitionOpenSince... variables to one Date variable
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
store$CompetitionOpenSinceMonth, sep = "-"))
# One competitor opened 1900
hist(as.yearmon("2015-10") - store$CompetitionOpenSince, 100, 
main = "Years since opening of nearest competition")
# Convert the Promo2Since... variables to one Date variable
# Assume that the promo starts on the first day of the week
store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, 
store$Promo2SinceWeek, 1, sep = "-"),
format = "%Y-%U-%u")
hist(as.numeric(as.POSIXct("2015-10-01", format = "%Y-%m-%d") - store$Promo2Since), 
100, main = "Days since start of promo2")
table(store$PromoInterval)
 

#The stores with promos tend to make lower sales. This does not necessary mean
#that the promos don't help or are counterproductive. They are possibly measures
#that are taken mainly by stores with low sales in the first place:
# Merge store and train 
train_store <- merge(train, store, by = "Store")
ggplot(train_store[Sales != 0], aes(x = factor(PromoInterval), y = Sales)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)
 

#The different store types and assortment types imply different overall levels of sales and seem to
#be exhibiting different trends:
  
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
  geom_smooth(size = 2)
ggplot(train_store[Customers != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(StoreType))) + 
  geom_smooth(size = 2)
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + 
  geom_smooth(size = 2)
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(Assortment))) + 
  geom_smooth(size = 2)
 

#The effect of the distance to the next competitor is a little counterintuitive.
#Lower distance to the next competitor implies (slightly, possibly not significantly)
#higher sales. This may occur
#(my assumption) because stores with a low distance to the next competitor are
#located in inner cities or crowded regions with higher sales in general. Maybe
#the effects of being in a good / bad region and having a competitor / not 
#having a competitor cancel out:
  
salesByDist <- aggregate(train_store[Sales != 0 & !is.na(CompetitionDistance)]$Sales, 
                         by = list(train_store[Sales != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), mean)
colnames(salesByDist) <- c("CompetitionDistance", "MeanSales")
ggplot(salesByDist, aes(x = log(CompetitionDistance), y = log(MeanSales))) + 
  geom_point() + geom_smooth()
 

#A missing value for `CompetitionDistance` doesn't necessarily mean that there is
#no competiton. Maybe that data was just not collected, yet. There is no obvious 
#connection between sales and having `NA` as `CompetitionDistance`:

ggplot(train_store[Sales != 0],
aes(x = factor(!is.na(CompetitionOpenSinceYear)), y = Sales)) +
geom_jitter(alpha = 0.1) +
geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA) +
ggtitle("Any competition?")
 

#So what happens if a competitor opens? In order to assess this effect we fetch
#data from all stores that first have `NA` as `CompetitorDistance` and later some
#value. **Only the month, not the date, of the opening of the competitor is known
#so we need a rather large window to see the effect (100 days)**. 147 stores
#had a competitor move into their area during the available time span. The 
#competition leaves a 'dent' in the sales which looks a little different 
#depending on the chosen `timespan` so I wouldn't like to argue about statistical
#significance based on this plot alone. It's informative to look at anyway:

# Sales before and after competition opens
train_store$DateYearmon <- as.yearmon(train_store$Date)
train_store <- train_store[order(Date)]
timespan <- 100 # Days to collect before and after Opening of competition
beforeAndAfterComp <- function(s) {
x <- train_store[Store == s]
daysWithComp <- x$CompetitionOpenSince >= x$DateYearmon
if (any(!daysWithComp)) {
compOpening <- head(which(!daysWithComp), 1) - 1
if (compOpening > timespan & compOpening < (nrow(x) - timespan)) {
x <- x[(compOpening - timespan):(compOpening + timespan), ] 
x$Day <- 1:nrow(x)
return(x)
}
}
}
temp <- lapply(unique(train_store[!is.na(CompetitionOpenSince)]$Store), beforeAndAfterComp)
temp <- do.call(rbind, temp)
# 147 stores first had no competition but at least 100 days before the end
# of the data set
length(unique(temp$Store))
ggplot(temp[Sales != 0], aes(x = Day, y = Sales)) + 
geom_smooth() + 
ggtitle(paste("Competition opening around day", timespan))
 


#The seasonplot is adapted from [spsrini](https://www.kaggle.com/spsrini/rossmann-store-sales/seasonplot-month/files):

temp <- as.data.frame(train)
temp$year <- as.factor(format(temp$Date, "%Y"))
temp$month <- as.factor(format(temp$Date, "%m"))
agg <- aggregate(Sales ~ ., data=temp[, c("Sales", "month" ,"year")], FUN=sum)
SalesTS <- ts(agg$Sales, start=2013, frequency=12)
col = rainbow(3)
seasonplot(SalesTS, col=col, year.labels.left = TRUE, pch=19, las=1)

#########################################################################

train <- merge(train,store)
test <- merge(test,store)

# There are some NAs in the integer columns so conversion to zero
library(caret)

train_numr = train[, sapply(train, is.numeric)]
train_char = train[, sapply(train, is.character)]

length(train)


preProcObj <- preProcess(train_numr,c("medianImpute","center", "scale"))
train_numr <- predict(preProcObj, train_numr)

test_numr = test[, sapply(test, is.numeric)]
preProcObj <- preProcess(test_numr,c("medianImpute","center", "scale"))
test <- predict(preProcObj, test)

length(train[is.na(train)]) 
length(test[is.na(test)])

train[is.na(train)] <- 0
test[is.na(test)] <- 0

cat("train data column names and details\n")
names(train)
str(train)
summary(train)
cat("test data column names and details\n")
names(test)
str(test)
summary(test)

# looking at only stores that were open in the train set
# may change this later
train <- train[ which(train$Open=='1'),]

# seperating out the elements of the date column for the train set
library(lubridate)
train$Date <- as.Date(train$Date)

train$month <- as.factor(month(train$Date))
train$year <- year(train$Date)
train$day <- as.factor(day(train$Date))

levels(train$day)
levels(test$year)

names(train)
# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
train <- train[,-c(3)]

# seperating out the elements of the date column for the test set
test$Date <- as.Date(test$Date)
test$month <- as.factor(month(test$Date))
test$year <- NULL
test$year <- year(test1$Date)
test$day <- as.factor(day(test$Date))

train$year

#test$month <- as.integer(format(test$Date, "%m"))
#test$year <- as.integer(format(test$Date, "%y"))
#test$day <- as.integer(format(test$Date, "%d"))

names(test)
# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
test <- test[,-c(4)]

feature.names <- names(train)[c(1,2,5:19)]
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

cat("train data column names after slight feature engineering\n")
names(train)
cat("test data column names after slight feature engineering\n")
names(test)

install.packages("randomForest")
library(randomForest)
"clf1 <- randomForest(train[,feature.names], 
                     log(train$Sales+1), 
                     ntree=12,
                     sampsize=300000, 
                     do.trace=TRUE,
                     nodesize = 10
)"
clf2 <- randomForest(train[,feature.names], 
                     log(train$Sales+1), 
                     ntree=12,
                     sampsize=200000, 
                     do.trace=TRUE,
                     nodesize = 10
)
cat("Predicting Sales\n")
clf2

#pred1 <- exp(predict(clf1, test)) -1
predict(clf2, test)
pred2 <- exp(predict(clf2, test)) -1
#pred = (pred1 + pred2)/2
submission <- data.frame(Id=test1$Id, Sales=pred2)
cat("saving the submission file\n")
write_csv(submission, "rf1.csv")
