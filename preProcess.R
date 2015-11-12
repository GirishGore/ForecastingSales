setwd("C:\\Users\\Girish.Gore\\Desktop\\RossMan")
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(lubridate)

train <- read.csv("train.csv")
store <- read.csv("store.csv")
test <- read.csv("test.csv")


train$Store <- as.integer(train$Store)
train$DayOfWeek <- as.factor(train$DayOfWeek)
train$Date <- as.Date(train$Date)
train$Week <- as.factor(week(train$Date))
train$Month <- as.factor(month(train$Date))
train$Year <- as.factor(year(train$Date))
train$Open <- as.factor(train$Open)
train$Promo <- as.factor(train$Promo)
train$SchoolHoliday <- as.factor(train$SchoolHoliday)

test$Store <- as.integer(test$Store)
test$DayOfWeek <- as.factor(test$DayOfWeek)
test$Date <- as.Date(test$Date)
test$Week <- as.factor(week(test$Date))
test$Month <- as.factor(month(test$Date))
test$Year <- as.factor(year(test$Date))
test$Open <- as.factor(test$Open)
test$Promo <- as.factor(test$Promo)
test$SchoolHoliday <- as.factor(test$SchoolHoliday)


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

head(missing_df)
head(train)

missing_df$Year <- year(missing_df$Date)
missing_df$Month <- month(missing_df$Date)
missing_df$Week <- week(missing_df$Date)


head(missing_df)
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
                                  mean(logSales, na.rm=T)), 
                           logSales))


train <- train_filled_gap

head(train)
train$Sales <- train$logSales

####################################################


library(dplyr)
train <- arrange(train, Store, Date)
test <- arrange(test ,  Store ,Date)

#debug(forecasting.algorithm)
train.frame <- forecasting.algorithm(data.frame(train), data.frame(test), 'Using.auto.arima')

submission <- merge(test , train.frame , by = c("Store","Date"))
submission <- submission[,c("Id","Sales")]
submission <- arrange(submission, Id)

write.csv(submission,"EnhanceAutoArimaSeasonal.csv", row.names = FALSE)
