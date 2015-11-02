

forecasting.algorithm <- function (train , test , algoname , ...) {
  
  ALGONAMES <- c('Using.auto.arima');
  
  if(algoname %in% ALGONAMES){
     cat(" This algorithm supported ")
     f <- get(algoname)
  } else {
    stop ( algoname , ' this algorithm not supported ')
  }
  
  forecastStores <- unique(test$Store)
  train.dates <- unique(train$Date)
  

  num.train.dates <- length(train.dates)
  cat(" Length of dates to be forecasted" , num.train.dates)
  train.frame <- data.frame(Date =rep(train.dates, length(forecastStores)),
                            Store=rep(forecastStores, each=num.train.dates))
  
  myfcst <- data.frame(matrix(ncol = 3, nrow = 0))
  
  for(currentStore in forecastStores)
  {
    if (currentStore != 512){
    
    cat("Running Forecasting for : ", currentStore , "\n")
    currentData <- filter(train , train$Store == currentStore)
    testData <- filter(test , test$Store == currentStore)
    
    head(currentData)
    cat("Running for store ", currentStore , " has data with rows ", nrow(currentData))
    horizon <- 48
    cat(" Horizon inferred ",horizon)
    s <- ts(currentData$Sales, frequency=365)
    
    newFcst <- f(currentData, testData , currentStore , s)
    myfcst <- bind_rows( myfcst, newFcst)
    }
  }
  
  myfcst   
}

Using.auto.arima <- function(currentData, testData, currentStore , s)
{
  
    model <- auto.arima(s,xreg = as.numeric(currentData$Promo) ,seasonal=TRUE)
    fc <- predict(model, h=horizon , newxreg= as.numeric(testData$Promo))
    
    
    newFcst <- data.frame(Store = rep(currentStore,48),
                         Date = seq(as.Date("2015-08-01"), as.Date("2015-09-17"), "day"),
                         Sales = fc$pred)
    
    newFcst
    
}
