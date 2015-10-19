

forecasting.algorithm <- function (train , test , algoname , ...) {
  
  #algoname <- 'auto.arima'
  ALGONAMES <- c('auto.arima');
  
  if(algoname %in% ALGONAMES){
     cat(" This algorithm supported ")
  } else {
    stop ( algoname , ' this algorithm not supported ')
  }
  
  length(train)
  
  forecastStores <- unique(test$Store)
  train.dates <- unique(train$Date)

  num.train.dates <- length(train.dates)
  cat(" Length of dates to be forecasted" , num.train.dates)
  train.frame <- data.frame(Date =rep(train.dates, length(forecastStores)),
                            Store=rep(forecastStores, each=num.train.dates))
  
  head(train.frame)
    
  myfcst <- data.frame(matrix(ncol = 3, nrow = 0))
  
  for(currentStore in forecastStores)
  {
    cat("Running Forecasting for : ", currentStore , "\n")
    currentData <- filter(train , train$Store == currentStore)
    
    head(currentData)
    cat("Running for store ", currentStore , " has data with rows ", nrow(currentData))
    horizon <- 48
    cat(" Horizon inferred ",horizon)
    s <- ts(currentData$Sales, frequency=365)
    model <- auto.arima(s,seasonal=FALSE)
    fc <- forecast(model, h=horizon)
    fc$mean
    
    newFcst <- data.frame(Store = rep(currentStore,48),
                         Date = seq(as.Date("2015-08-01"), as.Date("2015-09-17"), "day"),
                         Sales = fc$mean)
   
      myfcst <- bind_rows( myfcst, newFcst)
  }
  
  myfcst   
}
