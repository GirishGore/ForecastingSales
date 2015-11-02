

forecasting.algorithm <- function (train , test , algoname , ...) {
  
  ALGONAMES <- c('Using.auto.arima',
                 'Visualizing.TimeSeries.Decomposition',
                 'Using.furier.series');
  
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
    if (currentStore == 512){
    #currentStore <- 512
    cat("Running Forecasting for : ", currentStore , "\n")
    currentData <- filter(train , train$Store == currentStore)
    testData <- filter(test , test$Store == currentStore)
    
    cat("Running for store ", currentStore , " has data with rows ", nrow(currentData))
    s <- ts(currentData$Sales, frequency=365)
    
    newFcst <- f(currentData, testData , currentStore , s)
    myfcst <- bind_rows( myfcst, newFcst)
    }
  }
  
  myfcst   
}

Visualizing.TimeSeries.Decomposition <- function(currentData, testData, currentStore ,s)
{
    fit <- stl(s, s.window = 365)
    plot(s, col="gray",
         main="Electrical equipment manufacturing",
         ylab="New orders index", xlab="")
    lines(fit$time.series[,2],col="red",ylab="Trend")
    plot(fit)
    monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal")
    
    plot(s, col="grey",
         main="Electrical equipment manufacturing",
         xlab="", ylab="New orders index")
    lines(seasadj(fit),col="red",ylab="Seasonally adjusted")
    
    NULL
}

Using.auto.arima <- function(currentData, testData, currentStore , s)
{
  
    horizon <- 48
    cat(" Horizon inferred ",horizon)
    model <- auto.arima(s,xreg = as.numeric(currentData$Promo) ,seasonal=TRUE)
    fc <- predict(model, h=horizon , newxreg= as.numeric(testData$Promo))
    
    
    newFcst <- data.frame(Store = rep(currentStore,48),
                         Date = seq(as.Date("2015-08-01"), as.Date("2015-09-17"), "day"),
                         Sales = fc$pred)
    
    newFcst
    
}

Using.furier.series <- function(currentData, testData, currentStore , s)
{
  horizon <- 365
  k <- length(s)/horizon
  s <- window(s ,k * horizon)
  cat(" Horizon inferred ",horizon , " length of time sereies s ", length(s))
  
  model <- auto.arima(s,xreg = data.frame(as.numeric(currentData$Promo), fourier(s,K=k)))
  
  cat(" Finished building a model ")
  fc <- predict(model, h=horizon , 
                newxreg= data.frame(as.numeric(testData$Promo), fourier(testData$Promo,K=k )))
  
  
  newFcst <- data.frame(Store = rep(currentStore,horion),
                        Date = seq(as.Date("2015-08-01"), as.Date("2015-09-17"), "day"),
                        Sales = fc$pred)
  
  newFcst
  
}
