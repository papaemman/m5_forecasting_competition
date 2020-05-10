#################################################
#                                               #
#  Statistical Univariate forecasting methods   #
#                                               #
#################################################

# // Statistical Benchmark methods: local time series forecasting //

# Note: 
# Every function takes as input a univarite signal and produce forecastings for the next fh days


## 01. Naive / Seasonal Naive models ----

Naive <- function(x, h, type){
  
  frcst <- rep(tail(x,1), h)   # Naive forecasting: Repeat h times the last value of the time series
  
  if (type=="seasonal"){
    frcst <- head(rep(as.numeric(tail(x,7)), h), h)  # Seasonal (weekly seasonality) Naive forecasting: Repeat h/7 times the last 7 values of the time series
  }
  
  if (type=="seasonal_month"){
    frcst <- head(rep(as.numeric(tail(x,28)), h), h)  # Seasonal (weekly seasonality) Naive forecasting: Repeat h/7 times the last 7 values of the time series
  }
  
  return(frcst)
}

# Naive(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2, type = "naive")
# Naive(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2, type = "seasonal")


## 02. Simple Explonential Smoothing function ----

SES <- function(a, x, h, job){
  y <- c()  
  y[1] <- x[1] #initialization
  
  for (t in 1:(length(x))){
    y[t+1] <- a*x[t]+(1-a)*y[t]
  }
  
  fitted <- head(y,(length(y)-1))
  forecast <- rep(tail(y,1),h)
  
  if (job=="train"){
    return(mean((fitted - x)^2))  # If the SES() called insids SexpS() an error value must be returned for the minimization process
    
  }else if (job=="fit"){
    return(fitted)
    
  }else{
    return(list(fitted=fitted,mean=forecast))
  }
}

# SES(a = 0.1, x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2, job = "train")
# SES(a = 0.1, x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2, job = "fit")
# SES(a = 0.1, x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2, job = "NA")


## 03. Simple explonential smoothing ----

SexpS <- function(x, h){
  a <- optim(par = c(0), fn = SES, x=x, h=1, job="train",
             lower = 0.01, upper = 0.5, method = "L-BFGS-B")$par
  y <- SES(a=a, x=x, h=1, job="forecast")$mean
  forecast <- rep(as.numeric(y), h)
  return(forecast)
}

# SexpS(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2)


## 04. Moving Average  ----

MA <- function(x, h){
  
  mse <- c()
  
  for (k in 2:14){
    
    # k=2
    y <- rep(NA, k)
    
    # For every value of k calculate the sliding window of length k.
    # Calculate the mean of each window and assign the value at the next time point.
    for (i in (k+1):length(x)){  
      y <- c(y, mean(x[(i-k):(i-1)]))
    }
    
    # For the fitted values calculate the mse
    mse <- c(mse, mean((y-x)^2, na.rm = T))
  }
  
  # Find the k which has the least mse
  k <- which.min(mse)+1
  
  # Repeat the last value of the best k-Moving Average
  forecast <- rep(mean(as.numeric(tail(x, k))), h)
  return(forecast)
}

# MA(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 5)


## 04. Moving Average v2----

MA_v2 <- function(x, h){
  
  frc <- c()
  for (i in 1:h) {
    x <- c(x, MA(x = x, h = 1))
  }
  
  return(tail(x, h))
}

# MA(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 5)
# MA_v2(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 5)


## 05. Croston method  ----

Croston <- function(x, h, type){
  
  if (type=="classic"){
    mult <- 1 
    a1 = a2 <- 0.1 
    
  }else if (type=="optimized"){
    mult <- 1 
    a1 <- optim(c(0), SES, x=demand(x), h=1, job="train", lower = 0.1, upper = 0.3, method = "L-BFGS-B")$par
    a2 <- optim(c(0), SES, x=intervals(x), h=1, job="train", lower = 0.1, upper = 0.3, method = "L-BFGS-B")$par
    
  }else if (type=="sba"){
    mult <- 0.95
    a1 = a2 <- 0.1
  }
  
  yd <- SES(a=a1, x=demand(x), h=1, job="forecast")$mean
  yi <- SES(a=a2, x=intervals(x), h=1, job="forecast")$mean
  forecast <- rep(as.numeric(yd/yi), h)*mult
  return(forecast)
}

# Croston(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,3.635758, 3.635758), h = 3, type = "classic")
# Croston(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 3, type = "optimized")
# Croston(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 3, type = "sba")


## 06. Teunter-Syntetos-Babai method ----

TSB <- function(x, h){
  n <- length(x)           # signal length
  p <- as.numeric(x != 0)  # non-zero positions
  z <- x[x != 0]           # non-zero values
  
  a <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.8) 
  b <- c(0.01,0.02,0.03,0.05,0.1,0.2,0.3)
  MSE <- c() ; forecast <- NULL
  
  for (atemp in a){
    for (btemp in b){
      
      zfit <- vector("numeric", length(x))
      pfit <- vector("numeric", length(x))
      zfit[1] <- z[1] ; pfit[1] <- p[1]
      
      for (i in 2:n) {
        pfit[i] <- pfit[i-1] + atemp*(p[i]-pfit[i-1])
        if (p[i] == 0) {
          zfit[i] <- zfit[i-1]
        }else {
          zfit[i] <- zfit[i-1] + btemp*(x[i]-zfit[i-1])
        }
      }
      yfit <- pfit * zfit
      forecast[length(forecast)+1] <- list(rep(yfit[n], h))
      yfit <- c(NA, head(yfit, n-1))
      MSE <- c(MSE, mean((yfit-x)^2, na.rm = T) )
    }
  }
  return(forecast[[which.min(MSE)]])
}

# TSB(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 3)
# TSB(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7, 5.265754, 5.265754), h = 3)


## 07. Aggreagate-Disaggregate Intermittent Demand approach  ----

ADIDA <- function(x, h){
  
  al <- round(mean(intervals(x)),0) # mean inter-demand interval
  
  # Aggregated series (AS)
  AS <- 
    as.numeric(
      na.omit(
        as.numeric(
          zoo::rollapply(tail(x = x, n = (length(x) %/% al)*al), width = al, FUN=sum, by = al)
        )
      )
    )
  
  forecast <- rep(SexpS(x = AS, h = 1)/al, h)
  return(forecast)
}

# ADIDA(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 3)
# ADIDA(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7, 6.05481, 6.05481), h = 3)

## 08. Intermittent Multiple Aggregation Prediction Algorithm  ----

iMAPA <- function(x, h){
  mal <- round(mean(intervals(x)),0)
  frc <- NULL
  for (al in 1:mal){
    frc <- rbind(frc,
                 rep(
                   SexpS(
                     as.numeric(
                       na.omit(
                         as.numeric(
                           rollapply(tail(x, (length(x) %/% al)*al), al, FUN=sum, by = al))
                       )
                     ),1)/al, h))
  }
  
  forecast <- colMeans(frc)
  return(forecast)
}

# iMAPA(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 3)
# iMAPA(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7, 6.05481, 6.05481), h = 2)


## 09. Exponential smoothing in SSOE state space model (smooth::es())  ----

smooth_es <- function(x, h){
  as.numeric(smooth::es(ts(data = x, frequency = 7), h=h)$forecast)
}

# smooth_es(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 5)

smooth_es_v2 <- function(x, h, xreg){
  as.numeric(smooth::es(y = ts(data = x, frequency = 7),
                        model = "AAdA", # "CCC"
                        initial = "backtesting",
                        loss="MSE",
                        h=h)$forecast)
}


## 10. ARIMA ----
auto_arima <- function(x, h){
  as.numeric(forecast(forecast::auto.arima(ts(data = x, frequency = 7)), h=h)$mean)
}

# auto_arima(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3), h = 7)



