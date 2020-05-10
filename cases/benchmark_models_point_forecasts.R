##################################################
#                                                #
#  M5 forecasting accuracy kaggle competition    #
#         benchmark models                       #     
#                                                #
##################################################

## Note: 
# This script was provided by competition hosts and
# contains all the benchamrk models, used in the specific dataset.
# One, can find more details in official github repository: https://github.com/Mcompetitions/M5-methods


## Note from hosts:
# The R code can be used for implementing the M5 benchmarks for the case of the point forecasts (Accuracy competition).
# The code describes - among others - the way the weights are calculated per series, the scaling applied, as well as how the final scores are computed per level and in total.
# Given that the validation test-set is not provided to the participants at this stage of the competition, a dummy test-set is given instead to enable the calculation of the scores.


## 00. Load packages ----
library(zoo)
library(randomForest)
library(RSNNS)
library(foreach)
library(smooth)
library(doSNOW)
library(plyr)
library(forecast)


## 01. Read data ----

# 1. Sales data
sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F)

# 2. Calendar data
calendar <- read.csv("data/raw/calendar.csv", stringsAsFactors = F)

# 3. Prices data
prices <- read.csv("data/raw/sell_prices.csv", stringsAsFactors = F)

  
## 02. Explore data ----

# Sales data
dim(sales)    # 30490  1919
View(head(sales))
str(sales)

sales$dept_id <- as.factor(sales$dept_id)
sales$cat_id <- as.factor(sales$cat_id)
sales$store_id <- as.factor(sales$store_id)
sales$state_id <- as.factor(sales$store_id)

summary(sales$store_id)
summary(sales$state_id)


# Calendar data
dim(calendar) # 1969   14
calendar$date <- as.Date(calendar$date)

head(calendar)
str(calendar)

calendar$date[1]
calendar$date[1969]
table(calendar$wm_yr_wk)

# Prices data
dim(prices)  # 6841121       4
head(prices)

prices$wm_yr_wk %>% summary()


## 03. Define helper functions ----

## These functions support the main forecasting functions

# 1. Count intervals with consequitevely 0s
intervals <- function(x){
  y <- c()
  k <- 1
  counter <- 0
  
  for (tmp in (1:length(x))){
    if(x[tmp]==0){
      counter<-counter+1
    }else{
      k<-k+1
      y[k]<-counter
      counter<-1
    }
  }
  y <- y[y>0]
  y[is.na(y)] <- 1
  y
}

## Test intervals()
# x <- c(1,0,0,0,1,0,0,2,1,0,0,1,0) 
# intervals(x)
# 1 4 3 1 3


# 2. Keep only demand values (ie only non-negative values)
demand <- function(x){
  y <- x[x!=0]
  y
}

# Test demand()
# x <- c(1,2,3,4,5,0,0,6,0)
# demand(x)


# 3. Recompose functions
recompose <- function(x,y1,y2,k){
  z1 = z2 <- c()
  
  tmp <- 1
  for (t in (1):(length(x)-k)){
    if (x[t]==0){
      tmp<-tmp
    }else{
      tmp<-tmp+1
    }
    z1[t+1]<-y1[tmp]
    z2[t+1]<-y2[tmp]
  }
  z<-z1/z2
  head(z, length(x))
}

# Test



# 4. CreateSamples() function used in for Machine Learning models (mlp, rf)
# Note: This functions takes sliding windows of length xi + x0,
#       to create all possible n-samples of the raw signal

CreateSamples <- function(datasample, xi, xo=1){
  
  ## Test
  # datasample = Sinsample
  # xi = ni
  # xo <- 1
  
  sample <- matrix(NA, nrow = length(datasample), ncol=(xi+xo)) 
  
  for (cid in (xi+xo):length(datasample)){
    sample[cid,] <- datasample[(cid-xi-xo+1):cid]
  }
  
  sample <- as.matrix(data.frame(na.omit(sample)))
  return(sample)
}

## Test: How CreateSamples function works?

## raw signal: t1, t2, t3, t4, t5, t6, t7, t8, t9, t10

## Machine Learning dataframe (window size xi = 4, x0 = 1)
# t1 t2 t3 t4 t5
# t2 t3 t4 t5 t6
# t3 t4 t5 t6 t7
# t4 t5 t6 t7 t8
# t5 t6 t7 t8 t9
# t6 t7 t8 t9 t10
# t7 t8 t9 t10 ..

CreateSamples(datasample = c(1,2,3,4,5,6,7,8,9,10), xi = 5,xo = 1)

# X1 X2 X3 X4 X5 X6
# 1  2  3  4  5  6
# 2  3  4  5  6  7
# 3  4  5  6  7  8
# 4  5  6  7  8  9
# 5  6  7  8  9 10


## 5. Statistics function
# This function returns some basic statistics about the forecasted series

statistics <- function(tsid){  # tsid: time series id
  
  # Test
  # tsid = 1
  # time_series_b   (this object will be defined in global environment, go to line 539 to define it)
  
  
  # Define input
  input <- time_series_b[[tsid]]
  
  ## Calculate summary statistics from the total time_series
  lngth <- length(input$x)
  D <- demand(input$x)
  ADI <- mean(intervals(input$x))
  CV2 <- (sd(D)/mean(D))^2
  Min <- min(input$x)
  Low25 <- as.numeric(quantile(input$x,0.25))
  Mean <- mean(input$x)
  Median <- median(input$x)
  Up25 <- as.numeric(quantile(input$x,0.75))
  Max <- max(input$x)
  pz <- length(input$x[input$x==0])/lngth
  
  if (ADI > (4/3)){
    if (CV2 > 0.5){
      Type <- "Lumpy"        # Εξόγκωμα
    }else{
      Type <- "Intermittent" # Διακοπτώμενος
    } 
  }else{
    if (CV2 > 0.5){
      Type <- "Erratic"      # Ασσυνεπής / Απρόβλεπτος
    }else{
      Type <- "Smooth"       # Ομαλός
    }
  }
  
  ## Define ids
  item_id = input$item_id
  dept_id = input$dept_id
  cat_id = input$cat_id
  store_id = input$store_id 
  state_id = input$state_id
  
  
  ## Calculate dollar sales from last 28 days
  
  # Ex-Prices (Get the corresponding ex-price from prices dataset)
  ex_price <- prices[(prices$item_id==input$item_id) & (prices$store_id==input$store_id),] 
  
  # Merge with calendar
  ex_calendar <- merge(έ, ex_price, by=c("wm_yr_wk"), all.x = T)   # Note that prices datasets doesn't contain prices for all days for every product
  ex_calendar <- ex_calendar[order(ex_calendar$date),]
  row.names(ex_calendar) <- NULL
  
  # Keep only date, wm_yr_wk, sell_price columns
  ex_dataset <- ex_calendar[,c("date", "wm_yr_wk", "sell_price")]
  
  # Add sales column
  ex_dataset$sales <- c(rep(NA, times = nrow(ex_dataset)-lngth-56), # Fill with NAs the starting positions which are equal to 0 in raw sales dataset
                        input$x,                                    # Add the actual sales data, after the first non-zero value
                        rep(NA, 56))                                # Add 56 = 28 + 28 more NAs, for the forecasting horizon
  
  # Keep the last 28 days (4 weeks) from training data
  ex_dataset <- head(tail(ex_dataset,3*28),28)
  
  # Compute dollar sales (sell_price * sales for each day) for this item, for the last 28 days
  dollar_sales <- sum(ex_dataset$sell_price*ex_dataset$sales, na.rm = T)
  
  
  ## Define Statistics matrix
  matrix_s <- data.frame(tsid,
                         item_id, dept_id, cat_id, store_id, state_id,
                         lngth, ADI, CV2, pz, Type, Min, Low25, Mean, Median, Up25, Max, 
                         dollar_sales)
  return(matrix_s)
  
}


## 04. Define Benchmark methods I  (local time series forecasting) ----

# Note: These functions implement the M5 benchmarks models

## Inputs:
# x: time series
# x = input
# h: forecasting horizon
# h = fh

## Outputs:
# frcst: forecasts


## 1. Naive / Seasonal Naive models
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

# 2. Simple Explonential Smoothing function
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


## 3. Simple explonential smoothing
SexpS <- function(x, h){
  a <- optim(par = c(0), fn = SES, x=x, h=1, job="train",
             lower = 0.01, upper = 0.5, method = "L-BFGS-B")$par
  y <- SES(a=a, x=x, h=1, job="forecast")$mean
  forecast <- rep(as.numeric(y), h)
  return(forecast)
}

# SexpS(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2)


## 4. Moving Average
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

# MA(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2)


## 5. Croston method
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

# Croston(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 3, type = "classic")
# Croston(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 3, type = "optimized")
# Croston(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 3, type = "sba")


## 6. Teunter-Syntetos-Babai method
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


## 7. Aggreagate-Disaggregate Intermittent Demand approach
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


## 8. Intermittent Multiple Aggregation Prediction Algorithm
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
  return(forecast)            # Later addition
}

# iMAPA(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 2)
# iMAPA(x = c(1,2,0,0,0,1,2,0,0,0,1,1,0,0,0,1,1,0,0,0,0), h = 1)


## 9. Exponential smoothing in SSOE state space model (smooth::es())

smooth_es <- function(x, h){
  as.numeric(smooth::es(ts(data = x, frequency = 7), h=h)$forecast)
}

# smooth_es(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), h = 1)


## 10. ARIMA

auto_arima_v2 <- function(x, h){
  as.numeric(forecast(forecast::auto.arima(y = ts(data = x, frequency = 7)), h=h)$mean)
}


auto_arima_v2 <- function(x, h){
  as.numeric(forecast(forecast::auto.arima(y = ts(data = x, frequency = 7),
                                           max.p = 7,
                                           max.q = 7,
                                           max.P = 5,
                                           max.Q = 5,
                                           max.order = 5,
                                           max.d = 4,
                                           max.D = 3,
                                           biasadj = T,
                                           nmodels = 200, stepwise = FALSE,
                                           parallel = T, num.cores = 8,
                                           ), h=h)$mean)
}

# auto_arima(x = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3), h = 7)
  

## 11. Multilayer Perceptron at time series level (local)

MLP_local <- function(input, fh, ni){
  
  # ni = 14
  
  # Scale data (min-max scaling)
  MAX <- max(input)
  MIN <- min(input)
  Sinsample <- (input-MIN)/(MAX-MIN)
  
  # Create training sample
  samplegenerate <- CreateSamples(datasample = Sinsample, xi = ni)
  dftrain <- data.frame(samplegenerate)
  colnames(dftrain) <- c(paste0("X",c(1:ni)),"Y")
  
  train_x <- as.matrix(dftrain[,1:(ncol(dftrain)-1)])
  train_y <- dftrain[,ncol(dftrain)]
  
  # Train 10 mlp models
  # From every model save the predictions (forecasts)
  frc_f <- NULL
  
  for (ssn in c(1:10)){
    
    modelMLP <- mlp(x = train_x,
                    y = train_y, 
                    size = (2*ni), maxit = 500, initFunc = "Randomize_Weights", 
                    learnFunc = "SCG", hiddenActFunc = "Act_Logistic", 
                    shufflePatterns = FALSE, linOut = TRUE)
    
    # Predict
    tempin <- data.frame(t(tail(Sinsample, ni)))    # Get the last ni observations from Sinsample data for predict (forecasting)
    frcst <- predict(modelMLP, tempin)
    MLf <- rep(as.numeric(frcst)*(MAX-MIN)+MIN, fh)
    
    frc_f <- rbind(frc_f, MLf)
  }
  
  # Get the median for each prediction from every one of the 10 models
  frc <- unlist(lapply(c(1:fh), function(x) median(frc_f[,x])))    # instead of mean -> median
  
  return(frc)
}

# MLP_local(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), fh = 1, ni = 6)


## 12. Random forest at time series level (local)
RF_local <- function(input, fh, ni){
  
  ## Scale data 
  # MAX <- max(input)
  # MIN <- min(input)
  # Sinsample <- (input-MIN)/(MAX-MIN)
  
  ## Skip scaling (Note: RF doesn't need scaling!)
  Sinsample <- input
  
  # Create training sample
  samplegenerate <- CreateSamples(datasample = Sinsample, xi = ni)
  dftrain <- data.frame(samplegenerate)
  colnames(dftrain) <- c(paste0("X",c(1:ni)),"Y")
  
  #Train model
  modelRF <- randomForest(formula = Y ~ .,  data = dftrain, ntree=500)
  
  # Predict
  tempin <- data.frame(t(tail(Sinsample, ni)))
  frcst <- predict(object = modelRF, newdata = tempin)
  
  MLf <- as.numeric(frcst)                      # MLf: Machine Learning forecast
  # MLf <- as.numeric(frcst)*(MAX-MIN)+MIN
  
  frc <- rep(MLf, fh)
  
  return(frc)
}


# RF_local(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), fh = 1, ni = 7)


## Define benchmarks_f() function to use all previous benchmark methods at once 

# Benchmark function: Takes an element x=time_series_b[[i]] and fh=28 (forecasting horizon)

benchmarks_f <- function(x, fh){
  
  # Test
  # x <- time_series_b[[1]]
  # fh = 23
  
  print(x$item_id)
    
  # Get signal
  input <- x$x
  
  # Get benchmarks names (fm_names: forecasting names / b_names: benchmark names)
  fm_names <- b_names
  
  # Drop the first period where the item wasn't active (This is not necessary because the same process have been done in time_series_b object making)
  start_period <- min(which(input!=0))
  input <- input[start_period:length(input)]
  
  ## Estimate forecasts
  Methods <- NULL
  Methods <- cbind(Methods, Naive(x = input, h = fh, type="simple"))
  Methods <- cbind(Methods, Naive(x = input, h = fh, type="seasonal"))
  Methods <- cbind(Methods, SexpS(x = input, h =fh))
  Methods <- cbind(Methods, MA(x = input, h = fh))
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "classic"))
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "optimized"))
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "sba"))
  Methods <- cbind(Methods, TSB(x = input, h = fh))
  Methods <- cbind(Methods, ADIDA(x = input, h = fh))
  Methods <- cbind(Methods, iMAPA(x = input, h = fh))
  Methods <- cbind(Methods, smooth_es(x = input, h = fh))
  Methods <- cbind(Methods, auto_arima(x = input, h = fh))
  Methods <- cbind(Methods, MLP_local(input = input, fh = fh, ni = 14))
  Methods <- cbind(Methods, RF_local(input = input, fh = fh, ni = 14))
  
  # Forecasting post-processing: Set negatives forecasting values to zero (if any)
  for (i in 1:nrow(Methods)){
    for (j in 1:ncol(Methods)){
      if (Methods[i,j]<0){ Methods[i,j]<-0  } 
    }
  }
  
  # Create Methods data frame
  Methods <- data.frame(Methods)
  colnames(Methods) <- fm_names
  
  Methods$item_id <- x$item_id
  Methods$dept_id <- x$dept_id
  Methods$cat_id <- x$cat_id
  Methods$store_id <- x$store_id
  Methods$state_id <- x$state_id
  Methods$fh <- c(1:fh)
  
  return(Methods)
}


## 05. Define Benchmark methods II (global time series forecasting) -----

## Note:
# Global time series forecasting means that the model gets data from multiple time series as training data,
# but still make predictions for every time series separately in the bottom level.

ML_Global <- function(fh, ni = 12, nwindows = 3){
  
  
  # 1. Define and clean necessary variables 
  # fh = 28
  # ni = 12
  # nwindows = 3
  
  x_train = y_train = x_test <- NULL
  Maxies = Minies <- c()
  
  
  # 2. Create global training dataset
  
  # Note: Every time series of the dataset offers nwindows = 3 rows in the global training dataset (x_train)
  # and 1 window (the last) for the test set
  
  for (i in 1:length(time_series_b)){
    
    # i = 1
    input <- time_series_b[[i]]$x
    
    # Scale data
    MAX <- max(input)
    MIN <- min(input)
    Sinsample <- (input-MIN)/(MAX-MIN)
    Maxies <- c(Maxies, MAX)
    Minies <- c(Minies, MIN)
    
    # Create training sample
    samplegenerate <- CreateSamples(datasample=Sinsample, xi=ni)
    dftest <- data.frame(samplegenerate)
    colnames(dftest) <- c(paste0("X",c(1:ni)),"Y")
    train_x <- as.matrix(dftest[,1:(ncol(dftest)-1)])
    train_y <- dftest[,ncol(dftest)]
    
    # Select a sample of nwindows for x_train global dataset
    select <- sample(x = c(1:nrow(train_x)), size = nwindows, replace = F)
    
    temp <- data.frame(train_x[select,])
    temp$ADI <- stat_total[i,]$ADI
    temp$CV2 <- stat_total[i,]$CV2
    
    x_train <- rbind(x_train, temp)
    y_train <- c(y_train, train_y[select])
    
    # Select the last observation for x_test
    temp <- data.frame(t((tail(time_series_b[[i]]$x, ni)-MIN)/(MAX-MIN)))
    temp$ADI <- stat_total[i,]$ADI
    temp$CV2 <- stat_total[i,]$CV2
    
    x_test <- rbind(x_test, temp)
  }
  
  # Make sure that both train and test set have the same colnames
  colnames(x_test) <- colnames(x_train)
  
  
  # 4. Scale ADI, CV2 both in train and test set. 
  # (SOS: I have to use train set paramers to normalize both train and test set!)
  
  x_train$ADI <- (x_train$ADI-min(x_train$ADI))/(max(x_train$ADI)-min(x_train$ADI))
  x_test$ADI <- (x_test$ADI-min(x_train$ADI))/(max(x_train$ADI)-min(x_train$ADI))
  
  x_train$CV2 <- (x_train$CV2-min(x_train$CV2))/(max(x_train$CV2)-min(x_train$CV2))
  x_test$CV2 <- (x_test$CV2-min(x_train$CV2))/(max(x_train$CV2)-min(x_train$CV2))
  
  
  # 5. MLP
  
  # Make 10 predictions (10 columns) for every one of the time series in the time_series_b dataset (rows in x_test dataset).
  
  frc_f <- NULL
  for (ssn in c(1:10)){
    modelMLP <- mlp(x_train, y_train, 
                    size = (2*ncol(x_train)), maxit = 500,initFunc = "Randomize_Weights", 
                    learnFunc = "SCG", hiddenActFunc = "Act_Logistic", 
                    shufflePatterns = FALSE, linOut = TRUE) 
    
    frc_f <- cbind(frc_f, as.numeric(predict(modelMLP, x_test)*(Maxies-Minies)+Minies))
  }
  
  frc_f <- unlist(lapply(c(1:nrow(frc_f)), function(x) median(frc_f[x,])))    # Take the median of the 10 predictions made for every time series
  MLP_g <- unlist(lapply(c(1:length(frc_f)), function(x) rep(frc_f[x], fh)))  # Repeat each value as many times as the fh
  
  
  # 6. RF
  dftest <- cbind(x_train, y_train)
  colnames(dftest)[ncol(dftest)] <- "Y"
  modelRF <- randomForest(formula = Y ~ .,  data = dftest, ntree=500)
  frc_f <- as.numeric(predict(modelRF, x_test)*(Maxies-Minies)+Minies)
  RF_g <- unlist(lapply(c(1:length(frc_f)), function(x) rep(frc_f[x], fh)))
  
  output <- data.frame(MLP_g, RF_g)
  
  return(output)
}



## 06. Prepare the series to be predicted (create time_series_b object) ----

# 1A. Don't needed to re-calculate the time_series_b (bottom) object
time_series_b <- readRDS("data/processed/time_series_b.rds")

# 1B. Calculate time_series_b object
time_series_b <- NULL

for (tsid in 1:nrow(sales)){
  
  # tsid = 1
  
  # Fetch the series to be forecasted (ex_sales = προηγούμενες πωλήσεις)
  ex_sales <- sales[tsid,]
  
  # Prepare the time series
  sales_train <- as.numeric(ex_sales[,7:ncol(ex_sales)])      # Drop first 7 columns, because contains ids information
  starting_period <- min(which(sales_train !=0))              # Keep only valid observations (first non-zero demand value and after)
  input <- sales_train[starting_period:length(sales_train)]   # Create input vector
  
  
  # Add new time series and the metadata (appropriate ids) in the time_series_b list (b = bottom)
  time_series_b[length(time_series_b)+1] <- list(list(x = input, 
                                                      item_id = ex_sales$item_id,
                                                      dept_id=ex_sales$dept_id, 
                                                      cat_id = ex_sales$cat_id,
                                                      store_id = ex_sales$store_id, 
                                                      state_id = ex_sales$state_id)
                                                 )
}

# Remove helper variables
input = sales_train = ex_sales = starting_period = tsid <- NULL

# Check time_time_series_b object (list of lists)
length(time_series_b)
str(time_series_b)

saveRDS(object = time_series_b, file = "data/processed/time_series_b.rds")


## 07. Estimate statistics and the dollar sales used for weighting ----

## 1A. Don't needed to re-calculate the stat_total file
stat_total <- readRDS("data/processed/stat_total.rds")
View(stat_total)

## 1B. Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 6, type = "SOCK"))

stat_total <- foreach(tsi=1:length(time_series_b), .combine='rbind') %dopar% statistics(tsi)
stat_total

# Save stat_total
saveRDS(object = stat_total, file = "data/processed/stat_total.rds")
write.csv(x = stat_total, file = "data/processed/stat_total.rds")


## 08. Get forecasts for bottom-up models ----

# Read time_series_b
time_series_b <- readRDS(file = "data/processed/time_series_b.rds")

## 1. Local time series (get training data just from one time series and make predicitons for this time series)

# Define benchmark names to be used from benchmarks_f() function
b_names <- c("Naive", "sNaive",
             "SES", "MA", 
             "Croston", "optCroston",
             "SBA", "TSB", 
             "ADIDA",
             "iMAPA",
             "ES_bu",
             "ARIMA_bu",
             "MLP_l",
             "RF_l")

# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 6, type = "SOCK", outfile = ""))

# Get forecasts for local models
frc_total <- foreach(tsi = 1:12,
                     .combine = 'rbind', 
                     .packages = c('zoo','randomForest','RSNNS','forecast','smooth')) %dopar% benchmarks_f(x = time_series_b[[tsi]], fh = 28)

saveRDS(object = frc_total, file = "data/processed/frc_total_l.RDS")
frc_total <- readRDS("data/processed/frc_total_l.RDS")


## 2. Global time series (get training data from multiple time series, but make predictions for every time series in bottom level)

# Get forecasts for global models
frc_total_g <- ML_Global(fh = 28, ni = 12, nwindows = 3)


## 3. Combine forecasts from local and global models
frc_total$MLP_g <- frc_total_g$MLP_g
frc_total$RF_g <- frc_total_g$RF_g
frc_total_g <- NULL

saveRDS(object = frc_total, file = "data/processed/frc_total_bu.RDS")
frc_total <- readRDS("data/processed/frc_total_bu.RDS")

## 09. Get forecasts for top-down models ----

## Count the total sales for each day for all products in all stores (aggreagation level 1)
insample_top <- ts(as.numeric(colSums(sales[,7:ncol(sales)])), frequency = 7)

## Get calendar events
x_var <- calendar
x_var$snap <- x_var$snap_CA+x_var$snap_WI+x_var$snap_TX
x_var$holiday <- 0
x_var[is.na(x_var$event_type_1)==F,]$holiday <- 1
x_var <- x_var[,c("snap","holiday")]

## 1. Exponential smoothing
es_f <- es(insample_top, h=28)$forecast 

## 2. Exponential smoothing with external variables
esx_f <- es(insample_top, xreg=x_var, h=28)$forecast

## 3. ARIMA
arima_f <- forecast(auto.arima(insample_top), h=28)$mean 

## 4. ARIMA with external variables
arimax_f <- forecast(auto.arima(insample_top,
                                xreg = as.matrix(head(x_var, length(insample_top)))),
                     h=28, xreg=as.matrix(tail(x_var, 28)))$mean 



## Calculate historical proportion for TOP-DOWN approach 

# Note: Divide the total sales of each product for the last 28 days, with the total sales of the last 28 days

proportions <- unlist(lapply(X = c(1:length(time_series_b)),    # for every row in time_series_b ie for every time series
                             FUN = function(x) {
                               sum(tail(as.numeric(sales[x,7:ncol(sales)]),28))/sum(tail(insample_top, 28))
                             }
                            ))

# Mulpiply the forecasting values for total predictions, with historical proportions to get the forecasting for each item_id
frc_total$ES_td <- unlist(lapply(c(1:length(time_series_b)), function(x) es_f*proportions[x]))
frc_total$ESX <- unlist(lapply(c(1:length(time_series_b)), function(x) esx_f*proportions[x]))
frc_total$ARIMA_td <- unlist(lapply(c(1:length(time_series_b)), function(x) arima_f*proportions[x]))
frc_total$ARIMAX <- unlist(lapply(c(1:length(time_series_b)), function(x) arimax_f*proportions[x]))

# Get forecasts for combination approaches
frc_total$Com_b <- (frc_total$ES_bu+frc_total$ARIMA_bu)/2
frc_total$Com_t <- (frc_total$ES_td+frc_total$ARIMA_td)/2
frc_total$Com_tb <- (frc_total$ES_bu+frc_total$ES_td)/2
frc_total$Com_lg <- (frc_total$MLP_l+frc_total$MLP_g)/2


## 10. Combine all forecasts (from 8,9) ----

## Add Top-down forecastings in frc_total (forecasting total object)

# b_names = benchmark names
b_names <- c("Naive", "sNaive", "SES", "MA", 
             "Croston", "optCroston","SBA", "TSB", 
             "ADIDA", "iMAPA",
             "ES_bu", "ARIMA_bu",
             "MLP_l", "RF_l", "MLP_g", "RF_g",
             "ES_td","ESX","ARIMA_td","ARIMAX", # ARIMAX can't fit in the data (remove it before define the b_names vector )
             "Com_b","Com_t","Com_tb","Com_lg")

frc_total <- frc_total[,c(b_names,"item_id","dept_id","cat_id","store_id","state_id","fh")]

saveRDS(object = frc_total, file = "data/processed/frc_total.RDS")



## 11. Evaluate forecasts ----

## Required datasets to calculate the WRMSSE:

# 1. forecastings (frc_total)
frc_total <- readRDS("data/forecastings/frc_top_12_item_ids.RDS")
colnames(frc_total)
View(frc_total)

# 2. stat_total (volume sales for weigthing)
stat_total <- readRDS("data/processed/stat_total.rds")
stat_total <- stat_total[1:12, ]

# 3. sales_test_validation dataset (for ground truth values)
sales_out <- read.csv("data/raw/sales_test_validation.csv", stringsAsFactors = F)

# 4. sales_train_validation dataset (past sales values for scaling)
time_series_b <- readRDS("data/processed/time_series_b.rds")

 
## Level 12: Unit sales of product x, aggregated for each store - 30,490

errors_total <- NULL

# Calculate the error for every time series

for (tsid in 1:nrow(sales)){ # 1:nrow(sales)
  
  # tsid = 1  
  
  # Historical sales demand (needed for the WRMSSE calculations)
  insample_d <- time_series_b[[tsid]] 
  insample <- insample_d$x
  
  # Ground_truth sales demand for this tsid for error calulcations
  outsample <- as.numeric(sales_out[tsid,6:ncol(sales_out)])
  
  # Get the forecastings for this tsid from frc_total (Use item_id and store_id for unique identification)
  Methods <- frc_total[(frc_total$item_id==insample_d$item_id) & (frc_total$store_id==insample_d$store_id),]  
  
  # Calculate the RMSSE for every column in Methods (ie for the forecastings for every different model)
  RMSSE <- c()
  for (j in 1:length(b_names)){ # For every benchmark name
    RMSSE <- c(RMSSE, sqrt(mean((Methods[,j]-outsample)^2)/ mean(diff(insample)^2)) ) # scale MSE using first differences
  }
  
  errors <- data.frame(t(RMSSE)) 
  colnames(errors) <- b_names
  errors$id <- tsid 
  errors$sales <- stat_total[tsid,]$dollar_sales
  row.names(errors) <- NULL
  errors_total <- rbind(errors_total, errors)
}

# For every method (column of errors_total) calculate the total score for all time series (rows),
# weighting the RMSSE based on sales column
WRMSSE_12 <- c()

for (mid in 1:length(b_names)){
  WRMSSE_12 <- c(WRMSSE_12,
                 sum( errors_total[,mid] * errors_total$sales / sum(errors_total$sales) )
                 #  column with errors for all time series from specific method * sales for all time series / total sales of all time_series (item_ids)
                 )}

names(WRMSSE_12) <- b_names
WRMSSE_12

## My idea: Use different forecasting method for every time series.
# Which method has the minimum error for every time_series?
# Use a time series cross validation to select the best method for every time series
View(errors_total)
apply(X = errors_total, MARGIN = 1, function(x){b_names[which.min(x)]})

best_method_ids <- apply(X = errors_total, MARGIN = 1, function(x){which.min(x)})

errors_vec <- c()
for(i in 1:nrow(errors_total)){ # For every tsid in errors_total get the error from the best method
  j <- best_method_ids[i]
  errors_vec <- c(errors_vec, errors_total[i,j])
}

sum( errors_vec * errors_total$sales / sum(errors_total$sales) )  # WRMSSE 


## Level 1: Unit sales of all products, aggregated for all stores/states	- 1

insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_1.rds")
outsample <- as.numeric(colSums(sales_out[1:12,6:ncol(sales_out)]))

insample <- as.numeric(colSums(sales[,7:ncol(sales)]))
outsample <- as.numeric(colSums(sales_out[,6:ncol(sales_out)]))

# Sum the partial forecastings for every item_id, for each one of the fh=28 days, to get the forcasting for the aggregation level 1
Methods <- ddply(.data = frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh")],  # data with forecastings without columns with ids
                 .variables =  .(fh),                                                 # split .data using the fh column values
                 .fun = colwise(sum))                                                 # in every subset apply colwise sum

Methods$fh <- NULL

# For every method (column of Methods) calculate the error,
# weighting the RMSSE based on outsample vector
WRMSSE_1 <- c()

for (j in 1:length(b_names)){
  WRMSSE_1 <- c(WRMSSE_1,
                sqrt(mean((Methods[,j]-outsample)^2)/mean(diff(insample)^2))
                )
}

WRMSSE_1


## Level 2: Unit sales of all products, aggregated for each State - 3
insample <- sales
insample$id = insample$item_id = insample$dept_id = insample$cat_id = insample$store_id <- NULL

# Get the insample time series for aggregation level 2
insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_2.rds")
insample <- ddply(.data = insample,          
                  .variables = .(state_id),  # split per state_id 
                  .fun = colwise(sum))       # column some the items for the same state

outsample <- sales_out[1:12,]
outsample <- sales_out  
outsample$item_id = outsample$dept_id = outsample$cat_id = outsample$store_id <- NULL
outsample <- ddply(.data = outsample,
                   .variables = .(state_id),
                   .fun =  colwise(sum))

Methods <- ddply(.data = frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","state_id")],
                 .variables = .(state_id, fh), 
                 .fun = colwise(sum))

Methods$fh <- NULL

WRMSSE_2 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,2:ncol(insample)])
    sstart <- data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,2:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[temp_w$state_id==insample[i,]$state_id,]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[Methods$state_id==insample[i,]$state_id,]
    temp_frc$state_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_2 <- c(WRMSSE_2, temp_error)
}

WRMSSE_2


## Level 3: Unit sales of all products, aggregated for each store - 10
insample <- sales
insample$id = insample$item_id = insample$dept_id = insample$cat_id = insample$state_id <- NULL

# Get the insample time series for aggregation level 3
insample <- ddply(.data = insample,
                  .variables = .(store_id),
                  .fun = colwise(sum))

outsample <- sales_out
outsample$item_id = outsample$dept_id = outsample$cat_id = outsample$state_id <- NULL
outsample <- ddply(.data = outsample,
                   .variables = .(store_id),
                   .fun =  colwise(sum))

Methods <- ddply(.data = frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","store_id")],
                 .variables =  .(store_id,fh),
                 .fun = colwise(sum))

Methods$fh <- NULL

WRMSSE_3 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,2:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,2:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[temp_w$store_id==insample[i,]$store_id,]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[Methods$store_id==insample[i,]$store_id,]
    temp_frc$store_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_3 <- c(WRMSSE_3, temp_error)
}


## Level 4:  Unit sales of all products, aggregated for each category - 3
insample <- sales
insample$id = insample$item_id = insample$dept_id = insample$store_id = insample$state_id <- NULL
insample <- ddply(.data = insample,
                  .variables = .(cat_id),
                  .fun = colwise(sum))

outsample <- sales_out
outsample$item_id = outsample$dept_id = outsample$store_id = outsample$state_id <- NULL
outsample <- ddply(.data = outsample, 
                   .variables = .(cat_id),
                   .fun = colwise(sum))

Methods <- ddply(.data = frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","cat_id")],
                 .variables = .(cat_id,fh), 
                 .fun = colwise(sum))
Methods$fh <- NULL

WRMSSE_4 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,2:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,2:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[temp_w$cat_id==insample[i,]$cat_id,]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[Methods$cat_id==insample[i,]$cat_id,]
    temp_frc$cat_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_4 <- c(WRMSSE_4, temp_error)
}


## Level 5:  Unit sales of all products, aggregated for each department - 7
insample <- sales
insample$id = insample$item_id = insample$cat_id = insample$store_id = insample$state_id <- NULL
insample <- ddply(.data = insample,
                  .variables = .(dept_id),
                  .fun = colwise(sum))

outsample <- sales_out
outsample$item_id = outsample$cat_id = outsample$store_id = outsample$state_id <- NULL
outsample <- ddply(.data = outsample,
                   .variables = .(dept_id),
                   .fun = colwise(sum))

Methods <- ddply(.data = frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","dept_id")],
                 .variables = .(dept_id,fh),
                 .fun = colwise(sum))

Methods$fh <- NULL

WRMSSE_5 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,2:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,2:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[temp_w$dept_id==insample[i,]$dept_id,]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[Methods$dept_id==insample[i,]$dept_id,]
    temp_frc$dept_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_5 <- c(WRMSSE_5, temp_error)
}


## Level 6: Unit sales of all products, aggregated for each State and category - 9
insample <- sales
insample$id = insample$item_id = insample$store_id = insample$dept_id <- NULL
insample <- ddply(.data = insample,
                  .variables = .(cat_id, state_id),
                  .fun = colwise(sum))

outsample <- sales_out
outsample$item_id = outsample$store_id = outsample$dept_id <- NULL
outsample <- ddply(.data = outsample, 
                   .variables = .(cat_id, state_id),
                   .fun = colwise(sum))

Methods <- ddply(.data = frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","cat_id","state_id")],
                 .variables = .(cat_id,state_id,fh), 
                 .fun = colwise(sum))

Methods$fh <- NULL

WRMSSE_6 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,3:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,3:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[(temp_w$cat_id==insample[i,]$cat_id)&(temp_w$state_id==insample[i,]$state_id),]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[(Methods$cat_id==insample[i,]$cat_id)&(Methods$state_id==insample[i,]$state_id),]
    temp_frc$state_id = temp_frc$cat_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_6 <- c(WRMSSE_6, temp_error)
}


## Level 7:  Unit sales of all products, aggregated for each State and department - 21
insample <- sales
insample$id = insample$item_id = insample$store_id = insample$cat_id <- NULL
insample <- ddply(.data = insample,
                  .variables =  .(dept_id, state_id),
                  .fun =  colwise(sum))


outsample <- sales_out
outsample$item_id = outsample$store_id = outsample$cat_id <- NULL
outsample <- ddply(.data = outsample,
                   .variables = .(dept_id, state_id),
                   .fun = colwise(sum))


Methods <- ddply(.data = frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","dept_id","state_id")],
                 .variables =  .(dept_id,state_id,fh),
                 .fun =  colwise(sum))

Methods$fh <- NULL

WRMSSE_7 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,3:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,3:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[(temp_w$dept_id==insample[i,]$dept_id)&(temp_w$state_id==insample[i,]$state_id),]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[(Methods$dept_id==insample[i,]$dept_id)&(Methods$state_id==insample[i,]$state_id),]
    temp_frc$state_id = temp_frc$dept_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_7 <- c(WRMSSE_7, temp_error)
}


## Level 8: Unit sales of all products, aggregated for each store and category - 30
insample <- sales
insample$id = insample$item_id = insample$state_id = insample$dept_id <- NULL
insample <- ddply(.data = insample,
                  .variables = .(cat_id, store_id),
                  .fun =  colwise(sum))

outsample <- sales_out
outsample$item_id = outsample$state_id = outsample$dept_id <- NULL
outsample <- ddply(outsample, .(cat_id, store_id), colwise(sum))

Methods <- ddply(frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","cat_id","store_id")], .(cat_id,store_id,fh), colwise(sum))
Methods$fh <- NULL

WRMSSE_8 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,3:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,3:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[(temp_w$cat_id==insample[i,]$cat_id)&(temp_w$store_id==insample[i,]$store_id),]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[(Methods$cat_id==insample[i,]$cat_id)&(Methods$store_id==insample[i,]$store_id),]
    temp_frc$store_id = temp_frc$cat_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_8 <- c(WRMSSE_8, temp_error)
}


## Level 9:  Unit sales of all products, aggregated for each store and department - 70
insample <- sales
insample$id = insample$item_id = insample$state_id = insample$cat_id <- NULL
insample <- ddply(.data = insample,
                  .variables = .(dept_id, store_id),
                  .fun =  colwise(sum))

outsample <- sales_out
outsample$item_id = outsample$state_id = outsample$cat_id <- NULL
outsample <- ddply(outsample, .(dept_id, store_id), colwise(sum))

Methods <- ddply(frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","dept_id","store_id")], .(dept_id,store_id,fh), colwise(sum))
Methods$fh <- NULL

WRMSSE_9 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,3:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,3:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[(temp_w$dept_id==insample[i,]$dept_id)&(temp_w$store_id==insample[i,]$store_id),]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[(Methods$dept_id==insample[i,]$dept_id)&(Methods$store_id==insample[i,]$store_id),]
    temp_frc$store_id = temp_frc$dept_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_9 <- c(WRMSSE_9, temp_error)
}


## Level 10: Unit sales of product x, aggregated for all stores/states - 3,049
insample <- sales
insample$id = insample$dept_id = insample$state_id = insample$cat_id = insample$store_id <- NULL
insample <- ddply(.data = insample,
                  .variables = .(item_id),
                  .fun =  colwise(sum))

outsample <- sales_out
outsample$dept_id = outsample$state_id = outsample$cat_id = outsample$store_id <- NULL
outsample <- ddply(outsample, .(item_id), colwise(sum))

Methods <- ddply(frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","item_id")], .(item_id,fh), colwise(sum))
Methods$fh <- NULL

WRMSSE_10 <- c()
for (j in 1:length(b_names)){
  temp_error <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,2:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,2:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[temp_w$item_id==insample[i,]$item_id,]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[Methods$item_id==insample[i,]$item_id,]
    temp_frc$item_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_10 <- c(WRMSSE_10, temp_error)
}


## Level 11: Unit sales of product x, aggregated for each State - 9,147
insample <- sales
insample$id = insample$dept_id = insample$cat_id = insample$store_id <- NULL

cl = registerDoSNOW(makeCluster(spec = 4, type = "SOCK"))
insample <- ddply(.data = insample, 
                  .variables = .(item_id, state_id),
                  .fun = colwise(sum), .parallel = T)
stopCluster(cl)

outsample <- sales_out
outsample$dept_id = outsample$cat_id = outsample$store_id <- NULL
outsample <- ddply(outsample, .(item_id, state_id), colwise(sum))

Methods <- ddply(frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","item_id","state_id")], .(item_id,state_id,fh), colwise(sum))
Methods$fh <- NULL

WRMSSE_11 <- c()
for (j in 1:length(b_names)){
  temp_error  <- 0
  for (i in 1:nrow(insample)){
    temp_in <- as.numeric(insample[i,3:ncol(insample)])
    sstart<-data.frame(temp_in, c(1:length(temp_in)))
    sstart <- min(sstart[sstart$temp_in>0,2])
    temp_in <- temp_in[sstart:length(temp_in)]
    
    temp_out <- as.numeric(outsample[i,3:ncol(outsample)])
    
    temp_w <- cbind(errors_total,stat_total)
    temp_w <- sum(temp_w[(temp_w$item_id==insample[i,]$item_id)&(temp_w$state_id==insample[i,]$state_id),]$dollar_sales)/sum(temp_w$dollar_sales)
    
    temp_frc <- Methods[(Methods$item_id==insample[i,]$item_id)&(Methods$state_id==insample[i,]$state_id),]
    temp_frc$item_id = temp_frc$state_id <- NULL
    temp_frc <- as.numeric(temp_frc[,j])
    temp_error <- temp_error + sqrt(mean((temp_frc-temp_out)^2)/mean(diff(temp_in)^2))*temp_w
  }
  WRMSSE_11 <- c(WRMSSE_11, temp_error)
}


## Combine WRMSSE (errors) for every aggregation level
WRMSSE <- rbind(WRMSSE_1, WRMSSE_2, WRMSSE_3,
                WRMSSE_4, WRMSSE_5, WRMSSE_6,
                WRMSSE_7, WRMSSE_8, WRMSSE_9,
                WRMSSE_10, WRMSSE_11, WRMSSE_12)

WRMSSE <- rbind(WRMSSE, colMeans(WRMSSE))

row.names(WRMSSE) <- c("Total", "State", "Store",
                       "Category", "Department", "State-Category",
                       "State-Department", "Store-Category", "Store-Department",
                       "Product", "Product-State", "Product-Store", "Average")

# Save errors
  write.csv(WRMSSE, "data/processed/WRMSSE.csv")


## 12. Export benchmarks' forecasts in Kaggle's format ----

## Import forecastings
frc_total <- readRDS("data/forecastings/frc_top_12_item_ids.RDS")
View(frc_total)

## benchmark names
b_names <- c("Naive", "sNaive", "SES", "MA", 
             "Croston", "optCroston","SBA", "TSB", 
             "ADIDA", "iMAPA",
             "ES_bu", "ARIMA_bu",
             "MLP_l", "RF_l", "MLP_g", "RF_g",
             "ES_td","ESX","ARIMA_td","ARIMAX", # ARIMAX can't fit in the data (remove it from the b_names vector )
             "Com_b","Com_t","Com_tb","Com_lg")


## For every method (mid) in frc_total (column), create a submission file with the appropriate format
for (mid in 1:length(b_names)){ # mid: method id
  
  # Get submission file for method id
  submission <- frc_total[,c("item_id", "store_id", b_names[mid], "fh")]
  colnames(submission)[1:2] <- c("Agg_Level_1", "Agg_Level_2")
  
  # Create new columns F1, F2, ..., F28
  submission$F7 = submission$F6 = submission$F5 = submission$F4 = submission$F3 = submission$F2 = submission$F1 <- NA
  submission$F14 = submission$F13 = submission$F12 = submission$F11 = submission$F10 = submission$F9 = submission$F8 <- NA
  submission$F21 = submission$F20 = submission$F19 = submission$F18 = submission$F17 = submission$F16 = submission$F15 <- NA
  submission$F28 = submission$F27 = submission$F26 = submission$F25 = submission$F24 = submission$F23 = submission$F22 <- NA
  
  l1_unique <- unique(submission$Agg_Level_1)  # Unique item ids
  l2_unique <- unique(submission$Agg_Level_2)  # Unique store ids
  frc <- NULL
  
  # Transpose the approprate column (3) to row
  
  for (l2 in l2_unique){   # l2 = l2_unique[1]
    for (l1 in l1_unique){ # l1 = l1_unique[1]
      temp <- submission[(submission$Agg_Level_1==l1)&(submission$Agg_Level_2==l2),]
      temp[1, 5:32] <- temp[,3]
      frc <- rbind(frc, data.frame(l1, l2, temp[1, 5:32]))
    }
  }
  
  # Create data frame with the rigth format (columns and column names)
  colnames(frc)[1:2] <- c("Agg_Level_1", "Agg_Level_2")
  frc$id <- paste0(frc$Agg_Level_1,"_",frc$Agg_Level_2,"_validation")
  frc <- frc[,c("id", colnames(frc)[3:30])]
  write.csv(frc, row.names = FALSE, paste0("BF_", b_names[mid],".csv")) 
}

