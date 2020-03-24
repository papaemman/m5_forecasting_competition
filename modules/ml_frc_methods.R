###########################################
#                                         #
#  Machine Learning forecasting methods   #
#                                         #
###########################################

# // Machine Learning Benchmark methods: local time series forecasting //

# Note: 
# Every function takes as input a univarite signal and produce forecastings for the next fh days


## 01. Multilayer Perceptron (at time series level - local) ----

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


## 02. Random forest (at time series level - local) ----

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



## 03. MLP and RF (global time series forecasting) -----

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


