################################################
#                                              #
#  Machine Learning forecasting methods local  #
#                                              #
################################################

# // Machine Learning Benchmark methods: local time series forecasting //

# Note: 
# Every function takes as input a univarite signal and produce forecastings for the next fh days


## 01 A. Multilayer Perceptron (at time series level - local) ----

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

# MLP_local(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), fh = 5, ni = 6)
# MLP_local(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7, 1.0000005), fh = 5, ni = 6)

## 01 B. Multilayer Perceptron v2 (at time series level - local) ----

# MLP_local_v2
MLP_local_v2 <- function(input, fh = 5, ni = 6){
  
  frc <- c()
  for (i in 1:fh) {
    input <- c(input, MLP_local(input = input, fh = 1, ni = ni))
  }
  
  return(tail(input, fh))
}

# MLP_local(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), fh = 5, ni = 6)
# MLP_local_v2(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), fh = 5, ni = 6)


## 02 A. Random forest (at time series level - local) ----

RF_local <- function(input, fh, ni, scale_data = F){
  
  ## Scale data 
  if(scale_data){
    MAX <- max(input)
    MIN <- min(input)
    Sinsample <- (input-MIN)/(MAX-MIN)
  } else { # Skip scaling (Note: RF doesn't need scaling!)
    Sinsample <- input
  }
  
  # Create training sample
  samplegenerate <- CreateSamples(datasample = Sinsample, xi = ni)
  dftrain <- data.frame(samplegenerate)
  colnames(dftrain) <- c(paste0("X",c(1:ni)),"Y")
  
  # Train model
  set.seed(33)
  modelRF <- randomForest(formula = Y ~ .,  data = dftrain, ntree=500)
  
  # Predict
  tempin <- data.frame(t(tail(Sinsample, ni)))
  frcst <- predict(object = modelRF, newdata = tempin)
  
  MLf <- as.numeric(frcst)                      # MLf: Machine Learning forecast
  # MLf <- as.numeric(frcst)*(MAX-MIN)+MIN
  
  frc <- rep(MLf, fh)
  
  return(frc)
}

# RF_local(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), fh = 4, ni = 7)
# RF_local(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7, 2.833467), fh = 4, ni = 7)


## 02 B. Random forest v2 (at time series level - local) ----

RF_local_v2 <- function(input, fh = 5, ni, scale_data = F){
  
  frc <- c()
  for (i in 1:fh) {
    input <- c(input, RF_local(input = input, fh = 1, ni = ni))
  }
  
  return(tail(input, fh))
}

# RF_local(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), fh = 4, ni = 7)
# RF_local_v2(input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7), fh = 4, ni = 7)


