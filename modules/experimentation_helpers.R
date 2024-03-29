######################################
#                                    #
#  Experimentation helper functions  #
#                                    #
######################################

# Note: The following functions will be repeatetively for every local time series


# // COPY THE wrappper_frc_methods_template FUNCTION TO CREATE A NEW ONE // # 

## //NEW// Function: wrapper_frc_methods(x, fh, b_names), to use any forecasting methods for every local time series ----
  
wrapper_frc_methods_test <- function(x, fh, b_names){
  
  # x <- time_series_b[[1]]
  # fh = 28
  # b_names <- c("Naive")
  
  # Print store and item id
  cat("\r", paste("Store id:", x$store_id, "| Item id:", x$item_id))
  
  # Get signal
  input <- x$x
  
  # Get benchmarks names (fm_names: forecasting methods names, b_names: benchmark names)
  fm_names <- b_names
  
  # Drop the first period where the item wasn't active (This is not necessary because the same process have been done in time_series_b object making)
  start_period <- min(which(input!=0))
  input <- input[start_period:length(input)]
  
  ## Estimate forecasts
  Methods <- NULL
  
  Methods <- cbind(Methods, Naive(x = input, h = fh, type="simple"))          # 1.  Naive
  
  
  ## Forecasting post-processing: Set negatives forecasting values to zero (if any)
  for (i in 1:nrow(Methods)){
    for (j in 1:ncol(Methods)){
      if (Methods[i,j]<0){ Methods[i,j]<-0  } 
    }
  }
  
  # Create Methods data frame
  Methods <- data.frame(Methods)
  colnames(Methods) <- fm_names
  
  # Save ids in Methods data frame
  Methods$item_id <- x$item_id
  Methods$dept_id <- x$dept_id
  Methods$cat_id <- x$cat_id
  Methods$store_id <- x$store_id
  Methods$state_id <- x$state_id
  Methods$fh <- c(1:fh)
  
  return(Methods)
}



## 00. Function: wrapper_frc_methods_template(x, fh, b_names), to use forecasting methods for every local time series ----

wrapper_frc_methods_template <- function(x, fh, b_names){
  
  # x <- time_series_b[[1]]
  # fh = 28
  # b_names <- c("Naive")
  
  # Print store and item id
  cat("\r", paste("Store id:", x$store_id, "| Item id:", x$item_id))
  
  # Get signal
  input <- x$x
  
  # Get benchmarks names (fm_names: forecasting methods names, b_names: benchmark names)
  fm_names <- b_names
  
  # Drop the first period where the item wasn't active (This is not necessary because the same process have been done in time_series_b object making)
  start_period <- min(which(input!=0))
  input <- input[start_period:length(input)]
  
  ## Estimate forecasts
  Methods <- NULL

  Methods <- cbind(Methods, Naive(x = input, h = fh, type="simple"))          # 1.  Naive
  Methods <- cbind(Methods, Naive(x = input, h = fh, type="seasonal"))        # 2.  Seasonal Naive
  Methods <- cbind(Methods, SexpS(x = input, h =fh))                          # 3.  Simple exponential smoothing
  Methods <- cbind(Methods, MA(x = input, h = fh))                            # 4.  Moving average
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "classic"))     # 5.  Croston's method classic
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "optimized"))   # 6.  Croston's method optimized
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "sba"))         # 7.  Croston's method sba
  Methods <- cbind(Methods, TSB(x = input, h = fh))                           # 8.  TSB method 
  Methods <- cbind(Methods, ADIDA(x = input, h = fh))                         # 9.  ADIDA
  Methods <- cbind(Methods, iMAPA(x = input, h = fh))                         # 10. iMAPA
  Methods <- cbind(Methods, smooth_es(x = input, h = fh))                     # 11. smooth_es
  Methods <- cbind(Methods, auto_arima(x = input, h = fh))                    # 12. auto_arima
  Methods <- cbind(Methods, MLP_local(input = input, fh = fh, ni = 14))       # 13. MLP_local
  Methods <- cbind(Methods, RF_local(input = input, fh = fh, ni = 14))        # 14. RF_local
  
  Methods <- cbind(Methods, prophet_frc(x = input, h = fh))                   # 15. Prophet
  Methods <- cbind(Methods, MA_v2(x = input, h = fh))                         # 16. MA v2 (recursive)
  Methods <- cbind(Methods, MLP_local_v2(input = input, fh = fh, ni = 14))    # 17. MLP_local_v2   
  Methods <- cbind(Methods, RF_local_v2(input  = input, fh = fh, ni = 14))    # 18. RF_local_v2
  
  
  ## Forecasting post-processing: Set negatives forecasting values to zero (if any)
  for (i in 1:nrow(Methods)){
    for (j in 1:ncol(Methods)){
      if (Methods[i,j]<0){ Methods[i,j]<-0  } 
    }
  }
  
  # Create Methods data frame
  Methods <- data.frame(Methods)
  colnames(Methods) <- fm_names
  
  # Save ids in Methods data frame
  Methods$item_id <- x$item_id
  Methods$dept_id <- x$dept_id
  Methods$cat_id <- x$cat_id
  Methods$store_id <- x$store_id
  Methods$state_id <- x$state_id
  Methods$fh <- c(1:fh)
  
  return(Methods)
}


## 01. Function: prophet_wrapper(x, fh, b_names)  -----

prophet_wrapper <- function(x, fh, b_names){
  
  # x <- time_series_b[[1]]
  # fh = 28
  
  # Print item id 
  # TODO Test cat("/r", paste("store_id:" x$store_id, "| item_id", x$item_id))
  print(x$item_id)
  
  # Get signal
  input <- x$x
  
  # Get benchmarks names (fm_names: forecasting methods names, b_names: benchmark names)
  fm_names <- b_names
  
  # Drop the first period where the item wasn't active (This is not necessary because the same process have been done in time_series_b object making)
  start_period <- min(which(input!=0))
  input <- input[start_period:length(input)]
  
  ## Estimate forecasts
  Methods <- NULL
  Methods <- cbind(Methods, prophet_frc(x = input, h = fh))
  
  ## Forecasting post-processing: Set negatives forecasting values to zero (if any)
  for (i in 1:nrow(Methods)){
    for (j in 1:ncol(Methods)){
      if (Methods[i,j]<0){ Methods[i,j]<-0  } 
    }
  }
  
  # Create Methods data frame
  Methods <- data.frame(Methods)
  colnames(Methods) <- fm_names
  
  # Save ids in Methods data frame
  Methods$item_id <- x$item_id
  Methods$dept_id <- x$dept_id
  Methods$cat_id <- x$cat_id
  Methods$store_id <- x$store_id
  Methods$state_id <- x$state_id
  Methods$fh <- c(1:fh)
  
  return(Methods)
}


## 02. Function: naive_frc_method(x, fh, b_names), to use forecasting methods for every local time series ----

naive_frc_method <- function(x, fh, b_names){
  
  # x <- time_series_b[[1]]
  # fh = 28
  # b_names <- "Naive"
  
  # Print item id
  print(x$item_id)
  
  # Get signal
  input <- x$x
  
  # Get benchmarks names (fm_names: forecasting methods names, b_names: benchmark names)
  fm_names <- b_names
  
  # Drop the first period where the item wasn't active (This is not necessary because the same process have been done in time_series_b object making)
  start_period <- min(which(input!=0))
  input <- input[start_period:length(input)]
  
  ## Estimate forecasts
  Methods <- NULL
  Methods <- cbind(Methods, Naive(x = input, h = fh, type="simple"))

  
  ## Forecasting post-processing: Set negatives forecasting values to zero (if any)
  for (i in 1:nrow(Methods)){
    for (j in 1:ncol(Methods)){
      if (Methods[i,j]<0){ Methods[i,j]<-0  } 
    }
  }
  
  # Create Methods data frame
  Methods <- data.frame(Methods)
  colnames(Methods) <- fm_names
  
  # Save ids in Methods data frame
  Methods$item_id <- x$item_id
  Methods$dept_id <- x$dept_id
  Methods$cat_id <- x$cat_id
  Methods$store_id <- x$store_id
  Methods$state_id <- x$state_id
  Methods$fh <- c(1:fh)
  
  return(Methods)
}


## 03. Function: benchmarks_f(x, fh, b_names), to use all benchmark methods for local time series at once ----

# Benchmark function: Takes an element x=time_series_b[[i]] and fh=28 (forecasting horizon)
#                     and produce forecasting for every forecasting method

benchmarks_f <- function(x, fh, b_names){
  
  # Test
  # x <- time_series_b[[1]]
  # fh = 23
  
  # Print item id
  print(x$item_id)
  
  # Get signal
  input <- x$x
  
  # Get benchmarks names (fm_names: forecasting methods names, b_names: benchmark names)
  fm_names <- b_names
  
  # Drop the first period where the item wasn't active (This is not necessary because the same process have been done in time_series_b object making)
  start_period <- min(which(input!=0))
  input <- input[start_period:length(input)]
  
  ## Estimate forecasts
  Methods <- NULL
  Methods <- cbind(Methods, Naive(x = input, h = fh, type="simple"))          # 1.  Naive
  Methods <- cbind(Methods, Naive(x = input, h = fh, type="seasonal"))        # 2.  Seasonal Naive
  Methods <- cbind(Methods, SexpS(x = input, h =fh))                          # 3.  Simple exponential smoothing
  Methods <- cbind(Methods, MA(x = input, h = fh))                            # 4.  Moving average
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "classic"))     # 5.  Croston's method classid
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "optimized"))   # 6.  Croston's method optimized
  Methods <- cbind(Methods, Croston(x = input, h = fh, type = "sba"))         # 7.  Croston's method sba
  Methods <- cbind(Methods, TSB(x = input, h = fh))                           # 8.  TSB method 
  Methods <- cbind(Methods, ADIDA(x = input, h = fh))                         # 9.  ADIDA
  Methods <- cbind(Methods, iMAPA(x = input, h = fh))                         # 10. iMAPA
  Methods <- cbind(Methods, smooth_es(x = input, h = fh))                     # 11. smooth_es
  Methods <- cbind(Methods, auto_arima(x = input, h = fh))                    # 12. auto_arima
  Methods <- cbind(Methods, MLP_local(input = input, fh = fh, ni = 14))       # 13. MLP_local
  Methods <- cbind(Methods, RF_local(input = input, fh = fh, ni = 14))        # 14. RF_local
  
  # Forecasting post-processing: Set negatives forecasting values to zero (if any)
  for (i in 1:nrow(Methods)){
    for (j in 1:ncol(Methods)){
      if (Methods[i,j]<0){ Methods[i,j]<-0  } 
    }
  }
  
  # Create Methods data frame
  Methods <- data.frame(Methods)
  colnames(Methods) <- fm_names
  
  # Save ids in Methods data frame
  Methods$item_id <- x$item_id
  Methods$dept_id <- x$dept_id
  Methods$cat_id <- x$cat_id
  Methods$store_id <- x$store_id
  Methods$state_id <- x$state_id
  Methods$fh <- c(1:fh)
  
  return(Methods)
}

# Test
# benchmarks_f(x = time_series_b[5]$x, fh = 28)
