######################################
#                                    #
#  Experimentation helper functions  #
#                                    #
######################################



## 01. Function: benchmarks_f(), to use all benchmark methods at once ----

# Benchmark function: Takes an element x=time_series_b[[i]] and fh=28 (forecasting horizon)
#                     and produce forecasting for every forecasting method

benchmarks_f <- function(x, fh){
  
  # Test
  # x <- time_series_b[[1]]
  # fh = 23
  
  # Print item id
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


# benchmarks_f(x = time_series_b[5]$x, fh = 28)
