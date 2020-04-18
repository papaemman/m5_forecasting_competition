#############################
#                           #
#   Helper functions        #
#                           #
#############################


## 01. Function: intervals(x) ----

# Count intervals with consequitevely 0s
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


## 02. Function: demand(x) ----

# Keep only demand values (ie only positive values)

demand <- function(x){
  y <- x[x!=0]
  y
}

# Test demand()
# x <- c(1,2,3,4,5,0,0,6,0)
# demand(x)
# [1] 1 2 3 4 5 6


## 03. Function: recompose(x,y1,y2,k) ----

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



## 04. Function: CreateSamples(datasample, xi, xo=1) ----

# This functions used in for Machine Learning models (mlp, rf).
# Takes sliding windows of length xi + x0, to create all possible n-samples of the raw signal

CreateSamples <- function(datasample, xi, xo=1){
  
  ## Test
  # datasample = time_series_b[[1]]$x
  # xi = 7
  # xo = 1
  
  sample <- matrix(NA, nrow = length(datasample), ncol=(xi+xo)) 
  
  for (cid in (xi+xo):length(datasample)){
    sample[cid,] <- datasample[(cid-xi-xo+1):cid]
  }
  
  sample <- as.matrix(data.frame(na.omit(sample)))
  return(sample)
}


## Q: How CreateSamples function works?

## raw signal: t1, t2, t3, t4, t5, t6, t7, t8, t9, t10

## Machine Learning dataframe (window size xi = 4, x0 = 1)

# X1 X2 X3 X4  Y
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

## 05. Function: statistics(tsid) ----

# Function to caclulate time series statistics 
# time_series_b object needed

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
  ex_calendar <- merge(calendar, ex_price, by=c("wm_yr_wk"), all.x = T)   # Note that prices datasets doesn't contain prices for all days for every product
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