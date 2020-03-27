############################
#                          #
#   Evaluate forecasts     #
#                          #
############################

## Note: Example dataset with forecastings (frc_total) to pass as argument in evaluate_experiment(frc_total, b_names) function:

## Forecastings
# frc_total <- readRDS("data/forecastings/frc_top_12_item_ids.RDS")
# colnames(frc_total)
# View(frc_total)
# str(frc_total)
# dim(frc_total)

# Columns : Methhod_1, Method_2, ..., item_id, dept_id, cat_id, store_id, state_id, fh
# Rows    : For every item_id + dept_id + cat_id + store_id + state_id + fh the forecasting value

## b_names : vector with the names of forecasting methods (columns in frc_total dataframe)
# b_names <- setdiff(colnames(frc_total), c("item_id", "dept_id", "cat_id", "store_id", "state_id", "fh"))
# b_names


## Function: Evaluate experiment ##

evaluate_experiment <- function(frc_total, b_names, evaluation_file_name_prefix){
  
  
  ## 0. Import required datasets to calculate the WRMSSE
  
  print("Import required datasets...")
  
  # sales_train_validation dataset (to calculate demand for aggregation levels) 
  # - Comment out the following line because I will read aggregated time series from disk, for faster computations
  # sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F)
  
  # sales_test_validation dataset (for ground truth values)
  sales_out <- read.csv("data/raw/sales_test_validation.csv", stringsAsFactors = F)
  
  # time series b dataset (past sales values for scaling)
  time_series_b <- readRDS("data/processed/time_series_b.rds")
  
  # stat_total (volume sales for weigthing)
  stat_total <- readRDS("data/processed/stat_total.rds")
  
  
  ## Level 12: Unit sales of product x, aggregated for each store - 30,490
  
  print("WRMSSE_12 calculations...")
  
  errors_total <- NULL
  
  # Calculate the error for every time series
  
  for (tsid in 1:length(time_series_b)){
    
    # tsid = 1
    
    # Print tsid repeatetively at the same line
    cat("\r", paste("tsid:", tsid))
    
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
  print(paste("WRMSSE_12", WRMSSE_12))
  
  
  
  ## Level 1: Unit sales of all products, aggregated for all stores/states	- 1
  
  print("WRMSSE_1 calculations...")
  
  # Get the insample time series for aggregation level 1
  # insample <- as.numeric(colSums(sales[,7:ncol(sales)]))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_1.rds")
  
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
  
  print(paste("WRMSSE_1:", WRMSSE_1))
  
  
  
  ## Level 2: Unit sales of all products, aggregated for each State - 3
  
  print("WRMSSE_2 calculations...")
  
  # Get the insample time series for aggregation level 2
  # insample <- sales
  # insample$id = insample$item_id = insample$dept_id = insample$cat_id = insample$store_id <- NULL
  # insample <- ddply(.data = insample,          
  #                   .variables = .(state_id),  # split per state_id 
  #                   .fun = colwise(sum))       # column some the items for the same state
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_2.rds")
  
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
  
  print(paste("WRMSSE_2:", WRMSSE_2))
  
  
  ## Level 3: Unit sales of all products, aggregated for each store - 10
  
  print("WRMSSE_3 calculations...")
  
  # Get the insample time series for aggregation level 3
  # insample <- sales
  # insample$id = insample$item_id = insample$dept_id = insample$cat_id = insample$state_id <- NULL
  # insample <- ddply(.data = insample,
  #                   .variables = .(store_id),
  #                   .fun = colwise(sum))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_3.rds")
  
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
  
  print(paste("WRMSSE_3:", WRMSSE_3))
  
  
  ## Level 4:  Unit sales of all products, aggregated for each category - 3
  
  print("WRMSSE_4 calculations...")
  
  # Get the insample time series for aggregation level 4
  # insample <- sales
  # insample$id = insample$item_id = insample$dept_id = insample$store_id = insample$state_id <- NULL
  # insample <- ddply(.data = insample,
  #                   .variables = .(cat_id),
  #                   .fun = colwise(sum))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_4.rds")
  
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
  
  print(paste("WRMSSE_4:", WRMSSE_4))
  
  
  
  ## Level 5:  Unit sales of all products, aggregated for each department - 7
  
  print("WRMSSE_5 calculations...")
  
  # Get the insample time series for aggregation level 5
  # insample <- sales
  # insample$id = insample$item_id = insample$cat_id = insample$store_id = insample$state_id <- NULL
  # insample <- ddply(.data = insample,
  #                   .variables = .(dept_id),
  #                   .fun = colwise(sum))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_5.rds")
  
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
  
  print(paste("WRMSSE_5:", WRMSSE_5))
  
  
  
  ## Level 6: Unit sales of all products, aggregated for each State and category - 9
  
  print("WRMSSE_6 calculations...")
  
  # Get the insample time series for aggregation level 6
  # insample <- sales
  # insample$id = insample$item_id = insample$store_id = insample$dept_id <- NULL
  # insample <- ddply(.data = insample,
  #                   .variables = .(cat_id, state_id),
  #                   .fun = colwise(sum))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_6.rds")
  
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
  
  print(paste("WRMSSE_6:", WRMSSE_6))
  
  
  
  ## Level 7:  Unit sales of all products, aggregated for each State and department - 21
  
  print("WRMSSE_7 calculations...")
  
  # Get the insample time series for aggregation level 7
  # insample <- sales
  # insample$id = insample$item_id = insample$store_id = insample$cat_id <- NULL
  # insample <- ddply(.data = insample,
  #                   .variables =  .(dept_id, state_id),
  #                   .fun =  colwise(sum))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_7.rds")
  
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
  
  print(paste("WRMSSE_7:", WRMSSE_7))
  
  
  ## Level 8: Unit sales of all products, aggregated for each store and category - 30
  
  print("WRMSSE_8 calculations...")
  
  # Get the insample time series for aggregation level 8
  # insample <- sales
  # insample$id = insample$item_id = insample$state_id = insample$dept_id <- NULL
  # insample <- ddply(.data = insample,
  #                   .variables = .(cat_id, store_id),
  #                   .fun =  colwise(sum))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_8.rds")
  
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
  
  print(paste("WRMSSE_8:", WRMSSE_8))
  
  
  ## Level 9:  Unit sales of all products, aggregated for each store and department - 70
  
  print("WRMSSE_9 calculations...")
  
  # Get the insample time series for aggregation level 9
  # insample <- sales
  # insample$id = insample$item_id = insample$state_id = insample$cat_id <- NULL
  # insample <- ddply(.data = insample,
  #                   .variables = .(dept_id, store_id),
  #                   .fun =  colwise(sum))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_9.rds")
  
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
  
  print(paste("WRMSSE_9:", WRMSSE_9))
  
  
  ## Level 10: Unit sales of product x, aggregated for all stores/states - 3,049
  
  print("WRMSSE_10 calculations...")
  
  # Get the insample time series for aggregation level 10
  # insample <- sales
  # insample$id = insample$dept_id = insample$state_id = insample$cat_id = insample$store_id <- NULL
  # insample <- ddply(.data = insample,
  #                   .variables = .(item_id),
  #                   .fun =  colwise(sum))
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_10.rds")
  
  outsample <- sales_out
  outsample$dept_id = outsample$state_id = outsample$cat_id = outsample$store_id <- NULL
  outsample <- ddply(outsample, .(item_id), colwise(sum))
  
  Methods <- ddply(frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","item_id")], .(item_id,fh), colwise(sum))
  Methods$fh <- NULL
  
  WRMSSE_10 <- c()
  for (j in 1:length(b_names)){
    temp_error <- 0
    for (i in 1:nrow(insample)){
      
      # Print tsid repeatetively at the same line
      cat("\r", paste("tsid:", i))
      
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
  
  print(paste("WRMSSE_10:", WRMSSE_10))
  
  
  ## Level 11: Unit sales of product x, aggregated for each State - 9,147
  
  print("WRMSSE_11 calculations...")
  
  # Get the insample time series for aggregation level 11
  # insample <- sales
  # insample$id = insample$dept_id = insample$cat_id = insample$store_id <- NULL
  # insample <- ddply(.data = insample, 
  #                   .variables = .(item_id, state_id),
  #                   .fun = colwise(sum), .parallel = T)
  
  insample <- readRDS(file = "data/processed/aggregated_time_series/sales_aggregation_level_11.rds")
  
  outsample <- sales_out
  outsample$dept_id = outsample$cat_id = outsample$store_id <- NULL
  outsample <- ddply(outsample, .(item_id, state_id), colwise(sum))
  
  Methods <- ddply(frc_total[,c(colnames(frc_total)[1:length(b_names)],"fh","item_id","state_id")], .(item_id,state_id,fh), colwise(sum))
  Methods$fh <- NULL
  
  WRMSSE_11 <- c()
  for (j in 1:length(b_names)){
    temp_error  <- 0
    for (i in 1:nrow(insample)){
      
      # Print tsid repeatetively at the same line
      cat("\r", paste("tsid:", i))
      
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
  
  print(paste("WRMSSE_11:", WRMSSE_11))
  
  
  ## Combine WRMSSE (errors) for every aggregation level
  print("Combining WRMSSE scores...")
  
  WRMSSE <- rbind(WRMSSE_1, WRMSSE_2, WRMSSE_3,
                  WRMSSE_4, WRMSSE_5, WRMSSE_6,
                  WRMSSE_7, WRMSSE_8, WRMSSE_9,
                  WRMSSE_10, WRMSSE_11, WRMSSE_12)
  
  WRMSSE <- rbind(WRMSSE, colMeans(WRMSSE))
  
  row.names(WRMSSE) <- c("Total", "State", "Store",
                         "Category", "Department", "State-Category",
                         "State-Department", "Store-Category", "Store-Department",
                         "Product", "Product-State", "Product-Store", "Average")
  
  print("Weigthed Root Mean Squared error (WRMSSE) is:")
  print(WRMSSE)
  
  ## Save errors
  path <- paste0("data/submissions/", evaluation_file_name_prefix, "_WRMSSE.csv")
  write.csv(x = WRMSSE, file = path, quote = F)
  
  print("Experiment Done!")
  print(paste("Score file saved at", path, "directory."))
  
  # return(WRMSSE)
}


# Test
# scores <- evaluate_experiment(frc_total = frc_total, b_names = b_names, evaluation_file_name_prefix = "exp_naive")