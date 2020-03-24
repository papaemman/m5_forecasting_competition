###################################
#                                 #
#  Create time_series_b object    #
#                                 #
###################################


# Import sales datasets
sales <- read.csv(file = "data/raw/sales_test_validation.csv")


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

# Save time_series_b object
saveRDS(object = time_series_b, file = "data/processed/time_series_b.rds")

