##################################################
#                                                #
# Helper function to Create data and Features    #
#                                                #
##################################################


## Function create_dt(): Import and merge datasets

create_dt <- function(is_train = TRUE, nrows = Inf, fh, max_lags, tr_last, fday) {
  
  ## Test
  
  # is_train = T                       # What part of sales data to import (first days or last+forecasting days)
  # nrows = Inf                        # How many sales data to import 
  # fh = 28                            # Forecasting horizon
  # max_lags = 366                     # Max lags (features)
  # tr_last = 1913                     # Training last day
  # fday = as.IDate("2016-04-25")      # Forecasting day (first day)
 
  
  
  ## 1. Sales dataset
  
  if (is_train) {
    
    # Import train complete sales train validation dataset
    dt <- fread("data/raw/sales_train_validation.csv", nrows = nrows)
    
  } else {
    
    # Import the last max_lags columns from sales train validation dataset 
    dt <- fread("data/raw/sales_train_validation.csv", nrows = nrows,
                   drop = paste0("d_", 1:(tr_last-max_lags)))
    
    # Merge empty column for every forecasting day
    dt[, paste0("d_", (tr_last+1):(tr_last+2*fh)) := NA_real_]
  }
  
  
  dt <- melt(dt,
             measure.vars = patterns("^d_"),
             variable.name = "d",
             value.name = "sales")
  
  dt[, d := as.integer(substring(d, 3))]
  
  
  ## 2. Calendar dataset
  
  # calendar <- fread("data/raw/calendar.csv")
  calendar <- readRDS("data/raw/calendar_full.rds")
  
  # merge
  dt <- dt[calendar, on = "d", nomatch = 0]
  rm(calendar)
  
  
  ## 3. Prices
  
  # prices <- fread("data/raw/sell_prices.csv")
  prices <- readRDS("data/raw/prices_full.rds")
  
  # merge
  # Use nomatch = 0 to drop initial zero-demand periods for every product
  dt <- dt[prices, on = c("store_id", "item_id", "wm_yr_wk"), nomatch = 0]
  rm(prices)
  gc()
  
  return(dt)
}


# Function create_fea(): create features

create_fea <- function(dt) {
  
  # Drop d, wm_yr_wk
  dt[, `:=`(wm_yr_wk = NULL)]
  
  ## Create Sales features
  dt <- create_sales_features(dt)
  
  # Turn non-numerics to integers
  cols <- c("item_id", "dept_id", "cat_id", "store_id", "state_id", "Type")
  dt[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]
  
  return(dt)
}







