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
    
    # Import the last max_lags columns from sales train validation dataset, to construct the test dataset
    dt <- fread("data/raw/sales_train_validation.csv",
                nrows = nrows,
                drop = paste0("d_", 1:(tr_last-max_lags)))
    
    # Merge empty column for every forecasting day 
    # (Use NA_real_ because the predictions will be real numbers)
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
  # Use nomatch = 0 to drop initial zero-demand periods for every product (wherever there isn't prics for these products)
  dt <- dt[prices, on = c("store_id", "item_id", "wm_yr_wk"), nomatch = 0]
  rm(prices)
  gc()
  
  return(dt)
}



# Function create_fea(): create features

add_features <- function(dt) {
  
  ## Features from stat_total file
  print("Preprocessing steps...")
  
  # Drop d, wm_yr_wk
  dt[, `:=`(wm_yr_wk = NULL)]
  
  # Extra column snap
  dt[, snap := ifelse(state_id == "CA", snap_CA, ifelse(state_id == "TX", snap_TX, snap_WI))]
  
  # Turn non-numerics to integers
  cols <- c("item_id", "dept_id", "cat_id", "store_id", "state_id")
  dt[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]
  
  # SOS (alphabetical order):
  # levels(as.factor(dt$dept_id))    # [1] "FOODS_1"     "FOODS_2"     "FOODS_3"     "HOBBIES_1"   "HOBBIES_2"   "HOUSEHOLD_1" "HOUSEHOLD_2"
  # levels(as.factor(dt$cat_id))     # [1] "FOODS"     "HOBBIES"   "HOUSEHOLD"
  # levels(as.factor(dt$store_id))   # [1] "CA_1" "CA_2" "CA_3" "CA_4" "TX_1" "TX_2" "TX_3" "WI_1" "WI_2" "WI_3"
  # levels(as.factor(dt$state_id))   # [1] "CA" "TX" "WI"
  
  
  ## Features from stat_total file
  print("Statistical Total features...")
  
  stat_total <- readRDS("data/processed/stat_total.rds")
  stat_total <- as.data.table(stat_total)
  stat_total_short <- stat_total[,c("item_id", "store_id", "ADI", "CV2","pz", "Type", "Low25", "Mean", "Median", "Up25", "Max")]
  cols <- c("item_id", "store_id","Type")
  stat_total_short[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]
  
  dt <- dt[stat_total_short, on = c("item_id", "store_id"), nomatch = 0]
  rm(stat_total)
  gc()
  
  
  return(dt)
}



# Reduce size: Drop long zero-sales periods for every product

drop_zero_demand_periods <- function(dt, nb_days = 60){
  
  # nb_days: number of consecutively days with 0 sales
  print(paste("Dropping periods with", nb_days, "consecutively days with 0 sales, from the dataset..."))
  
  dt[, orig := .I]
  dt[, rleLength := {rr <- rle(sales); rep(rr$length, rr$length)}, by = c("item_id", "store_id")]
  dt <- dt[!(sales==0 & rleLength > nb_days), ][order(orig)][,orig := NULL]
  gc()
  
  return(dt)
}






