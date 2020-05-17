##################################################
#                                                #
# Helper function to Create data and Features    #
#                                                #
##################################################

# // Functions //

# 1. create_dt()
# 2. create_hierarhy_sales_features()
# 3. create_categorical_encoding_features() 
#    add_categorical_encoding_features_test() 
# 4. add_more_features()
#    drop_zero_demand_periods()
# 5. helper function (create feature encoding data tables for test)

## 01. Function create_dt(): Import and merge datasets -----

create_dt <- function(is_train = TRUE, nrows = Inf, fh, max_lags, tr_last, fday, filter_params = NA) {
  
  ## Test
  
  # is_train = T                       # What part of sales data to import (first days or last+forecasting days)
  # nrows = Inf                        # How many sales data to import 
  # fh = 28                            # Forecasting horizon
  # max_lags = 366                     # Max lags (features)
  # tr_last = 1913                     # Training last day
  # fday = as.IDate("2016-04-25")      # Forecasting day (first day)
  # filter_params = "dept_id == 'HOBBIES_1' "
  
  
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
  
  # Filter sales
  if(!is.na(filter_params)){
    dt <- dt[eval(parse(text = filter_params)), ]
  }
  
  
  ## 2. Merger Calendar features
  
  # calendar <- fread("data/raw/calendar.csv")
  calendar <- readRDS("data/raw/calendar_full.rds")
  
  # merge
  dt <- dt[calendar, on = "d", nomatch = 0]
  rm(calendar)
  
  
  ## 3. Merge Prices features
  
  # prices <- fread("data/raw/sell_prices.csv")
  prices <- readRDS("data/raw/prices_full.rds")
  
  # merge
  # Use nomatch = 0 to drop initial zero-demand periods for every product (whenever there isn't prices for these products)
  dt <- dt[prices, on = c("store_id", "item_id", "wm_yr_wk"), nomatch = 0]
  rm(prices)
  gc()
  
  return(dt)
}



## 02. Function create_hierarhy_sales_features() -----

# SOS: These features must be used with lags too!

create_hierarhy_sales_features <- function(dt){
  
  
  # Hierarhy levels Features
  print("create hierarhy sales features...")
  
  dt <- dt[ dt[, .(total_sales = sum(sales)), by = c("d")], on = "d", nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_state_sales = sum(sales)), by = c("state_id", "d")], on = c("state_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_store_sales = sum(sales)), by = c("store_id", "d")], on = c("store_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_cat_sales = sum(sales)), by = c("cat_id", "d")], on = c("cat_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_dept_sales = sum(sales)), by = c("dept_id", "d")], on = c("dept_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_state_cat_sales = sum(sales)), by = c("state_id", "cat_id", "d")], on = c("state_id", "cat_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_state_dept_sales = sum(sales)), by = c("state_id","dept_id", "d")], on = c("state_id","dept_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_store_cat_sales = sum(sales)), by = c("store_id","cat_id", "d")], on = c("store_id","cat_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_store_dept_sales = sum(sales)), by = c("store_id","dept_id", "d")], on =  c("store_id","dept_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_item_sales = sum(sales)), by = c("item_id", "d")], on = c("item_id", "d"), nomatch = 0]
  gc()
  
  dt <- dt[ dt[, .(total_item_state_sales = sum(sales)), by = c("state_id","item_id", "d")], on = c("state_id","item_id", "d"), nomatch = 0]
  gc()
  
  # sales
  # dt <- dt[ dt[, .(total_item_store_sales = sum(sales)), by = c("store_id","item_id", "d")], on = c("store_id","item_id", "d"), nomatch = 0]
  
  return(dt)
}                                

# Test
# dt <- create_hierarhy_sales_features(dt = dt)                       
# View(head(dt, 300))


## 03. Function create_categorical_encoding_features() ----

create_categorical_encoding_features <- function(dt){
  
  # TASK:
  # Calculate the mean, sd, etc for the target variable for evey level of categorical features.
  
  # Why?
  # I have 3 possible ways to add categorical features as predictors in any ML model:
  #   1. label encoding   (1,2,3,...)
  #   2. one hot encoding 
  #   3. categorical endoging with target
  
  # The label encoding I think it's the least optimal way.
  # For example for state_id:
  #   1. label encoding   (state_id = {CA = 1, TX = 2, WI = 3})
  #   2. one hot encoding (CA_feature ={0,1}, TX_feature = {0,1}, WI_feature = {0,1})
  #   3. cat_encoding     (enc_state_id =  {CA = 1.57, TX = 1.341, WI = 1.320})
  
  # So ths distance between states it's more appropriate in the third solution.
  # dt[,mean(sales),by = c("state_id")]
  
  
  ## // Categorical features //
  
  # Categorical features : item_id, dept_id,  cat_id, store_id, state_id
  # Target               : sales
  
  # Helper
  # dt[,list(mean_sales = mean(sales), max_sales = max(sales), min_sales = min(sales), sd_sales = sd(sales) ),by = c("state_id")]
  
  # 1. state_id
  dt <- dt[ dt[, .(enc_state_mean = round(mean(sales),3),
                   enc_state_sd = round(sd(sales), 3)), by = c("state_id")], on = c("state_id"), nomatch = 0]
  gc()
  
  # 2. store_id
  dt <- dt[ dt[, .(enc_store_mean = round(mean(sales),3),
                   enc_store_sd = round(sd(sales), 3)), by = c("store_id")], on = c("store_id"), nomatch = 0]
  gc()
  
  # 3. cat_id
  dt <- dt[ dt[, .(enc_cat_mean = round(mean(sales),3),
                   enc_cat_sd = round(sd(sales), 3)), by = c("cat_id")], on = c("cat_id"), nomatch = 0]
  gc()
  
  # 4. dept_id
  dt <- dt[ dt[, .(enc_dept_mean = round(mean(sales),3),
                   enc_dept_sd = round(sd(sales),3)), by = c("dept_id")], on = c("dept_id"), nomatch = 0]
  gc()
  
  # 5. state_id - cat_id
  dt <- dt[ dt[, .(enc_state_cat_mean = round(mean(sales),3),
                   enc_state_cat_sd = round(sd(sales), 3)), by = c("state_id", "cat_id")], on = c("state_id", "cat_id"), nomatch = 0]
  gc()
  
  # 6. state_id - dept_id
  dt <- dt[ dt[, .(enc_state_dept_mean = round(mean(sales),3),
                   enc_state_dept_sd = round(sd(sales), 3)), by = c("state_id", "dept_id")], on = c("state_id", "dept_id"), nomatch = 0]
  gc()
  
  # 7. store_id - cat_id
  dt <- dt[ dt[, .(enc_store_cat_mean = round(mean(sales),3),
                   enc_store_cat_sd = round(sd(sales), 3)), by = c("store_id", "cat_id")], on = c("store_id", "cat_id"), nomatch = 0]
  gc()
  
  # 8. store_id - dept_id
  dt <- dt[ dt[, .(enc_store_dept_mean = round(mean(sales),3),
                   enc_store_dept_sd = round(sd(sales), 3)), by = c("store_id", "dept_id")], on = c("store_id", "dept_id"), nomatch = 0]
  gc()
  
  # 9. item_id
  dt <- dt[ dt[, .(enc_item_mean = round(mean(sales),3),
                   enc_item_sd = round(sd(sales), 3),
                   enc_item_max = max(sales)), by = c("item_id")], on = c("item_id"), nomatch = 0]
  gc()
  
  # 10. item_id - state_id
  dt <- dt[ dt[, .(enc_item_state_mean = round(mean(sales),3),
                   enc_item_state_sd = round(sd(sales), 3),
                   enc_item_state_max = max(sales)), by = c("item_id", "state_id")], on = c("item_id", "state_id"), nomatch = 0]
  
  gc()
  
  # 11. item_id - store_id (Next function!)
  # dt <- dt[ dt[, .(enc_item_store_mean = round(mean(sales),3),
  #                  enc_item_store_sd = round(sd(sales), 3),
  #                  enc_item_store_max = max(sales)), by = c("item_id", "store_id")], on = c("item_id", "store_id"), nomatch = 0]
  # gc()
  
  
  return(dt)
}


add_categorical_encoding_features_test <- function(dt){
  
  
  # 1. state_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_state.rds")
  dt <- dt[dt_temp, on = c("state_id"), nomatch = 0]
  gc()
  
  # 2. store_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_store.rds")
  dt <- dt[dt_temp, on = c("store_id"), nomatch = 0]
  gc()
  
  # 3. cat_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_cat.rds")
  dt <- dt[dt_temp, on = c("cat_id"), nomatch = 0]
  gc()
  
  # 4. dept_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_dept.rds")
  dt <- dt[dt_temp, on = c("dept_id"), nomatch = 0]
  gc()
  
  # 5. state_id - cat_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_state_cat.rds")
  dt <- dt[dt_temp, on = c("state_id", "cat_id"), nomatch = 0]
  gc()
  
  # 6. state_id - dept_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_state_dept.rds")
  dt <- dt[dt_temp, on = c("state_id", "dept_id"), nomatch = 0]
  gc()
  
  # 7. store_id - cat_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_store_cat.rds")
  dt <- dt[dt_temp, on = c("store_id", "cat_id"), nomatch = 0]
  gc()
  
  # 8. store_id - dept_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_store_dept.rds")
  dt <- dt[dt_temp, on = c("store_id", "dept_id"), nomatch = 0]
  gc()
  
  # 9. item_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_item.rds")
  dt <- dt[dt_temp, on = c("item_id"), nomatch = 0]
  gc()
  
  # 10. item_id - state_id
  dt_temp <- readRDS("data/categorical_enc/dt_enc_item_state.rds")
  dt <- dt[dt_temp, on = c("item_id", "state_id"), nomatch = 0]
  gc()
  
  rm(dt_temp)
  return(dt)
}


## 04. Function add_more_features()  ----

# Add more features for item_id - store_id time series

add_more_features <- function(dt) {
  
  ## Features from stat_total file
  print("Preprocessing steps...")
  
  # 1. Drop d, wm_yr_wk
  dt[, `:=`(wm_yr_wk = NULL)]
  
  # 2. Extra column snap
  dt[, snap := ifelse(state_id == "CA", snap_CA, ifelse(state_id == "TX", snap_TX, snap_WI))]
  
  
  # 3. Features from stat_total file
  print("Statistical Total features...")
  
  stat_total <- readRDS("data/processed/stat_total.rds")
  stat_total <- as.data.table(stat_total)
  
  dt <- dt[stat_total[,c("item_id", "store_id", "lngth", "ADI", "CV2","pz", "Type", "Low25", "Mean", "Median", "Up25", "Max", "dollar_sales")],
           on = c("item_id", "store_id"), nomatch = 0]
  
  rm(stat_total)
  gc()
  
  dt[, `:=`(type_1 = as.numeric(Type == "Lumpy"),
            type_2 = as.numeric(Type == "Intermittent"),
            type_3 = as.numeric(Type == "Smooth"),
            type_4 = as.numeric(Type == "Erratic"))][, Type := NULL]
  
  gc()
  
  # 3. Label encoding for categorical features (Turn non-numerics to integers)
  
  # cols <- c("item_id", "dept_id", "cat_id", "store_id", "state_id")
  # dt[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]
  
  # SOS (alphabetical order):
  # levels(as.factor(dt$dept_id))    # [1] "FOODS_1"     "FOODS_2"     "FOODS_3"     "HOBBIES_1"   "HOBBIES_2"   "HOUSEHOLD_1" "HOUSEHOLD_2"
  # levels(as.factor(dt$cat_id))     # [1] "FOODS"     "HOBBIES"   "HOUSEHOLD"
  # levels(as.factor(dt$store_id))   # [1] "CA_1" "CA_2" "CA_3" "CA_4" "TX_1" "TX_2" "TX_3" "WI_1" "WI_2" "WI_3"
  # levels(as.factor(dt$state_id))   # [1] "CA" "TX" "WI"
  
  
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


## 05. Helpers ----
##  Save categorical_encoding values data.tables, calculated from all historic sales, to add them on test set
function(){
   
  
  # 1. state_id
  dt_enc_state <- dt[, .(enc_state_mean = round(mean(sales),3),
                         enc_state_sd = round(sd(sales), 3)), by = c("state_id")]
  saveRDS(dt_enc_state, "data/categorical_enc/dt_enc_state.rds")
  
  # 2. store_id
  dt_enc_store <- dt[, .(enc_store_mean = round(mean(sales),3),
                         enc_store_sd = round(sd(sales), 3)), by = c("store_id")]
  
  saveRDS(dt_enc_store, "data/categorical_enc/dt_enc_store.rds")
  
  # 3. cat_id
  dt_enc_cat <- dt[, .(enc_cat_mean = round(mean(sales),3),
                       enc_cat_sd = round(sd(sales), 3)), by = c("cat_id")]
  
  saveRDS(dt_enc_cat, "data/categorical_enc/dt_enc_cat.rds")
  
  # 4. dept_id
  dt_enc_dept <- dt[, .(enc_dept_mean = round(mean(sales),3),
                        enc_dept_sd = round(sd(sales),3)), by = c("dept_id")]
  
  saveRDS(dt_enc_dept, "data/categorical_enc/dt_enc_dept.rds")
  
  # 5. state_id - cat_id
  dt_enc_state_cat <- dt[, .(enc_state_cat_mean = round(mean(sales),3),
                             enc_state_cat_sd = round(sd(sales), 3)), by = c("state_id", "cat_id")]
  
  saveRDS(dt_enc_state_cat, "data/categorical_enc/dt_enc_state_cat.rds")
  
  # 6. state_id - dept_id
  dt_enc_state_dept <- dt[, .(enc_state_dept_mean = round(mean(sales),3),
                              enc_state_dept_sd = round(sd(sales), 3)), by = c("state_id", "dept_id")]
  
  saveRDS(dt_enc_state_dept, "data/categorical_enc/dt_enc_state_dept.rds")
  
  # 7. store_id - cat_id
  dt_enc_store_cat <- dt[, .(enc_store_cat_mean = round(mean(sales),3),
                             enc_store_cat_sd = round(sd(sales), 3)), by = c("store_id", "cat_id")]
  
  saveRDS(dt_enc_store_cat, "data/categorical_enc/dt_enc_store_cat.rds")
  
  
  # 8. store_id - dept_id
  dt_enc_store_dept <- dt[, .(enc_store_dept_mean = round(mean(sales),3),
                              enc_store_dept_sd = round(sd(sales), 3)), by = c("store_id", "dept_id")]
  
  saveRDS(dt_enc_store_dept, "data/categorical_enc/dt_enc_store_dept.rds")
  
  
  # 9. item_id
  dt_enc_item <- dt[, .(enc_item_mean = round(mean(sales),3),
                        enc_item_sd = round(sd(sales), 3),
                        enc_item_max = max(sales)), by = c("item_id")]
  
  saveRDS(dt_enc_item, "data/categorical_enc/dt_enc_item.rds")
  
  # 10. item_id - state_id
  
  dt_enc_item_state <- dt[, .(enc_item_state_mean = round(mean(sales),3),
                              enc_item_state_sd = round(sd(sales), 3),
                              enc_item_state_max = max(sales)), by = c("item_id", "state_id")]
  
  
  saveRDS(dt_enc_item_state, "data/categorical_enc/dt_enc_item_state.rds")
}


