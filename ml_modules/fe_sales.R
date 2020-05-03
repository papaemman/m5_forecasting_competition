##################################
#                                #
# Feature Engineering: Sales     #
#                                #
##################################

## Source dependencies
# source("ml_modules/create_data.R")

# library(data.table)
# library(RcppRoll)

## Import data (melted sales )
# dt <- create_dt(is_train = T, nrows = 10000, fh = 28, max_lags = 366, tr_last = 1913, fday = as.IDate("2016-04-25"))


create_sales_features <- function(dt){
  
  # dt
  
  ## Sales Features construction
  
  
  ## 01. Sales lags ----
  print("Sales lag features...")
  
  dt[, `:=`(lag_t1  = dplyr::lag(sales, 1),
            lag_t2  = dplyr::lag(sales, 2),
            lag_t3  = dplyr::lag(sales, 3),
            lag_t4  = dplyr::lag(sales, 4),
            lag_t5  = dplyr::lag(sales, 5),
            lag_t6  = dplyr::lag(sales, 6),
            lag_t7  = dplyr::lag(sales, 7),
            lag_t8  = dplyr::lag(sales, 8),
            lag_t14 = dplyr::lag(sales, 14),
            lag_t21 = dplyr::lag(sales, 21),
            lag_t28 = dplyr::lag(sales, 28),
            lag_t35 = dplyr::lag(sales, 35),
            lag_t42 = dplyr::lag(sales, 42),
            lag_t49 = dplyr::lag(sales, 49)),
     by = "id"]
  
  gc()
  
  
  ## 02. Rolling features ----
  
  print("Sales rolling features...")
  
  dt[ , `:=`(
    
    # Last months means
    mean_last_month = (lag_t7 + lag_t14 + lag_t21 + lag_t28)/4,

    mean_last_2_monts = (lag_t7 + lag_t14 + lag_t21 + lag_t28 + lag_t35 + lag_t42 + lag_t49)/7,
    
    mean_previous_month = (lag_t28 + lag_t35 + lag_t42 + lag_t49)/4,
    
    
    # Rolling mean, sd, sum, min, max of lag7 (These feature vectors will use predictions as ground truth values)
    
    rolling_mean_lag7_t7 = roll_meanr(lag_t7, 7),
    rolling_mean_lag7_t30 = roll_meanr(lag_t7, 30),
    rolling_mean_lag7_t60 = roll_meanr(lag_t7, 60),
    rolling_mean_lag7_t90 = roll_meanr(lag_t7, 90),
    rolling_mean_lag7_t120 = roll_meanr(lag_t7, 120),
    rolling_mean_lag7_t180 = roll_meanr(lag_t7, 180),
    
    rolling_sd_lag7_t7 = roll_sdr(lag_t7, 7),
    rolling_sd_lag7_t30 = roll_sdr(lag_t7, 30),
    
    rolling_sum_lag7_t7 = roll_sumr(lag_t7, 7),
    
    rolling_min_lag7_t7 = roll_minr(lag_t7, 7),
    rolling_min_lag7_t30 = roll_minr(lag_t7, 30),
    
    rolling_max_lag7_t7 = roll_maxr(lag_t7, 7),
    rolling_max_lag7_t30 = roll_maxr(lag_t7, 30),
    
    
    
    # Rolling mean, sd, sum, min, max of lag28 (These feature vectors will not use predictions as ground truth values)
    
    rolling_mean_lag28_t7 = roll_meanr(lag_t28, 7),
    rolling_mean_lag28_t30 = roll_meanr(lag_t28, 30),
    rolling_mean_lag28_t60 = roll_meanr(lag_t28, 60),
    rolling_mean_lag28_t90 = roll_meanr(lag_t28, 90),
    rolling_mean_lag28_t120 = roll_meanr(lag_t28, 120),
    rolling_mean_lag28_t180 = roll_meanr(lag_t28, 180),
    
    rolling_sd_lag28_t7 = roll_sdr(lag_t28, 7),
    rolling_sd_lag28_t30 = roll_sdr(lag_t28, 30),
    
    rolling_sum_lag28_t7 = roll_sumr(lag_t28, 7),
    
    rolling_min_lag28_t7 = roll_minr(lag_t28, 7),
    rolling_min_lag28_t30 = roll_minr(lag_t28, 30),
    
    rolling_max_lag28_t7 = roll_maxr(lag_t28, 7),
    rolling_max_lag28_t30 = roll_maxr(lag_t28, 30)),  by = "id"]
  
  gc()
  
  return(dt)
  
}
                   



create_hierarhy_sales_features <- function(dt){
  
  
  ## Hierarhy levels Features ----
  print("create hierarhy sales features...")
  
  # Sum, mean, sd
  
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
           
# dt <- create_hierarhy_sales_features(dt = dt)                       
# View(head(dt, 300))
