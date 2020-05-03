##################################
#                                #
# Feature Engineering: Sales     #
#                                #
##################################

## Source dependencies
# source("modules/main.R")

# library(data.table)
# library(RcppRoll)


## Import data 
# sales <- fread("data/raw/sales_train_validation.csv")


create_sales_features <- function(dt){
  
  # dt
  
  ## Sales Features construction
  
  # 1. Sales lags
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
            lag_t28 = dplyr::lag(sales, 28)),
     by = "id"]
  
  gc()
  
  
  # 2. Rolling features
  print("Sales rolling features...")
  
  dt[ , `:=`(
    
    ## Last month mean
    mean_last_month = (lag_t7 + lag_t14 + lag_t21 + lag_t28)/4,
    
    
    ## Rolling mean, sd lag7
    
    rolling_mean_lag7_t7 = roll_meanr(lag_t7, 7),
    rolling_mean_lag7_t30 = roll_meanr(lag_t7, 30),
    rolling_mean_lag7_t60 = roll_meanr(lag_t7, 60),
    rolling_mean_lag7_t90 = roll_meanr(lag_t7, 90),
    rolling_mean_lag7_t120 = roll_meanr(lag_t7, 120),
    rolling_mean_lag7_t180 = roll_meanr(lag_t7, 180),
    
    rolling_sd_lag7_t7 = roll_sdr(lag_t7, 7),
    rolling_sd_lag7_t30 = roll_sdr(lag_t7, 30),
    
    
    ## Rolling mean, sd lag28
    
    rolling_mean_lag28_t7 = roll_meanr(lag_t28, 7),
    rolling_mean_lag28_t30 = roll_meanr(lag_t28, 30),
    rolling_mean_lag28_t120 = roll_meanr(lag_t28, 120),
    
    rolling_sd_lag28_t7 = roll_sdr(lag_t28, 7),
    rolling_sd_lag_28_t30 = roll_sdr(lag_t28, 30)
    ),
    
    by = "id"]
  
  gc()
  
  return(dt)
  
}
                   
                                  
                                  