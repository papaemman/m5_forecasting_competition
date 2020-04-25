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
  
  ## Features from stat_total file
  
  stat_total <- readRDS("data/processed/stat_total.rds")
  stat_total <- as.data.table(stat_total)
  stat_total_short <- stat_total[,c("item_id", "store_id", "ADI", "CV2","pz", "Type", "Low25", "Mean", "Median", "Up25")]
  
  dt <- dt[stat_total_short, on = c("item_id", "store_id"), nomatch = 0]
  rm(stat_total)
  gc()
  
  
  ## Sales Features construction
  
  # 1. Sales lags
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
  
  
  # 2. Rolling features
  dt[ , `:=`(
    
    ## Last month mean
    mean_last_month = (lag_t7 + lag_t14 + lag_t21 + lag_t28)/4,
    
    
    ## Rolling mean,sd lag7
    
    rolling_mean_lag7_t7 = roll_meanr(lag_t7, 7),
    rolling_mean_lag7_t30 = roll_meanr(lag_t7, 30),
    # rolling_mean_lag7_t60 = roll_meanr(lag_t7, 60),
    # rolling_mean_lag7_t90 = roll_meanr(lag_t7, 90),
    # rolling_mean_lag7_t120 = roll_meanr(lag_t7, 120),
    # rolling_mean_lag7_t180 = roll_meanr(lag_t7, 180),
    
    rolling_sd_lag7_t7 = roll_sdr(lag_t7, 7),
    rolling_sd_lag7_t30 = roll_sdr(lag_t7, 30),
    
    ## Rolling mean,sd lag28
    
    rolling_mean_lag28_t7 = roll_meanr(lag_t28, 7),
    rolling_mean_lag28_t30 = roll_meanr(lag_t28, 30),
    # rolling_mean_lag28_t180 = roll_meanr(lag_t28, 180),
    
    rolling_sd_lag28_t7 = roll_sdr(lag_t28, 7),
    rolling_sd_lag_28_t30 = roll_sdr(lag_t28, 30)
    ),
    
    by = "id"]
  
  
  # Drop NAs from first days
  dt <- dt[!is.na(rolling_mean_lag28_t30)]
  
  
  ## Extra column snap
  dt[, snap := ifelse(state_id == "CA", snap_CA, ifelse(state_id == "TX", snap_TX, snap_WI))]
  
  
  ## Reduce size: Drop long zero-sales periods for every product
  
  nb_days <- 60  # number of consecutively days with 0 sales
  
  dt[, orig := .I]
  dt[, rleLength := {rr <- rle(sales); rep(rr$length, rr$length)}, by = c("item_id", "store_id")]
  dt <- dt[!(sales==0 & rleLength > nb_days), ][order(orig)][,orig := NULL]
  
  print(paste("Periods with", nb_days, "consecutively days with 0 sales, have been droped from the dataset"))
  
  gc()
  return(dt)
  
}
                   
                                  
                                  