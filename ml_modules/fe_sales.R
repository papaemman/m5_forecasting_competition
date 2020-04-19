##################################
#                                #
# Feature Engineering: Sales     #
#                                #
##################################

## Source dependencies
# source("modules/main.R")

# library(data.table)
# library(RcppRoll)


create_sales_data <- function(){
  
  ## Import data 
  sales <- fread("data/raw/sales_train_validation.csv")
  
  # fh <- 2*28
  fh <- 28
  
  ## Add empty observations
  sales[, id := gsub("_validation", "", id)]                         
  empty_dt = matrix(NA_integer_, ncol = fh, nrow = 1, dimnames = 
                      list(NULL, paste("d", 1913 + 1:fh, sep = "_")))
  
  sales <- cbind(sales, empty_dt)
  
  rm(empty_dt)
  
  
  ## Sales: Reshape
  sales <- data.table::melt(sales,
                id.vars = c("id", "item_id", "dept_id", "cat_id", "store_id", "state_id"), 
                variable.name = "d", value = "demand")
  
  sales[, d := as.integer(substring(d, 3))]

  
  # sales[ , .N, by=c("item_id", "store_id")]
  # dim(sales)  [1] 60034810        8
  # 1969*30490
  
  
  ## Sales: Reduce size
  
  # First way: Drop the first 1000 rows for every item_id, store_id
  # sales <- sales[d >= 1000]
  
  # Second way: Drop initial zero-demand periods for every product
  stat_total <- readRDS("data/processed/stat_total.rds")
  stat_total <- as.data.table(stat_total)
  stat_total[, first_day := 1914 - lngth]
  stat_total_short <- stat_total[,c("item_id", "store_id", "first_day")]
  
  sales <- sales[stat_total_short, on = c("item_id", "store_id"), nomatch = 0]

  sales <- sales[d>=first_day]
  sales[,first_day:=NULL]
  # > dim(sales)
  # [1] 47649940        8
  
  rm(stat_total);rm(stat_total_short);gc();

  
  ## Sales: Feature construction
  
  sales[, `:=`(lag_t1  = dplyr::lag(demand, 1),
               lag_t2  = dplyr::lag(demand, 2),
               lag_t3  = dplyr::lag(demand, 3),
               lag_t4  = dplyr::lag(demand, 4),
               lag_t5  = dplyr::lag(demand, 5),
               lag_t6  = dplyr::lag(demand, 6),
               lag_t7  = dplyr::lag(demand, 7),
               lag_t8  = dplyr::lag(demand, 8),
               lag_t14 = dplyr::lag(demand, 14),
               lag_t21 = dplyr::lag(demand, 21),
               lag_t28 = dplyr::lag(demand, 28)),
        by = "id"]
  
  
  
  sales[ , `:=`(
    
    mean_last_month = (lag_t7 + lag_t14 + lag_t21 + lag_t28)/4,
    
    rolling_mean_lag7_t7 = roll_meanr(lag_t7, 7),
    rolling_mean_lag7_t30 = roll_meanr(lag_t7, 30),
    # rolling_mean_lag7_t60 = roll_meanr(lag_t7, 60),
    # rolling_mean_lag7_t90 = roll_meanr(lag_t7, 90),
    # rolling_mean_lag7_t120 = roll_meanr(lag_t7, 120),
    # rolling_mean_lag7_t180 = roll_meanr(lag_t7, 180),
    
    rolling_sd_lag7_t7 = roll_sdr(lag_t7, 7),
    rolling_sd_lag7_t30 = roll_sdr(lag_t7, 30),
    
    rolling_mean_lag28_t7 = roll_meanr(lag_t28, 7),
    rolling_mean_lag28_t30 = roll_meanr(lag_t28, 30),
    # rolling_mean_lag28_t180 = roll_meanr(lag_t28, 180),
    
    rolling_sd_lag28_t7 = roll_sdr(lag_t28, 7),
    rolling_sd_lag_28_t30 = roll_sdr(lag_t28, 30)
    ),
    
    by = "id"]
  
  
  # Drop NAs from first days
  sales <- sales[d >= 1914 | !is.na(rolling_mean_lag28_t30)]
  
  # dim(sales)
  # [1] 45912010       28
 
  
  ## Third way to reduce size: Drop long zero-demand periods for every product
  
  nb_days <- 60  # number of consecutively days with 0 demand
  print(paste("Periods with", nb_days, "consecutively days with 0 demand, have been droped from the dataset"))
  
  sales[, orig := .I]
  sales[, rleLength := {rr <- rle(demand); rep(rr$length, rr$length)}, by = c("item_id", "store_id")]
  
  sales <- sales[!(demand==0 & rleLength > nb_days), ][order(orig)][,orig := NULL]
  
  gc()
  return(sales)
  
}
                   
                                  
                                  