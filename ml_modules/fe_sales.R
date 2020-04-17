##################################
#                                #
# Feature Engineering: Sales     #
#                                #
##################################

## Source dependencies
# source("modules/main.R")
library(data.table)
library(RcppRoll)


create_sales_data <- function(){
  
  # Import data 
  sales <- fread("data/raw/sales_train_validation.csv", stringsAsFactors = F)
  
  
  # Add empty observations
  sales[, id := gsub("_validation", "", id)]                         
  empty_dt = matrix(NA_integer_, ncol = 2 * 28, nrow = 1, dimnames = 
                      list(NULL, paste("d", 1913 + 1:(2 * 28), sep = "_")))
  
  sales <- cbind(sales, empty_dt)
  
  # Sales: Reshape
  sales <- melt(sales,
                id.vars = c("id", "item_id", "dept_id", "cat_id", "store_id", "state_id"), 
                variable.name = "d", value = "demand")
  
  sales[, d := as.integer(substring(d, 3))]
  sales[store_id == "CA_1"][order(item_id, d)]
  
  # Sales: Reduce size
  # sales <- sales[d >= 1000]
  
  
  # Sales: Feature construction
  
  sales[, `:=`(lag_t1  = dplyr::lag(demand, 1),
               lag_t6  = dplyr::lag(demand, 6),
               lag_t7  = dplyr::lag(demand, 7),
               lag_t8  = dplyr::lag(demand, 8),
               lag_t14 = dplyr::lag(demand, 14),
               lag_t21 = dplyr::lag(demand, 21),
               lag_t28 = dplyr::lag(demand, 28)),
        by = "id"]
  
  
  sales[, `:=`(
    mean_last_month = (lag_t7 + lag_t14 + lag_t21 + lag_t28)/4,
    
    rolling_mean_lag7_t7 = roll_meanr(lag_t7, 7),
    rolling_mean_lag7_t120 = roll_meanr(lag_t7, 120),
    rolling_mean_lag7_t120 = roll_meanr(lag_t7, 180),
    
    rolling_mean_t7 = roll_meanr(lag_t28, 7),
    rolling_mean_t30 = roll_meanr(lag_t28, 30),
    rolling_mean_t60 = roll_meanr(lag_t28, 60),
    rolling_mean_t90 = roll_meanr(lag_t28, 90),
    rolling_mean_t180 = roll_meanr(lag_t28, 180),
    
    rolling_sd_t7 = roll_sdr(lag_t28, 7),
    rolling_sd_t30 = roll_sdr(lag_t28, 30)), 
    by = "id"]
  
  # Drop NAs from first days
  sales <- sales[d >= 1914 | !is.na(rolling_mean_t180)]
  
  rm(empty_dt);gc()
  return(sales)
  
}


format(object.size(sales), unit = "auto") # "9.6 Gb"
dim(sales) # 53.723.380, 27
gc()                                  
                                  
                                  
                                  