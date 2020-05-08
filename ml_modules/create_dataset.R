##################################
#                                #
#  Modeling / Training           #
#                                #
##################################

## 00. Load packages

library(tidyverse)
library(data.table)
library(RcppRoll)
library(lightgbm)
library(tictoc)

## Source dependencies
source("ml_modules/create_data.R")
source("ml_modules/fe_sales.R")



## Define parameters
nrows = Inf                        # How many sales data to import
fh = 28                            # Forecasting horizon
max_lags = 366                     # Max lags (features)
tr_last = 1913                     # Training last day
fday = as.IDate("2016-04-25")      # Forecasting day (first day)



## Create (merge) datasets
# dt <- create_dt(is_train = TRUE, nrows = nrows,
#                 fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday,
#                 filter_params = "dept_id == 'HOBBIES_1' ")

dt <- create_dt(is_train = TRUE, nrows = nrows,
                fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday,
                filter_params = NA)
gc()


## Create hierarhy sales features
dt <- create_hierarhy_sales_features(dt)
gc()


## Preprocessing steps and add more features
dt <- add_more_features(dt = dt)
gc()


## Create Sales features
dt <- create_sales_features(dt)
gc()


## Save train dataset (without drop NAs)
dim(dt) # [1] 46027957      211
dt[,.N, by = c("store_id", "item_id")] %>% nrow() #  [1] 30490

format(object.size(dt), unit = "auto") #  "58.5 Gb"
saveRDS(object = dt, file = "../kitematic/temp/dt_full_train.rds")


## Save train dataset after drop NAs from first days (Use the column with maximum lags)
dt <- dt[!is.na(rolling_mean_lag28_t180), ]  # faster than na.omit()
anyNA(dt)

dim(dt) # [1] 39720284      211
dt[,.N, by = c("store_id", "item_id")] %>% nrow()  # [1] 30421  (I lost some products)

format(object.size(dt), unit = "auto") #  [1] "50.5 Gb"
saveRDS(object = dt, file = "../kitematic/temp/dt_full_train_without_NAs.rds")


