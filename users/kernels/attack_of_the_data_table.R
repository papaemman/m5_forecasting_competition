############################################################
#                                                          #
# M5 Forecast: attack of the data.table [Score = 0.58724]  #
#                                                          #
############################################################

## REALLY HEAVY TASKS!

## Kernel's link: https://www.kaggle.com/mayer79/m5-forecast-attack-of-the-data-table

## // My Notes //
# General Approach: 
# - Bottom up (predictions for every time series in aggregation level 12)
# - one global ml model (lightgbm),
# - feature engineering: create lag and rolling mean features from sales, prices, dummy calendar events


## Notebook ##

# The aim of this notebook is to provide a solution to the M5 Forecast Accuracy competition by two core pieces:
# 1. R's super fast data.table package, a data.frame on steroids. (https://cran.r-project.org/web/packages/data.table/index.html)
# 2. Microsoft's boosting algorithm LightGBM. (https://lightgbm.readthedocs.io/en/latest/)

# Some ideas are stolen from the cool kernel Very fst Model (https://www.kaggle.com/ragnar123/very-fst-model),
# some come from my Keras kernel (https://www.kaggle.com/mayer79/m5-forecast-keras-with-categorical-embeddings-v2)
# and some are new.


## 00. Load packages ----

library(data.table)
library(RcppRoll)
library(dplyr)
library(lightgbm)


## 01. Import data -----
calendar <- fread("data/raw/calendar.csv")
selling_prices <- fread("data/raw/sell_prices.csv")
sample_submission <- fread("data/submissions/sample_submission.csv")
sales <- fread("data/raw/sales_train_validation.csv")


# Garbage collection helper function
igc <- function() {
  invisible(gc()); invisible(gc())   
}

igc()


## 02. Prepare data -----

# We first prepare each of the data sets.
# After this, we merge them together to one big file. 
# In order to keep RAM consumption acceptable, we kick out the early lines.

# 1. Calendar
str(calendar)
calendar[, `:=`(date = NULL, weekday = NULL, d = as.integer(substring(d, 3)))]

cols <- c("event_name_1", "event_type_1") #, "event_name_2", "event_type_2")

calendar[, (cols) := lapply(X = .SD, FUN = function(z) as.integer(as.factor(z))), .SDcols = cols]
head(calendar)


# 2. Selling prices                
selling_prices[, `:=`(
  sell_price_diff = sell_price - dplyr::lag(sell_price),
  sell_price_rel_diff = sell_price / dplyr::lag(sell_price) - 1,
  sell_price_cumrel = (sell_price - cummin(sell_price)) / (1 + cummax(sell_price) - cummin(sell_price)),
  sell_price_roll_sd7 = roll_sdr(sell_price, n = 7)
), by = c("store_id", "item_id")]

head(selling_prices)


# 3. Sales

# Rename id column
sales[, id := gsub(pattern = "_validation", replacement = "", x = id)]

# Filter rows
# sales <- sales[1:10]

# Create empy datatable for every forecasting value and cbind in sales dataset
empty_dt = matrix(NA_integer_, ncol = 2 * 28, nrow = 1, dimnames = list(NULL, paste("d", 1913 + 1:(2 * 28), sep = "_")))
sales <- cbind(sales, empty_dt)

View(sales)

# Reshape (melt) from wide in long format
sales <- data.table::melt(sales,
                          id.vars = c("id", "item_id", "dept_id", "cat_id", "store_id", "state_id"), 
                          variable.name = "d", value = "demand")

sales[, d := as.integer(substring(d, 3))]

head(sales)
format(object.size(sales),"auto")
igc()

# Reduce size (filter for d > 1000)
sales <- sales[d >= 1000]

## Sales Feature construction: Subset of features from very fst kernel
stopifnot(!is.unsorted(sales$d))

sales[, lag_t28 := dplyr::lag(demand, 28), by = "id"]

sales[, `:=`(rolling_mean_t7 = roll_meanr(lag_t28, 7),
             rolling_mean_t30 = roll_meanr(lag_t28, 30),
             rolling_mean_t60 = roll_meanr(lag_t28, 60),
             rolling_mean_t90 = roll_meanr(lag_t28, 90),
             rolling_mean_t180 = roll_meanr(lag_t28, 180),
             rolling_sd_t7 = roll_sdr(lag_t28, 7),
             rolling_sd_t30 = roll_sdr(lag_t28, 30)), 
      by = "id"]
igc()

# Filter rows
sales <- sales[d >= 1914 | !is.na(rolling_mean_t180)]

# Save sales dataset
sales <- readRDS("sales.rds")


## 03. Merge all datasets -----

sales <- calendar[sales, on = "d"]
igc()

# Merge selling prices to sales and drop key
train <- selling_prices[sales, on = c('store_id', 'item_id', 'wm_yr_wk')][, wm_yr_wk := NULL]

# Save train dataset and remove other datasets
saveRDS(object = train, file = "data/processed/train_nb_data_table.rdss")
rm(sales, selling_prices, calendar)
igc()

# Turn non-numerics to integer
cols <- c("item_id", "dept_id", "cat_id", "store_id", "state_id")
train[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]


## 04. Create train, validation and test datasets ----

# Separate submission data and reconstruct id columns
test <- train[d >= 1914]
test[, id := paste(id, ifelse(d <= 1941, "validation", "evaluation"), sep = "_")]
test[, F := paste0("F", d - 1913 - 28 * (d > 1941))]


# Keep 1 month of validation data
flag <- train$d < 1914 & train$d >= 1914 - 28
valid <- lgb.Dataset(data.matrix(train[flag, x, with = FALSE]), 
                     label = train[["demand"]][flag])


# Final preparation of training data
flag <- train$d < 1914 - 28
y <- train[["demand"]][flag]
train <- data.matrix(train[flag, x, with = FALSE])
igc()
train <- lgb.Dataset(train, label = y)
igc()


## 05. Machine Learning training -----

# It's time for LightGBM fun. Parameters are lent from Very fst Model notebook.

# Covariables used
x <- c("wday", "month", "year", 
       "event_name_1", "event_type_1", #"event_name_2", "event_type_2", 
       "snap_CA", "snap_TX", "snap_WI",
       "sell_price", "sell_price_rel_diff", "sell_price_cumrel", "sell_price_roll_sd7",
       "lag_t28", "rolling_mean_t7", "rolling_mean_t30", "rolling_mean_t60", 
       "rolling_mean_t90", "rolling_mean_t180", "rolling_sd_t7", "rolling_sd_t30",
       "item_id", "dept_id", "cat_id", "store_id", "state_id")


# Parameter choice
params = list(objective = "poisson",
              metric = "rmse",
              seed = 20,
              learning_rate = 0.08,
              lambda = 0.1,
              num_leaves = 63,
              bagging_fraction = 0.66,
              bagging_freq = 1, 
              colsample_bytree = 0.77)

# Training
fit <- lgb.train(params = params, data = train, num_boost_round = 2000, 
                 eval_freq = 100, early_stopping_rounds = 400, 
                 valids = list(valid = valid))

# Importance
imp <- lgb.importance(fit)
lgb.plot.importance(imp, top_n = Inf)



## 06. Create Submission file -----

pred <- predict(fit, data.matrix(test[, x, with = FALSE]))
test[, demand := pmax(0, pred)]

test_long <- dcast(test, id ~ F, value.var = "demand")

submission <- merge(sample_submission[, .(id)], 
                    test_long[, colnames(sample_submission), with = FALSE], 
                    by = "id")

fwrite(submission, file = "submission.csv", row.names = FALSE)
