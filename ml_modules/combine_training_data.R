##################################
#                                #
# Combining training datasets    #
#                                #
##################################

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(RcppRoll)
library(data.table)
library(lightgbm)


## Import preprocessd data

calendar <- create_calendar_data()
prices <- create_prices_data()
sales <- create_sales_data()


View(calendar)
View(prices)
View(sales)

dim(calendar)  # 1969     55
dim(prices)    # 7125900  12
dim(sales)     # 53723380 27

View(sales[id == "HOBBIES_1_001_CA_2"][order(c("item_id", "d", decreasing = F))])


## 03. Merge all -----

# Merge calendar to sales
sales <- calendar[sales, on = "d"]

# Merge selling prices to sales and drop key
sales <- prices[sales, on = c('store_id', 'item_id', 'wm_yr_wk')][, wm_yr_wk := NULL]
rm(prices, calendar)
gc()


# Rename dataset
train <- sales  
dim(train) # 56467480       81
rm(sales)
gc()


# Turn non-numerics to integer
cols <- c("item_id", "dept_id", "cat_id", "store_id", "state_id")
train[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]


# 3. Save train dataset and remove other datasets
format(object.size(train), unit = "auto") # 32.4 Gb
saveRDS(object = train, file = "../kitematic/train.rds")


# 4. Add more features
head(train)

# Extra column snap
train[, snap := ifelse(state_id == "CA", snap_CA, ifelse(state_id == "TX", snap_TX, snap_WI))]

str(train)



# Features used

x <- c("wday", "month", "year", 
       "day_month",  "day_quarter", "day_year",
       "week_month", "week_quarter", "week_year", "weekend",
       "month_quarter", "quart", "semester",
       
       "event_name_1", "event_type_1",
       
       "snap_CA", "snap_TX", "snap_WI", "snap",
       
       "wday_s1",  "wday_c1", "wday_s2", "wday_c2", "wday_s3", "wday_c3",
       "month_s1",  "month_c1", "month_s2", "month_c2", "month_s3", "month_c3",
       
       "wday_rbf_1",  "wday_rbf_2", "wday_rbf_3", "wday_rbf_4", "wday_rbf_5", "wday_rbf_6", "wday_rbf_7",
       "month_rbf_1", "month_rbf_2", "month_rbf_3","month_rbf_4","month_rbf_5","month_rbf_6","month_rbf_7",
       "month_rbf_8","month_rbf_9","month_rbf_10", "month_rbf_11", "month_rbf_12",
       
       "sell_price", "sell_price_diff", "sell_price_rel_diff", "sell_price_cumrel", "sell_price_roll_sd7",
       "diff_from_mean",
       
       "stores",
       
       "lag_t1", "lag_t6",  "lag_t7", "lag_t8","lag_t14", "lag_t21",  "lag_t28", 
       "mean_last_month", 
       
       "rolling_mean_t7", "rolling_mean_t30", "rolling_mean_t60", "rolling_mean_t90", "rolling_mean_t180",
       "rolling_sd_t7", "rolling_sd_t30",
       "rolling_mean_lag7_t7",   "rolling_mean_lag7_t120",
       
       "item_id", "store_id",
       "state_id", "cat_id", "dept_id")

setdiff(colnames(train), x)
# Don't include: id, d, demand

categoricals <- c("store_id", "item_id", "dept_id", "cat_id", "state_id",  
                  "wday", "day_month", "day_quarter", "day_year",
                  "week_month", "week_quarter", "week_year", "weekend",
                  "month", "month_quarter", "quart","semester", "year", 
                  "snap_CA", "snap_TX", "snap_WI", "snap")



# Separate submission data and reconstruct id columns
test <- train[d >= 1914]
test[, id := paste(id, ifelse(d <= 1941, "validation", "evaluation"), sep = "_")]
test[, F := paste0("F", d - 1913 - 28 * (d > 1941))]


# Keep 1 month of validation data
flag <- train$d < 1914 & train$d >= 1914 - 28
valid <- lgb.Dataset(data = train[flag, x, with = FALSE], categorical_feature = categoricals, label = train[["demand"]][flag])

# valid <- lgb.Dataset(data.matrix(train[flag, x, with = FALSE]), label = train[["demand"]][flag])


# Final preparation of training data
flag <- train$d < 1914 - 28
y <- train[["demand"]][flag]
# train <- data.matrix(train[flag, x, with = FALSE])
train <- train[flag, x, with = FALSE]
gc()
train <- lgb.Dataset(train[flag, x, with = FALSE], label = train[["demand"]][flag])
gc()



params <- list(objective = "poisson",
          metric ="rmse",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 8)


fit <- lgb.train(params, train, num_boost_round = 4000, 
                 eval_freq = 200, early_stopping_rounds = 600,
                 valids = list(valid = valid))


imp <- lgb.importance(m_lgb)
lgb.plot.importance(imp, 20, cex = 0.9) 

