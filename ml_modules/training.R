##################################
#                                #
#  Modeling / Training           #
#                                #
##################################


library(dplyr)
library(data.table)
library(lightgbm)
library(tictoc)



## 01, Import training dataset

# train <- readRDS("../kitematic/temp/train.rds")

head(train)
dim(train)   # 40150972       89
str(train)


# Select Features to be use in training

features <- c("store_id", "item_id",
              
              "sell_price", "sell_price_diff", "sell_price_rel_diff", "sell_price_cumrel", "sell_price_roll_sd7",
              "nb_stores", "diff_from_mean_price", "rel_diff_from_mean_price", "best_price",
              
              "wday", "month", "year",
              "event_name_1", "event_type_1",
              "snap_CA", "snap_TX", "snap_WI",       
              
              "day_month", "day_quarter", "day_year",
              "week_month", "week_quarter", "week_year", "weekend",
              "month_quarter", "quart", "semester",
              
              "wday_s1", "wday_c1", "wday_s2" , "wday_c2", "wday_s3", "wday_c3",
              "month_s1", "month_c1", "month_s2", "month_c2", "month_s3","month_c3",
              
              "wday_rbf_1", "wday_rbf_2", "wday_rbf_3", "wday_rbf_4", "wday_rbf_5", "wday_rbf_6", "wday_rbf_7",
              
              "month_rbf_1", "month_rbf_2", "month_rbf_3", "month_rbf_4", "month_rbf_5", "month_rbf_6", "month_rbf_7" ,"month_rbf_8", "month_rbf_9",
              "month_rbf_10", "month_rbf_11", "month_rbf_12",
              
              "has_event",
              
              "dept_id", "cat_id", "state_id", 
              
              "lag_t1", "lag_t2", "lag_t3", "lag_t4",  "lag_t5", "lag_t6", "lag_t7", "lag_t8", "lag_t14", "lag_t21", "lag_t28",
              "mean_last_month",
              
              "rolling_mean_lag7_t7", "rolling_mean_lag7_t30",
              "rolling_sd_lag7_t7", "rolling_sd_lag7_t30",
              
              "rolling_mean_lag28_t7", "rolling_mean_lag28_t30",
              "rolling_sd_lag28_t7", "rolling_sd_lag_28_t30",
              
              "rleLength",
              
              "snap")

setdiff(colnames(train), features)
# Don't include: [1] "d"      "id"     "demand"


# Which of these features should be treaten as categorical

categoricals <- c("store_id", "item_id", "dept_id", "cat_id", "state_id",  
                  
                  "wday", "day_month", "day_quarter", "day_year",
                  
                  "week_month", "week_quarter", "week_year", "weekend",
                  
                  "month", "month_quarter", "quart","semester", "year",
                  
                  "event_name_1", "event_type_1", "has_event",
                  
                  "snap_CA", "snap_TX", "snap_WI", "snap",
                  
                  "nb_stores", "rleLength",
                  
                  "best_price")



## Separate submission data and reconstruct id columns
test <- train[d >= 1914]
test[, id := paste(id, ifelse(d <= 1941, "validation", "evaluation"), sep = "_")]
test[, F := paste0("F", d - 1913 - 28 * (d > 1941))]


# Keep 1 month of validation data
flag <- train$d < 1914 & train$d >= 1914 - 28
valid_data <- data.matrix(train[flag, features, with = FALSE])
valid <- lgb.Dataset(data = valid_data, categorical_feature = categoricals, label = train[["demand"]][flag])


# Final preparation of training data
flag <- train$d < 1914 - 28
y <- train[["demand"]][flag]

train <- data.matrix(train[flag, features, with = FALSE])
train <- lgb.Dataset(train, categorical_feature = categoricals, label = y)

gc()



# Define parameters for training

params <- list(objective = "poisson",
               metric ="rmse",
               force_row_wise = TRUE,
               learning_rate = 0.075,
               sub_feature = 0.8,
               sub_row = 0.75,
               bagging_freq = 1,
               lambda_l2 = 0.1,
               nthread = 8)


tic()
lgb_model <- lgb.train(params, train, num_boost_round = 4000, 
                       eval_freq = 200, early_stopping_rounds = 600,
                       valids = list(valid = valid))

toc()

# Save model
cat("Best score:", lgb_model$best_score, "at", lgb_model$best_iter, "iteration") 
saveRDS(lgb_model, file = "lgb_model_v0.rds")

# Check importances
imp <- lgb.importance(lgb_model)
View(imp)
lgb.plot.importance(imp, 20, cex = 0.9) 



