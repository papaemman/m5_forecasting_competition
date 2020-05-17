##################################
#                                #
#  Modeling / Training           #
#                                #
##################################

## 00. Load packages ----

library(tidyverse)
library(data.table)
library(RcppRoll)
library(lightgbm)
library(tictoc)

fh = 28


## 01. Load the dataset ----

# tr = dt
# rm(dt)

tr <- readRDS("../kitematic/temp/dt_full_train.rds")  # 46027957      232

tables()
class(tr)
gc()

tr[1:3, 1:10]
dim(tr)
str(tr)



## 02. Preprocesssing steps ----

## Drop zero-demand periods
nrow(unique(dt[, c("item_id")]))
dt <- drop_zero_demand_periods(dt, nb_days = 60)
gc()



## 03. Select Features to be use in training ----

source("ml_modules/features.R")

setdiff(features, colnames(tr))  # Errors
setdiff(colnames(tr), features)  # Features to not include in model


# Which of these features should be treaten as categorical
categoricals
setdiff(categoricals, features)      # Errors
setdiff(categoricals, colnames(tr))  # Errors


## 04. Feature Selection (From importances) ----

# features <- imp_1$Feature[1:80]
# categoricals <- categoricals[categoricals %in% features]


## 05. Build multiple different models ----

stat_total <- read.csv("data/processed/stat_total.csv", stringsAsFactors = F)
stores <- unique(stat_total$store_id)
departments <- unique(stat_total$dept_id)

# Define the rights order in tr dataset (alphabetical order)
# stores <- c("CA_1", "CA_2", "CA_3", "CA_4", "TX_1", "TX_2", "TX_3", "WI_1", "WI_2", "WI_3")
# departments <- c("FOODS_1", "FOODS_2", "FOODS_3", "HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2")


## - A. One model per STORE (10 models) -
# df <- expand.grid(stores, stringsAsFactors = F)
# colnames(df) <- c("store_id")
# df

## - B. One model per DEPARTMENT (7 models) -
df <- expand.grid(departments, stringsAsFactors = F)
colnames(df) <- c("dept_id")
df

# - C. One model per STORE - DEPARTMENT (70 models) -
# df <- expand.grid(stores,departments, stringsAsFactors = F)
# colnames(df) <- c("store_id", "dept_id")
# dim(df)
# head(df)

## 06. Training ----

# Save full training data
tr_full = tr


for (i in 1:nrow(df)){
  
  # Test
  # i=1
  
  # Print data subsets
  store <- df[i,"store_id"]
  dept <- df[i,"dept_id"]
  print(paste("Store:", store, "| Dept:", dept))
  
  
  ## - 1. One model for all data -
  # tr <- tr_full
  
  ## - 1A. One model per STORE (10 models) -
  # tr <- tr_full[store_id == which(store == stores),]
  
  ## - 1B. One model per DEPARTMENT (7 models) -
  # tr <- tr[dept_id == which(dept == departments),]
  tr <- tr_full[dept_id == dept,]
  
  
  ## - 1C. One model per STORE - DEPARTMENT (70 models) -
  # tr <- tr_full[store_id == which(store == stores) & dept_id == which(dept == departments),]
  
  
  ## Deal with NAs
  
  # dim(tr)    
  # anyNA(tr)                                          # TRUE
  # nrow(unique(tr[,j = c("item_id", "store_id")]))    # Total items (5150)
  # tr <- tr[!is.na(rolling_mean_lag28_t180),]
  # dim(tr)                                            # 7.125.749     232
  # nrow(unique(tr[,j = c("item_id", "store_id")]))    # Total items (5148)   (Note: 2 items completely lost!)
  # anyNA(tr)                                          # FALSE
  # gc()
  
  ## Sanity check
  # View(tr[item_id == "HOUSEHOLD_2_002" & store_id == 'CA_1', c("d", "sales", "Max", "lag_t1", "lag_t2")])
  
  
  
  ## 2. Data construction parameters (binning)
  
  data_params <- list(
    max_bin = 255,                          # max number of bins that feature values will be bucketed in
    min_data_in_bin = 3,                    # minimal number of data inside one bin 
    bin_construct_sample_cnt = nrow(tr),    # number of data that sampled to construct histogram bins | aliases: subsample_for_bin
    data_random_seed = 33, 
    is_enable_sparse = TRUE,                # used to enable/disable sparse optimization | aliases: is_sparse, enable_sparse, sparse
    feature_pre_filter = F,
    weight_column = ""
  )

  
  ## // Train without validation data // 
  
  # y <- tr$sales
  # tr <- data.matrix(tr[,..features,])
  # tr <- lgb.Dataset(tr, categorical_feature = categoricals, label = y,  params = data_params)
  # gc()
  
  ## // Keep 1 month (last month) of validation data //
  
  # Validation data
  flag <- tr$d >= 1914 - 28
  sum(is.na(tr[flag, rolling_mean_lag28_t180])) # NAs in validation
  
  valid_data <- data.matrix(tr[flag, ..features])
  valid_data <- lgb.Dataset(data = valid_data, categorical_feature = categoricals, label = tr[["sales"]][flag], params = data_params, free_raw_data = T)
  
  prepare_custom_evaluation_metric_dependencies()
  
  # Training data
  tr <- tr[!is.na(rolling_mean_lag28_t180),]  # Drop NA values   -  anyNA(tr)
  flag <- tr$d < 1914 - 28 
  y <- tr[["sales"]][flag]
  train_data <- data.matrix(tr[flag,..features,])
  train_data <- lgb.Dataset(data = train_data, categorical_feature = categoricals, label = y, params = data_params, free_raw_data = T)
  gc()
  
  
  ## Define parameters for training
  
  # DOCUMENTATION: https://lightgbm.readthedocs.io/en/latest/Parameters.html
  # R API Documentation: https://lightgbm.readthedocs.io/en/latest/R/index.html
  
  params <- list(

    # 1. Core parameters ----
    task = "train",
    
    objective = "tweedie",           # "regression", "regression_l1", "poisson", "huber", "tweedie" | aliases: objective_type, app, application
    
    boosting = "gbdt",               # Boosting type: "gbdt", "rf", "dart", "goss"
    
    # num_iterations = 3000,         # number of training rounds            | aliases: num_iteration, n_iter, num_tree, num_trees, num_round, num_rounds, num_boost_round, n_estimators
    learning_rate = 0.03,            # shrinkage rate                       | aliases: shrinkage_rate, eta
    num_leaves = 2**11 - 1,          # max number of leaves in one tree     | aliases: num_leaf, max_leaves, max_leaf
    tree_learner = "serial",         # "serial", "feature", "data", "voting"
    
    seed = 33,
    nthread = 8,
    device_type = "cpu",
    
    # 2. Learning Control Parameters  ----
    
    force_col_wise = TRUE,
    force_row_wise = FALSE,
    
    max_depth = 50, # -1
    min_data_in_leaf = 20, # 20              # minimal number of data in one leaf   | aliases: min_data_per_leaf, min_data, min_child_samples
    
    bagging_fraction = 0.6,             # randomly select part of data without resampling  | aliases: sub_row, subsample, bagging
    bagging_freq = 1,                   # frequency for bagging                            | subsample_freq 
    bagging_seed = 33,
    
    feature_fraction = 0.6, # 1             # for boosting "rf" |  aliases: sub_feature, colsample_bytree
    feature_fraction_bynode = 0.5,      # 
    feature_fraction_seed = 33,
    
    lambda_l1 = 0.1, # 0                  #  | aliases: reg_alpha
    lambda_l2 = 0.1, # 0                 #  | aliases: reg_lambda, lambda
  
    # min_gain_to_split = 0.01,
    
    extra_trees = FALSE,
    extra_seed = 33, 
    
    
    # 3. I/O parameters ----
    
    # Dataset parameters
    tweedie_variance_power= 1.1,
    # metric = "rmse",

    # 4. Objective parameters ----
    boost_from_average = F
    
    # 5. Metric Parameters ----

  )
  
  
  ## // Train model //
  # tic()
  # lgb_model <- lgb.train(params = params, data = train_data,
  #                        valids = list(valid = valid_data), eval_freq = 50, early_stopping_rounds = 400, metric = "rmse", # Validation parameters
  #                        nrounds = 1000,  
  #                        categorical_feature = categoricals,
  #                        verbose = 1, record = TRUE, init_model = NULL, colnames = NULL,
  #                        callbacks = list(), reset_data = FALSE)
  # 
  # toc()
  # 
  # cat("Best score:", lgb_model$best_score, "at", lgb_model$best_iter, "iteration") 
  
  
  ## // Train with Custom evaluation function //
  # lgb_model <- lgb.train(params = params, data = train_data,
  #                        valids = list(valid = valid_data), eval_freq = 50, early_stopping_rounds = 400, eval = custom_rmse_metric, metric = "rmse", # Validation parameters
  #                        nrounds = 1000,  
  #                        categorical_feature = categoricals,
  #                        verbose = 1, record = TRUE, init_model = NULL, colnames = NULL,
  #                        callbacks = list(), reset_data = FALSE)
  
  
  ## // Train with Custom evaluation function - WRMSSE //
  
  
  ## Custom evaluation function
  lgb_model <- lgb.train(params = params, data = train_data,
                         valids = list(valid = valid_data), eval_freq = 50, early_stopping_rounds = 400, # Validation parameters
                         eval = custom_wrmsse_metric, # SOS:  wrmsse_den, weights_df, fh
                         metric = "rmse", 
                         nrounds = 1000,  
                         categorical_feature = categoricals,
                         verbose = 1, record = TRUE, init_model = NULL, colnames = NULL,
                         callbacks = list(), reset_data = FALSE)
  
  cat("Best rmse score:", lgb_model$best_score, "at", lgb_model$best_iter, "iteration") 
  cat("Best wrmsse score:", min(unlist(lgb_model$record_evals$valid$wrmsse)), "at", which.min(unlist(lgb_model$record_evals$valid$wrmsse)), "iteration") 
  
  # tic()
  # lgb_model <- lgb.train(params = params, data = tr)
  # toc()
  
  
  ## Check importances (Why so slow?)
  # imp <- lgb.importance(lgb_model)
  # View(imp)
  # lgb.plot.importance(imp, 20, cex = 0.9) 
  # write.csv(imp, file = "ml_modules/imp/importnaces_CA.csv", row.names = F)
  
}


## Define custom evaluation metric - function

# I need first to re-write the rmse evaluation function as sanity check
custom_rmse_metric <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  # preds <- labels + 1
  
  rmse <- sqrt(sum((labels-preds)^2)/ length(preds))

  ls <- list(name = "rmse",
             value = rmse,
             higher_better = FALSE)
  
  return(ls)
}

custom_rmse_metric_v2 <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  # preds <- labels + 1
  
  ls <- list(name = "length_labels",
             value = length(labels),
             higher_better = FALSE)
  return(ls)
}


# WRMSSE

prepare_custom_evaluation_metric_dependencies <- function(){
  
  ## Load denominator for wrmsSe
  # wrmsse_den <- read.csv("data/wrmsse_den_without_last_28_v2.csv")
  wrmsse_den <- read.csv("data/wrmsse_den_v2.csv")
  
  ## Load weights for Wrmsse
  weights_df <- read.csv("data/bts_weights_v2.csv")
  
  ## The validation data are:
  flag <- tr$d >= 1914 - 28
  item_group_frc <- tr[flag, c("store_id", "item_id")]
  # dim(item_group_frc)
  print(paste("Total items:", item_group_frc %>% unique() %>% nrow()))
  
  ## Select the approprate store_ids - item_ids
  
  wrmsse_den <<- merge(item_group_frc, wrmsse_den , by = c("store_id", "item_id"), all.x = T, sort = F)
  # wrmsse_den[,c("store_id", "item_id")] %>% unique() %>% nrow()
  # dim(wrmsse_den)
  
  weights_df <<- merge(item_group_frc, weights_df , by = c("store_id", "item_id"), all.x = T, sort = F)
  # weights_df[, c("store_id", "item_id")] %>% unique() %>% nrow()
  # dim(weights_df)
  
  print("wrmsse_den and weights_df datasets loaded...")
}


custom_wrmsse_metric <- function(preds, dtrain) { #  wrmsse_den, weights_df, fh
  
  ## Test

  # - labels          : Ground truth values from the validation set
  # - preds           : Predictions from the model
  # - wrmsse_den      : Denominator for every time serie (item_id, dept_id)
  # - weight          : Error weight for every time serie
  # - item_group_frc  : store_id, item_id for the values in validation dataset (labels)
  
  # labels <- getinfo(valid_data, "label")
  # preds <- runif(n = length(labels), min = 0, max = 10)
  # length(labels)/28
  # dim(wrmsse_den)
  # dim(weights_df)
  # dim(item_group_frc)
  
  
  # Get the ground truth values from the validation dataset
  labels <- getinfo(dtrain, "label")
  
  
  ## 1. Calculate the rmsse separately for every time-serie
  
  # Test
  # rmsse <- sqrt( sum((preds_per_ts[[1]] - labels_per_ts[[1]])^2) / ( wrmsse_den[1, "den"] * fh) )
  # rmse <- sqrt(sum((labels_per_ts[[1]]-preds_per_ts[[1]])^2)/ fh)
  
  wrmsse_den_unique <- wrmsse_den %>% unique()
  
  preds_per_ts <- preds %>% split(wrmsse_den_unique[,c("store_id", "item_id")])
  labels_per_ts <- labels %>% split(wrmsse_den_unique[,c("store_id", "item_id")])
  
  length(preds_per_ts);length(labels_per_ts);dim(wrmsse_den_unique)
  
  
  rmsse_per_ts <- mapply(FUN = function(x, y, z){
    sqrt( sum((x - y)^2) / ( z * fh) )
  }, preds_per_ts, labels_per_ts, as.list(wrmsse_den_unique$den), USE.NAMES = T, SIMPLIFY = T)
  
  
  ## 2. Combine the rmsse for all time-series, using the appropriate weights_df
  
  # Test
  # head(weights_unique)
  # head(rmsse_per_ts)
  
  weights_unique <- unique(weights_df)
  
  wrmsse = sum(rmsse_per_ts*weights_unique$weight)
  
  
  ## 3. Return results
  ls <- list(name = "wrmsse",
             value = wrmsse,
             higher_better = FALSE)
  
  return(ls)
}







 

