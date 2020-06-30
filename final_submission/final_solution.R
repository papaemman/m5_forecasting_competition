#######################
#                     #
#   Final solution    #
#                     #
#######################

# 1. Feature engineering (few feature)
# 2. Bayesian hyperparameter optimization
# 3. Separate model for each store-department
# 4. Lightgbm with custome evaluation function



## 01. Load packages ----

library(tidyverse)
library(data.table)
library(RcppRoll)
library(ParBayesianOptimization)
library(tictoc)
library(lightgbm)


## // TRAINING // ----

## 01. Load and merge datasets ----

# Import train complete sales train validation dataset
dt <- fread("data/raw/sales_train_validation.csv")

# Transform wide format to long format
dt <- melt(dt,
           measure.vars = patterns("^d_"),
           variable.name = "d",
           value.name = "sales")

# calendar
calendar <- fread("data/raw/calendar.csv")
dt <- calendar[dt, on = "d"]
rm(calendar)


# price
prices_raw <- fread("data/raw/sell_prices.csv")
dt <- prices_raw[dt, on = c("store_id", "item_id", "wm_yr_wk")]


## 02. Data preprocessing | (calendar-prices Feature engineering) ----

# Omit days with NA in price
dt <- na.omit(dt)  # before: 58.327.370 - after: 46.027.957

# Drop columns
dt[, `:=`(wm_yr_wk = NULL, date = NULL, weekday = NULL)]

# Fix Data types
str(dt)

dt[, d := as.integer(substring(d, first = 3))]


## 03. sales Feature engineering ----

# If I use features with lags>28, then I don't need to impute feature vectors with predictions.

dt

## Lags
dt[order(d), `:=`(lag_t28 = dplyr::lag(sales, 28),
                  lag_t29 = dplyr::lag(sales, 29),
                  lag_t30 = dplyr::lag(sales, 30),
                  lag_t35 = dplyr::lag(sales, 35),
                  lag_t42 = dplyr::lag(sales, 42),
                  lag_t49 = dplyr::lag(sales, 49)),
   by = "id"]


# Rolling features
dt[order(d), `:=`(mean_previous_month = (lag_t28 + lag_t35 + lag_t42 + lag_t49)/4,
                  rolling_mean_lag28_t7 = roll_meanr(lag_t28, 7),
                  rolling_mean_lag28_t30 = roll_meanr(lag_t28, 30),
                  rolling_mean_lag28_t40 = roll_meanr(lag_t28, 40)),
   by = "id"]


# Sanity check
# dt[id == "FOODS_3_823_WI_3_validation", list(sales, lag_t28, lag_t29, lag_t35, lag_t42, lag_t49, mean_previous_month) ] %>% View()


## 04. Add more features ----

# Features from stat_total file
stat_total <- readRDS("data/processed/stat_total.rds")
stat_total <- as.data.table(stat_total)
stores <- as.character(unique(stat_total$store_id))
departments <- as.character(unique(stat_total$dept_id))
stat_total <- stat_total[,c("item_id", "store_id", "lngth", "ADI", "CV2", "pz", "Max")]

dt <- stat_total[dt, on = c("item_id", "store_id")]


## 05. Lightgbm training - Hyperparameter tuning ---- 

## Feature Selection

features <- c("lngth", "ADI", "CV2", "pz", "Max",
              "sell_price",
              "wday", "month", "year",
              "snap_CA", "snap_TX", "snap_WI",                
              "lag_t28", "lag_t29", "lag_t30","lag_t35", "lag_t42", "lag_t49",
              "mean_previous_month", "rolling_mean_lag28_t7", "rolling_mean_lag28_t30", "rolling_mean_lag28_t40",
              "store_id", "state_id")

categoricals <- c("wday", "month", "year","store_id", "state_id")

setdiff(colnames(tr), features)  
setdiff(features, colnames(tr))  
dim(tr)

# Note SOS: Don't add "sales" column in Features!


## Helper functions and objects ----

# 0. Filter data
filter_data <- expand.grid(list(stores, departments), stringsAsFactors = F)
colnames(filter_data) <- c("store_id", "dept_id")
head(filter_data)

# 1. Scoring function
scoringFunction <- function(learning_rate, num_leaves,                                                   # Core parameters
                            max_depth, min_data_in_leaf,                                                 # Learning Control Parameters
                            bagging_fraction, bagging_freq, feature_fraction, feature_fraction_bynode,
                            lambda_l1, lambda_l2,
                            tweedie_variance_power) {                                                    # Objective parameters 
  
  
  ## Test
  
  
  # Define training parameters
  params <- list(
    
    # 1. Core parameters ----
    task = "train",                  # 
    
    objective = "tweedie",           # "regression", "regression_l1", "poisson", "huber", "tweedie"
    boosting = "gbdt",               # Boosting type: "gbdt", "rf", "dart", "goss"
    
    # num_iterations = 3000,         # number of training rounds            | aliases: num_iteration, n_iter, num_tree, num_trees, num_round, num_rounds, num_boost_round, n_estimators
    learning_rate = learning_rate,   # shrinkage rate                       | aliases: shrinkage_rate, eta
    num_leaves = num_leaves,         # max number of leaves in one tree     | aliases: num_leaf, max_leaves, max_leaf
    tree_learner = "serial",         # "serial", "feature", "data", "voting"
    
    seed = 33,
    nthread = 8,
    device_type = "cpu",
    
    # 2. Learning Control Parameters  ----
    
    force_col_wise = TRUE,
    force_row_wise = FALSE,
    
    max_depth = max_depth,
    min_data_in_leaf = min_data_in_leaf,                # minimal number of data in one leaf   | aliases: min_data_per_leaf, min_data, min_child_samples
    
    bagging_fraction = bagging_fraction,                # randomly select part of data without resampling  | aliases: sub_row, subsample, bagging
    bagging_freq = bagging_freq,                        # frequency for bagging                            | subsample_freq 
    bagging_seed = 33,
    
    feature_fraction = feature_fraction,                 # for boosting "rf" |  aliases: sub_feature, colsample_bytree
    feature_fraction_bynode = feature_fraction_bynode,   # 
    feature_fraction_seed = 33,
    
    lambda_l1 = lambda_l1,                               #  | aliases: reg_alpha
    lambda_l2 = lambda_l2,                               #  | aliases: reg_lambda, lambda
    
    # min_gain_to_split = 0.01,
    
    extra_trees = FALSE,
    extra_seed = 33, 
    
    
    # 3. I/O parameters ----
    # 4. Objective parameters ----
    
    tweedie_variance_power= tweedie_variance_power,
    boost_from_average = F
    
    # 5. Metric Parameters ----
    
  )
  
  
  ## Lightgbm Training
  
  lgb_model <- lgb.train(params = params, data = train_data,
                         valids = list(valid = valid_data), eval_freq = 30, early_stopping_rounds = 300, # Validation parameters
                         eval = custom_wrmsse_metric,         # SOS:  wrmsse_den, weights, fh datafiles needed for wrmsse calculations
                         metric = "rmse", 
                         nrounds = 1000,  
                         categorical_feature = categoricals,
                         verbose = -1, record = TRUE, init_model = NULL, colnames = NULL,
                         callbacks = list(), reset_data = FALSE)
  
  
  ## rmse results
  # min(unlist(lgb_model$record_evals$valid$rmse$eval))
  # which.min(unlist(lgb_model$record_evals$valid$rmse$eval))
  
  ## wrmse results
  # min(unlist(lgb_model$record_evals$valid$wrmsse$eval))
  # which.min(unlist(lgb_model$record_evals$valid$wrmsse$eval))
  
  # NOTE:
  # ParBayesianOptimization will maximize the function you provide it, to change your problem to a maximization one, just multiply the RMSE (Score) being output by -1.
  
  gc()
  
  # Get results (Score = wrmse and rmse)
  ls <- list(Score = - min(unlist(lgb_model$record_evals$valid$wrmsse$eval)),
             nrounds_wrmsse = which.min(unlist(lgb_model$record_evals$valid$wrmsse$eval)),
             rmse = min(unlist(lgb_model$record_evals$valid$rmse$eval)),
             nrounds_rmse = which.min(unlist(lgb_model$record_evals$valid$rmse$eval)))
  
  return(ls)
}



# 2. Bounds
bounds <- list(
  # Core parameters
  learning_rate = c(0.01, 0.6),
  num_leaves = c(2L, 10000L),  
  # Learning Control Parameters
  max_depth = c(1L, 6000L),
  min_data_in_leaf = c(5L, 100L),                                                 
  bagging_fraction = c(0.1, 1),
  bagging_freq = c(0L, 100L),
  feature_fraction = c(0.1, 1),
  feature_fraction_bynode = c(0.1, 1),
  lambda_l1 = c(0.00, 2.00),
  lambda_l2 = c(0.00, 2.00),
  # Objective parameters 
  tweedie_variance_power = c(1.00, 1.99)
)

fh = 28


# 3. WRMSSE files
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

# 4. Custom evaluation metric function
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


## Make loop ----

for(i in c(41,51,61)){
  
  # i = 1
  print(i)

  # dept
  print(paste("Dept id:",  filter_data$dept_id[i]))
  tr <- dt[dept_id == filter_data$dept_id[i],]
  
  # Prepare evaluation metric dataset
  prepare_custom_evaluation_metric_dependencies()
  
  # data parameters
  data_params <- list(
    max_bin = 255,                          # max number of bins that feature values will be bucketed in
    min_data_in_bin = 5,                    # minimal number of data inside one bin 
    bin_construct_sample_cnt = nrow(tr),    # number of data that sampled to construct histogram bins | aliases: subsample_for_bin
    data_random_seed = 33, 
    is_enable_sparse = TRUE,                # used to enable/disable sparse optimization | aliases: is_sparse, enable_sparse, sparse
    feature_pre_filter = F,
    weight_column = ""
  )
  
  # Convert all factors/chars to numeric
  lgb.prepare(tr)
  
  
  # Validation data
  flag <- tr$d >= 1914 - 28
  sum(is.na(tr[flag,])) # NAs in validation
  valid_data <- data.matrix(tr[flag, ..features])
  valid_data <- lgb.Dataset(data = valid_data, categorical_feature = categoricals, label = tr[["sales"]][flag], params = data_params, free_raw_data = T)
  
  
  
  # Training data (Be carefull here with na.omit)
  tr <- na.omit(tr)
  flag <- tr$d < 1914 - 28 
  y <- tr[["sales"]][flag]
  train_data <- data.matrix(tr[flag,..features,])
  train_data <- lgb.Dataset(data = train_data, categorical_feature = categoricals, label = y, params = data_params, free_raw_data = T)
  gc()
  
  set.seed(0)
  
  optObj <- bayesOpt(
    FUN = scoringFunction,
    bounds = bounds,
    saveFile = NULL,
    initPoints = 12,
    iters.n = 10,
    iters.k = 1,
    otherHalting = list(timeLimit = Inf, minUtility = 0),
    acq = "ucb",  # "ucb", "ei", "eips", "poi"
    kappa = 2.576,
    eps = 0,
    parallel = FALSE,
    gsPoints =  pmax(100, length(bounds)^3),
    convThresh = 1e+08,
    acqThresh = 1,
    errorHandling = "stop",
    plotProgress = T,
    verbose = 2
  )
  
  # View(optObj$scoreSummary)
  
  saveRDS(optObj, paste0("final_submission/hyperparameter_tuning_results/optObj_dept_", filter_data$dept_id[i], ".rds"))
  
  print("--------------------------------------------------------------------------")
  
}

# SOS: Do the only the first iteration end-to-end!



## 06. Lightgbm final training and save models ----

features
categoricals



### Make loop through the data, train the model with the best hyperparamters and save it.

for(i in c(1, 11, 21, 31, 41, 51, 61)){
  
  # i=11
  print(i)
  
  # dept
  print(paste("Dept id:",  filter_data$dept_id[i]))
  tr <- dt[dept_id == filter_data$dept_id[i],]
  
  
  # data parameters
  data_params <- list(
    max_bin = 255,                          # max number of bins that feature values will be bucketed in
    min_data_in_bin = 5,                    # minimal number of data inside one bin 
    bin_construct_sample_cnt = nrow(tr),    # number of data that sampled to construct histogram bins | aliases: subsample_for_bin
    data_random_seed = 33, 
    is_enable_sparse = TRUE,                # used to enable/disable sparse optimization | aliases: is_sparse, enable_sparse, sparse
    feature_pre_filter = F,
    weight_column = ""
  )
  
  # Convert all factors/chars to numeric
  lgb.prepare(tr)
  

  # Training data (Be carefull here with na.omit)
  tr <- na.omit(tr)
  flag <- tr$d
  y <- tr[["sales"]][flag]
  train_data <- data.matrix(tr[flag,..features,])
  train_data <- lgb.Dataset(data = train_data, categorical_feature = categoricals, label = y, params = data_params, free_raw_data = T)
  gc()
  
  
  ## Define training parameters
  
  params <- list(
    task = "train",                  
    objective = "tweedie",          
    boosting = "gbdt",               
    tree_learner = "serial",  
    seed = 33,
    nthread = 8,
    device_type = "cpu",
    force_col_wise = TRUE,
    force_row_wise = FALSE,
    bagging_seed = 33,
    feature_fraction_seed = 33,
    extra_trees = FALSE,
    extra_seed = 33, 
    boost_from_average = F
  )
  
  # Import best hyperparameters
  optObj <- readRDS(paste0("data/hyperparamater_tuning_results/optObj_dept_", filter_data$dept_id[i], ".rds"))
  
  params <- append(params, getBestPars(optObj))
  
  
  ## Lightgbm Training
  lgb_model <- lgb.train(params = params, data = train_data,
                         nrounds = optObj$scoreSummary %>% arrange(rmse) %>% pull(nrounds_rmse) %>% head(1),  
                         categorical_feature = categoricals,
                         verbose = -1, record = TRUE, init_model = NULL, colnames = NULL,
                         callbacks = list(), reset_data = FALSE)
  
  ## Sanity check (Importances)
  # importances <- lgb.importance(lgb_model)
  # plot(importances)
  # View(importances)
  
  saveRDS.lgb.Booster(object = lgb_model, paste0("models/lgb_",  filter_data$dept_id[i], ".rds"))
  
  print("--------------------------------------------------------------------------")
  
}



## // Predict // ----

## 01. Load and merge datasets ----

# Import train complete sales 
dt <- fread("data/raw/sales_train_validation.csv")

# Merge empty column for every forecasting day 
# (Use NA_real_ because the predictions will be real numbers)
dt[, paste0("d_", 1914:(1914+55)) := NA_real_]

# Transform wide format to long format
dt <- melt(dt,
           measure.vars = patterns("^d_"),
           variable.name = "d",
           value.name = "sales")

# calendar
calendar <- fread("data/raw/calendar.csv")
dt <- calendar[dt, on = "d"]
rm(calendar)


# price
prices_raw <- fread("data/raw/sell_prices.csv")
dt <- prices_raw[dt, on = c("store_id", "item_id", "wm_yr_wk")]


## 02. Data preprocessing | (calendar-prices Feature engineering) ----

# Drop columns
# dt[, `:=`(wm_yr_wk = NULL, date = NULL, weekday = NULL)]

# Fix Data types
str(dt)

dt[, d := as.integer(substring(d, first = 3))]


## 03. Add more features ----

# Features from stat_total file
stat_total <- readRDS("data/processed/stat_total.rds")
stat_total <- as.data.table(stat_total)
stores <- as.character(unique(stat_total$store_id))
departments <- as.character(unique(stat_total$dept_id))
stat_total <- stat_total[,c("item_id", "store_id", "lngth", "ADI", "CV2", "pz", "Max")]

dt <- stat_total[dt, on = c("item_id", "store_id")]


## 04. sales Feature engineering ----

# Function to be used recursively
sales_fe <- function(dt){
  
  # Lags
  dt[order(d), `:=`(lag_t28 = dplyr::lag(sales, 28),
                    lag_t29 = dplyr::lag(sales, 29),
                    lag_t35 = dplyr::lag(sales, 35),
                    lag_t42 = dplyr::lag(sales, 42),
                    lag_t49 = dplyr::lag(sales, 49)),
     by = "id"]
  
  
  # Rolling features
  dt[order(d), `:=`(mean_previous_month = (lag_t28 + lag_t35 + lag_t42 + lag_t49)/4,
                    rolling_mean_lag28_t7 = roll_meanr(lag_t28, 7),
                    rolling_mean_lag28_t30 = roll_meanr(lag_t28, 30),
                    rolling_mean_lag28_t60 = roll_meanr(lag_t28, 40)),
     by = "id"]
}



## 05. Predict -----

features <- c("lngth", "ADI", "CV2", "pz", "Max",
              "sell_price",
              "wday", "month", "year",
              "snap_CA", "snap_TX", "snap_WI",                
              "lag_t28", "lag_t29", "lag_t35", "lag_t42", "lag_t49", "mean_previous_month",   
              "rolling_mean_lag28_t7", "rolling_mean_lag28_t30", "rolling_mean_lag28_t60",
              "sales",
              "store_id", "state_id")

categoricals <- c("wday", "month", "year","store_id", "state_id")

setdiff(colnames(dt), features)


# Dept
filter_data <- expand.grid(list(departments), stringsAsFactors = F)
colnames(filter_data) <- c("dept_id")
head(filter_data)


## Keep a subset of the test data with the neccessary data to compute lags for the specific forecasting day
dt <- dt[!is.na(sell_price), ]

dt <- dt[d >= 1800,]

fh = 28

for (day in seq(from = 1913 + 1, to = 1913 + 2*fh, by = 1) ){ # 2*fh
  
  ## Test
  # day = 1914
  cat(day, "\n")
  
  
  ## 1. Keep a subset of the test data with the neccessary data to compute lags for the specific forecasting day
  
  # Get a subset from dt (test dataset), with the last max_lags days
  # because these are the days required to create the lagging sales features.
  
  tst <- dt[d <= day]
  
  
  ## 2. Create Sales features - lags (232 features)
  tst <- sales_fe(tst)
  gc()
  
  
  ## 3. Sanity check of feature vectors for the specific test day - NA-imputations
  # tst[d==day, ] %>% View()
  # anyNA(tst[d==day,..features])
  # summary(tst[d==day,])
  
  # 4. Make forecastings for the specific day, using different model for each department
  
  for (i in 1:nrow(filter_data)){
    
    # Test
    # i=1

    dept <- filter_data[i,"dept_id"]
    print(dept)
    
    
    # Load model
    lgb_model <- readRDS.lgb.Booster(paste0("models/lgb_", dept, ".rds"))
    
    tst_day <- tst[d == day & dept_id == dept, ..features]
    
    
    # Convert all factors/chars to numeric
    lgb.prepare(tst_day)
    tst_day <- data.matrix(tst_day)
    
    
    dt[d == day & dept_id == dept, sales := predict(lgb_model, tst_day)] 

    gc()
  }
  
  gc()
}