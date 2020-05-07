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
dt <- create_dt(is_train = TRUE, nrows = nrows,
                fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday,
                filter_params = "dept_id == 'HOBBIES_1'")
gc()


## Create hierarhy sales features
dt <- create_hierarhy_sales_features(dt)
gc()


## Preprocessing steps and add features
dt <- add_features(dt = dt)
gc()


## Create Sales features
dt <- create_sales_features(dt)
gc()


## Save train dataset and remove other datasets
dim(dt) # [1] 46027957      163
dt[,.N, by = c("store_id", "item_id")] %>% View()

format(object.size(dt), unit = "auto") #  "23 Gb"
saveRDS(object = dt, file = "../kitematic/temp/dt_h.rds")


## Drop zero-demand periods
nrow(unique(dt[, c("item_id")]))
dt <- drop_zero_demand_periods(dt, nb_days = 60)
gc()


## Drop NAs from first days (Use the column with maximum lags)
dt <- dt[!is.na(rolling_mean_lag28_t180), ]  # faster than na.omit()
gc()
anyNA(dt)



imp <- read.csv("imp_data_leak.csv")
View(imp)
## Select Features to be use in training

# tr = dt
# rm(dt)

library(lightgbm)
library(data.table)
library(tictoc)

# tr <- readRDS("../kitematic/temp/train.rds")
tr <- readRDS("../kitematic/temp/dt_h.rds")


tables()
class(tr)
gc()

tr[1:3, 1:10]
dim(tr)
str(tr)


features <- c(
  
  # "id",
  
  # "item_id",  "dept_id", "cat_id", "store_id", "state_id", 
  
  # "d", "sales",
  
  "wday", "month", "year",
  "event_name_1", "event_type_1", 
  
  # "event_name_2", "event_type_2",
  
  "snap_CA", "snap_TX", "snap_WI",   
  
  "day_month", "day_quarter", "day_year",
  "week_month", "week_quarter", "week_year", "weekend",
  "month_quarter", "quart", "semester",
  
  "wday_s1", "wday_c1", "wday_s2" , "wday_c2", "wday_s3", "wday_c3",
  "month_s1", "month_c1", "month_s2", "month_c2", "month_s3","month_c3",
  
  "wday_rbf_1", "wday_rbf_2", "wday_rbf_3", "wday_rbf_4", "wday_rbf_5", "wday_rbf_6", "wday_rbf_7",
  
  "month_rbf_1", "month_rbf_2", "month_rbf_3", "month_rbf_4", "month_rbf_5", "month_rbf_6", "month_rbf_7" ,"month_rbf_8", "month_rbf_9",
  "month_rbf_10", "month_rbf_11", "month_rbf_12",
  
  "sporting_event", "cultural_event", "national_event", "religious_event",
  
  "has_event", "has_event_lead_t1", "has_event_lead_t2", "has_event_lead_t3", "week_events",
  
  "sporting_event_lead_1" , "cultural_event_lead_1" , "national_event_lead_1", "religious_event_lead_1", 
  "sporting_event_lead_3",  "cultural_event_lead_3" , "national_event_lead_3",   "religious_event_lead_3",
  
  
  "sell_price", "sell_price_diff", "sell_price_rel_diff", "sell_price_cumrel", "sell_price_roll_sd7",
  
  # "nb_stores", 
  
  # "unique_sell_prices", "sell_price_changes", "mean_sell_price", "min_sell_price", "max_sell_price", "max_diff",
  
  "sell_price_to_mean", "sell_price_to_min", "sell_price_to_max", "sell_price_diff_to_max_diff",
  
  "diff_from_mean_price", "rel_diff_from_mean_price", "best_price",
  
  "sell_price_to_min_lead_1", "sell_price_to_min_lead_2", "sell_price_to_max_lead_1", "sell_price_to_max_lead_2",
  
  
  # "total_sales", "total_state_sales", "total_store_sales", "total_cat_sales", "total_dept_sales",           
  # "total_state_cat_sales", "total_state_dept_sales", "total_store_cat_sales", "total_store_dept_sales",   
  # "total_item_sales", "total_item_state_sales",
  
  "snap",
  
  "lngth", "ADI", "CV2", "pz", "Min", "Low25", "Mean", "Median", "Up25", "Max", "dollar_sales",
  # "Type",
  "type_1", "type_2", "type_3", "type_4",
  
  
  "lag_t1", "lag_t2", "lag_t3", "lag_t4", "lag_t5", "lag_t6", "lag_t7", "lag_t8",
  "lag_t14", "lag_t21", "lag_t28", "lag_t35", "lag_t42", "lag_t49",
  
  "mean_last_month", "mean_last_2_monts", "mean_previous_month",
  
  "rolling_mean_lag1_t7", "rolling_mean_lag1_t120",
  
  "rolling_mean_lag7_t7", "rolling_mean_lag7_t30", "rolling_mean_lag7_t60" ,
  "rolling_mean_lag7_t90", "rolling_mean_lag7_t120", "rolling_mean_lag7_t180", 
  
  "rolling_sd_lag7_t7", "rolling_sd_lag7_t30", 
  "rolling_sum_lag7_t7","rolling_min_lag7_t7",
  
  "rolling_min_lag7_t30", "rolling_max_lag7_t7",
  
  "rolling_max_lag7_t30", "rolling_mean_lag28_t7",
  
  "rolling_mean_lag28_t30", "rolling_mean_lag28_t60", "rolling_mean_lag28_t90", "rolling_mean_lag28_t120", "rolling_mean_lag28_t180" ,   
  "rolling_sd_lag28_t7", "rolling_sd_lag28_t30", 
  "rolling_sum_lag28_t7", 
  
  "rolling_min_lag28_t7", "rolling_min_lag28_t30",
  "rolling_max_lag28_t7", "rolling_max_lag28_t30",
  
  "lag_t7_total_sales", "lag_t7_total_state_sales", "lag_t7_total_store_sales",                  
  "lag_t7_total_cat_sales", "lag_t7_total_dept_sales", "lag_t7_total_state_cat_sales",               
  "lag_t7_total_state_dept_sales", "lag_t7_total_store_cat_sales", "lag_t7_total_store_dept_sales",              
  "lag_t7_total_item_sales", "lag_t7_total_item_state_sales",
  
  "lag_t28_total_sales", "lag_t28_total_state_sales", "lag_t28_total_store_sales", "lag_t28_total_cat_sales",                    
  "lag_t28_total_dept_sales", "lag_t28_total_state_cat_sales", "lag_t28_total_state_dept_sales",             
  "lag_t28_total_store_cat_sales", "lag_t28_total_store_dept_sales", "lag_t28_total_item_sales" ,                  
  "lag_t28_total_item_state_sales",
  
  "meanl_last_total_sales", "mean_last_total_state_sales", "mean_last_total_store_sales",
  "mean_last_total_cat_sales", "mean_last_total_dept_sales","mean_last_total_state_cat_sales",
  "mean_last_total_state_dept_sales","mean_last_total_store_cat_sales",            
  "mean_last_total_store_dept_sales", "mean_last_total_item_sales", "mean_last_total_item_state_sales",           
  
  "rolling_mean_lag_t28_total_sales", "rolling_mean_lag_t28_total_state_sales", "rolling_mean_lag_t28_total_store_sales",     
  "rolling_mean_lag_t28_total_cat_sales", "rolling_mean_lag_t28_total_dept_sales", "rolling_mean_lag_t28_total_state_cat_sales", 
  "rolling_mean_lag_t28_total_state_dept_sales" ,"rolling_mean_lag_t28_total_store_cat_sales", "rolling_mean_lag_t28_total_store_dept_sales",
  "rolling_mean_lag_t28_total_item_sales", "rolling_mean_lag_t28_total_item_state_sales"
  
  # "orig", "rleLength" 
  
)



setdiff(features, colnames(tr))
setdiff(colnames(tr), features)
# Don't include: [1] "id"        "d"         "sales"     "date"      "rleLength"


# Which of these features should be treaten as categorical

categoricals <- c(
  
  "wday", "month", "year",
  "event_name_1", "event_type_1", 

  "snap_CA", "snap_TX", "snap_WI",   
  
  "day_month", "day_quarter", "day_year",
  "week_month", "week_quarter", "week_year", "weekend",
  "month_quarter", "quart", "semester",
  
  "sporting_event", "cultural_event", "national_event", "religious_event",
  
  "has_event", "has_event_lead_t1", "has_event_lead_t2", "has_event_lead_t3", "week_events",
  
  "sporting_event_lead_1" , "cultural_event_lead_1" , "national_event_lead_1", "religious_event_lead_1", 
  "sporting_event_lead_3",  "cultural_event_lead_3" , "national_event_lead_3",   "religious_event_lead_3",
  
  "best_price",
  
  "snap",
  
  # "Type", 
  "type_1", "type_2", "type_3", "type_4",
  
  "Low25", "Mean", "Median", "Up25", "Max"
  
)

# setdiff(categoricals, which(sapply(tr, function(x){class(x)=="integer"})) %>% names())


## Build multiple different models
stat_total <- read.csv("data/processed/stat_total.csv", stringsAsFactors = F)
stores <- unique(stat_total$store_id)
departments <- unique(stat_total$dept_id)
df <- expand.grid(stores,departments, stringsAsFactors = F)
colnames(df) <- c("store_id", "dept_id")
head(df)

tr_full = tr

# Define the order in tr dataset (alphabetical order)
stores <- c("CA_1", "CA_2", "CA_3", "CA_4", "TX_1", "TX_2", "TX_3", "WI_1", "WI_2", "WI_3")
departments <- c("FOODS_1", "FOODS_2", "FOODS_3", "HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2")


for (i in 1:nrow(df)){
  
  # Test
  # i=43
  
  store <- df[i,"store_id"]
  dept <- df[i,"dept_id"]
  print(paste("Store:", store, "| Dept:", dept))
  
  
  ## Subset training data
  tr <- tr_full[store_id == which(store == stores) & dept_id == which(dept == departments),] # & !is.na(rolling_mean_lag28_t180)
  
  ## NAs
  dim(tr)     # 651783    163
  anyNA(tr)
  sapply(tr, function(x){sum(is.na(x))})             # Check NAs in every column
  nrow(unique(tr[,j = c("item_id", "store_id")]))    # Total items (416)
  
  tr <- tr[!is.na(rolling_mean_lag28_t180),]
  dim(tr)     # 565671    163
  nrow(unique(tr[,j = c("item_id")]))    # Total items (416)
  gc()
  
  
  
  ## Data construction parameters (binning)
  data_params <- list(
    max_bin = 255,                         # max number of bins that feature values will be bucketed in
    min_data_in_bin = 3,                   # minimal number of data inside one bin 
    bin_construct_sample_cnt = 5000000,    # number of data that sampled to construct histogram bins | aliases: subsample_for_bin
    data_random_seed = 33, 
    is_enable_sparse = TRUE,               # used to enable/disable sparse optimization | aliases: is_sparse, enable_sparse, sparse
    feature_pre_filter = F,
    weight_column = ""
  )

  
  ## // Train without validation data // 
  
  # y <- tr$sales
  # tr <- data.matrix(tr[,..features,])
  # tr <- lgb.Dataset(tr, categorical_feature = categoricals, label = y,  params = data_params)
  # gc()
  
  # tic()
  # lgb_model <- lgb.train(params = params, data = tr)
  # toc()
  
  
  ## // Keep 1 month (last month) of validation data //
  
  # Validation data
  flag <- tr$d >= 1914 - 28
  valid_data <- data.matrix(tr[flag, ..features])
  valid <- lgb.Dataset(data = valid_data, categorical_feature = categoricals, label = tr[["sales"]][flag], params = data_params)
  
  # Training data
  flag <- tr$d < 1914 - 28
  y <- tr[["sales"]][flag]
  tr <- data.matrix(tr[flag,..features,])
  tr <- lgb.Dataset(data = tr, categorical_feature = categoricals, label = y, params = data_params)

  
  ## Define parameters for training
  # DOCUMENTATION: https://lightgbm.readthedocs.io/en/latest/Parameters.html
  # R API Documentation: https://lightgbm.readthedocs.io/en/latest/R/index.html
  # https://github.com/Microsoft/LightGBM/issues/127

  
  params <- list(

    # 1. Core parameters ----
    task = "train",
    
    objective = "tweedie",         # "regression", "regression_l1", "poisson", "huber", "tweedie"
    
    boosting = "gbdt",               # Boosting type: "gbdt", "rf", "dart", "goss"
    
    # num_iterations = 3000,         # number of training rounds            | aliases: num_iteration, n_iter, num_tree, num_trees, num_round, num_rounds, num_boost_round, n_estimators
    learning_rate = 0.03,           # shrinkage rate                       | aliases: shrinkage_rate, eta
    num_leaves = 2**11 - 1,               # max number of leaves in one tree     | aliases: num_leaf, max_leaves, max_leaf
    tree_learner = "serial",       # "serial", "feature", "data", "voting"
    
    seed = 33,
    nthread = 8,
    device_type = "cpu",
    
    # 2. Learning Control Parameters  ----
    
    force_col_wise = TRUE,
    force_row_wise = FALSE,
    
    max_depth = 50,
    min_data_in_leaf = 20,            # minimal number of data in one leaf   | aliases: min_data_per_leaf, min_data, min_child_samples
    
    bagging_fraction = 0.6,           # randomly select part of data without resampling  | aliases: sub_row, subsample, bagging
    bagging_freq = 1,                 # frequency for bagging                            | subsample_freq 
    bagging_seed = 33,
    
    feature_fraction = 0.6,             # for boosting "rf" |  aliases: sub_feature, colsample_bytree
    feature_fraction_bynode = 0.5,    # 
    feature_fraction_seed = 33,
    
    lambda_l1 = 0.1,                  #  | aliases: reg_alpha
    lambda_l2 = 0.1,                  #  | aliases: reg_lambda, lambda
  
    # min_gain_to_split = 0.01,
    
    extra_trees = FALSE,
    extra_seed = 33, 
    
    
    # 3. I/O parameters ----
    
    # Dataset parameters
    tweedie_variance_power= 1.1,
    metric = "rmse",

    # 4. Objective parameters ----
    boost_from_average = F
    
    # 5. Metric Parameters ----

  )
  
  ## Train
  tic()
  lgb_model <- lgb.train(params = params, data = tr,
                         valids = list(valid = valid), eval_freq = 20, early_stopping_rounds = 600, nrounds = 1000,  # Validation parameters
                         categorical_feature = categoricals,
                         verbose = 1, record = TRUE, init_model = NULL, colnames = NULL,
                         callbacks = list(), reset_data = FALSE)
  
  toc()
  
  cat("Best score:", lgb_model$best_score, "at", lgb_model$best_iter, "iteration") 
  
  
  ## Check importances (Why so slow?)
  # imp <- lgb.importance(lgb_model)
  # View(imp)
  # lgb.plot.importance(imp, 20, cex = 0.9) 
  
  
  ## // HOLD - OUT |  CROSS - VALIDATION //
  
  
  grid_search <- expand.grid(
                             # Parameters for best fit
                             num_leaves = 100,
                             min_data_in_leaf = c(1,2,4),
                             max_depth = c(10,20,40,80),
                             
                             # Parameters for faster speed
                             bagging_fraction = c(0.4,0.6),
                             bagging_freq = c(2,4),
                             feature_fraction = c(0.8,0.9,0.95),
                             
                             min_sum_hessian_in_leaf = c(0.05,0.1,0.2),
 
                             lambda_l1 = c(0.2,0.4),
                             lambda_l2 = c(0.2,0.4),
                             min_gain_to_split = c(0.2,0.4))
  
  perf <- numeric(nrow(grid_search))
  
  
  for (i in 1:nrow(grid_search)) {
    
    params <- list(
                   #  Constant parameters
                   objective = "regression",
                   metric = "l2",
                   min_data = 1,
                   learning_rate = 0.1,
                   
                   # Grid search parameters
                   max_depth = grid_search[i, "max_depth"],
                   min_data_in_leaf = grid_search[i,"min_data_in_leaf"],
                   min_sum_hessian_in_leaf = grid_search[i, "min_sum_hessian_in_leaf"],
                   feature_fraction =  grid_search[i, "feature_fraction"],
                   bagging_fraction =  grid_search[i, "bagging_fraction"],
                   bagging_freq =  grid_search[i, "bagging_freq"],
                   lambda_l1 =  grid_search[i, "lambda_l1"],
                   lambda_l2 =  grid_search[i, "lambda_l2"],
                   min_gain_to_split =  grid_search[i, "min_gain_to_split"])
    
    
    lgb_model <- lgb.train(params = params, data = tr,
                          valids = list(valid = valid), eval_freq = 10, early_stopping_rounds = 600,  # Validation parameters
                          categorical_feature = categoricals,
                          verbose = 1, record = TRUE, init_model = NULL, colnames = NULL,
                          callbacks = list(), reset_data = FALSE)
    
    perf[i] <- min(rbindlist(lgb_model$record_evals$test$l2))
    gc(verbose = FALSE)
    
  }
  
  # Grid search results
  cat("Model ", which.min(perf), " is lowest loss: ", min(perf), sep = "","\n")
  print(grid_search[which.min(perf), ])

  
  

  
  ## Save best model
  saveRDS.lgb.Booster(lgb_model, paste0("models/", store, "_", dept, "_lgb_model.rds"))
  gc()
  
  ## Check importances
  # imp <- lgb.importance(lgb_model)
  # View(imp)
  # lgb.plot.importance(imp, 20, cex = 0.9) 
  
  
  
 
  
  
}






