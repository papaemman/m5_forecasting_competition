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

tr <- readRDS("../kitematic/temp/dt_full_train.rds")  # 46.027.957      232

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
  # i=7
  
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
  # anyNA(tr)
  # sapply(tr, function(x){sum(is.na(x))})             # Check NAs in every column
  # nrow(unique(tr[,j = c("item_id", "store_id")]))    # Total items (416)
  # tr <- tr[!is.na(rolling_mean_lag28_t180),]
  # dim(tr)                                            # 565671    163
  # nrow(unique(tr[,j = c("item_id")]))                # Total items (416)
  # gc()
  
  
  
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
  
  # Drop NA values
  nrow_full = nrow(tr)
  # anyNA(tr)
  
  tr <- tr[!is.na(rolling_mean_lag28_t180),]
  print(paste("Row droped", nrow_full - nrow(tr)))
  gc()
  # anyNA(tr)
  
  y <- tr$sales
  tr <- data.matrix(tr[,..features,])
  gc()
  tr <- lgb.Dataset(tr, categorical_feature = categoricals, label = y,  params = data_params, free_raw_data = TRUE)
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
  tic()
  lgb_model <- lgb.train(params = params, data = tr,
                         nrounds = 500,
                         categorical_feature = categoricals,
                         verbose = 1, record = TRUE, init_model = NULL, colnames = NULL,
                         callbacks = list(), reset_data = FALSE)

  toc()
  
  saveRDS.lgb.Booster(lgb_model, paste0("models/", dept, "_lgb_model.rds"))
  # saveRDS.lgb.Booster(lgb_model, paste0("models/", store, "_", dept, "_lgb_model.rds"))
  
  gc()
  rm(tr)
  gc()
}








