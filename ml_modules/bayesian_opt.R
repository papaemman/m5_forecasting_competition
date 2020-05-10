##################################
#                                #
#   Bayesian optimization        #
#                                #
##################################

## 00. Load packages ----

library(tidyverse)
library(data.table)
library(RcppRoll)
library(lightgbm)
library(tictoc)

library(ParBayesianOptimization)

fh = 28


## 01. Load the dataset ----

# tr = dt
# rm(dt)

# tr <- readRDS("../kitematic/temp/dt_full_train.rds")
tr <- readRDS("../kitematic/temp/dt_full_train_without_NAs.rds")   # [1] 39720284      211


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
setdiff(colnames(tr), features)  # Features to not include in model (ids, d, sales, total_sales)

# Which of these features should be treaten as categorical
categoricals
setdiff(categoricals, colnames(tr))  # Errors

# setdiff(categoricals, which(sapply(tr, function(x){class(x)=="integer"})) %>% names())


## 04. Build multiple different models ----

stat_total <- read.csv("data/processed/stat_total.csv", stringsAsFactors = F)
stores <- unique(stat_total$store_id)
departments <- unique(stat_total$dept_id)

# Define the rights order in tr dataset (alphabetical order)
stores <- c("CA_1", "CA_2", "CA_3", "CA_4", "TX_1", "TX_2", "TX_3", "WI_1", "WI_2", "WI_3")
departments <- c("FOODS_1", "FOODS_2", "FOODS_3", "HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2")


# - A. One model per STORE (10 models) -
# df <- expand.grid(stores, stringsAsFactors = F)
# colnames(df) <- c("store_id")
# df

# - B. One model per DEPARTMENT (7 models) -
df <- expand.grid(departments, stringsAsFactors = F)
colnames(df) <- c("dept_id")
df

# - C. One model per STORE - DEPARTMENT (70 models) -
# df <- expand.grid(stores,departments, stringsAsFactors = F)
# colnames(df) <- c("store_id", "dept_id")
# dim(df)
# head(df)


# Save full training data
tr_full = tr


## 05. Select one dataset and create lightgbm dataset ----

i=1

# Print data subsets
store <- df[i,"store_id"]
dept <- df[i,"dept_id"]
print(paste("Store:", store, "| Dept:", dept))


## - 1. One model for all data -
# tr <- tr_full

## - 1A. One model per STORE (10 models) -
# tr <- tr_full[store_id == which(store == stores),]

## - 1B. One model per DEPARTMENT (7 models) -
# tr <- tr_full[dept_id == which(dept == departments),]
tr <- tr[dept_id == which(dept == departments),]

## - 1C. One model per STORE - DEPARTMENT (70 models) -
# tr <- tr_full[store_id == which(store == stores) & dept_id == which(dept == departments),]


## Deal with NAs
tr
dim(tr)
anyNA(tr)
# sapply(tr, function(x){sum(is.na(x))})           # Check NAs in every column
nrow(unique(tr[,j = c("item_id", "store_id")]))    # Total items (416)
# tr <- tr[!is.na(rolling_mean_lag28_t180),]



## Data construction parameters (binning)

data_params <- list(
  max_bin = 255,                          # max number of bins that feature values will be bucketed in
  min_data_in_bin = 3,                    # minimal number of data inside one bin 
  bin_construct_sample_cnt = nrow(tr),    # number of data that sampled to construct histogram bins | aliases: subsample_for_bin
  data_random_seed = 33, 
  is_enable_sparse = TRUE,                # used to enable/disable sparse optimization | aliases: is_sparse, enable_sparse, sparse
  feature_pre_filter = F,
  weight_column = ""
)


## // Keep 1 month (last month) of validation data //

# Validation data
flag <- tr$d >= 1914 - 28
valid_data <- data.matrix(tr[flag, ..features])
valid_data <- lgb.Dataset(data = valid_data, categorical_feature = categoricals, label = tr[["sales"]][flag], params = data_params, free_raw_data = F)

# Training data
flag <- tr$d < 1914 - 28
y <- tr[["sales"]][flag]
train_data <- data.matrix(tr[flag,..features,])
train_data <- lgb.Dataset(data = train_data, categorical_feature = categoricals, label = y, params = data_params, free_raw_data = F)


## Load datasets for WRMSSE caluclations

# Load denominator for wrmsSe
wrmsse_den <- read.csv("data/wrmsse_den_without_last_28.csv")

# Load weights for Wrmsse
weights <- read.csv("data/bts_weights.csv")

# The validation data are:
flag <- tr$d >= 1914 - 28
item_group_frc <- tr[flag, c("store_id", "item_id")]
item_group_frc
item_group_frc %>% unique() %>% nrow()
dim(item_group_frc)

# Select the approprate store_ids - item_ids
wrmsse_den <- merge(item_group_frc, wrmsse_den , by = c("store_id", "item_id"), all.x = T, sort = F)
wrmsse_den[,c("store_id", "item_id")] %>% unique() %>% nrow()
dim(wrmsse_den)
weights <- merge(item_group_frc, weights , by = c("store_id", "item_id"), all.x = T, sort = F)
weights[, c("store_id", "item_id")] %>% unique() %>% nrow()
dim(weights)


## 06. Define the scoring function for the Bayesian optimization  -----
 
# Don't forget to define custom_wrmsse_metric() function

scoringFunction <- function(learning_rate, num_leaves,                                                   # Core parameters
                            max_depth, min_data_in_leaf,                                                 # Learning Control Parameters
                            bagging_fraction, bagging_freq, feature_fraction, feature_fraction_bynode,
                            lambda_l1, lambda_l2,
                            tweedie_variance_power) {                                                    # Objective parameters 
  

  
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
  
  
  ## Lightgbm Trainining

  lgb_model <- lgb.train(params = params, data = train_data,
                         valids = list(valid = valid_data), eval_freq = 30, early_stopping_rounds = 300, # Validation parameters
                         eval = custom_wrmsse_metric,                           # SOS:  wrmsse_den, weights, fh datafiles needed for wrmsse calculations
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



##  07. Define Bounds for parameters ----

# DOCUMENTATION: https://lightgbm.readthedocs.io/en/latest/Parameters.html

bounds <- list(
  # Core parameters
  learning_rate = c(0.01, 0.6),
  num_leaves = c(2L, 100000L),  
  # Learning Control Parameters
  max_depth = c(1L, 10000L),
  min_data_in_leaf = c(5L, 100L),                                                 
  bagging_fraction = c(0.1, 1),
  bagging_freq = c(0L, 100L),
  feature_fraction = c(0.1, 1),
  feature_fraction_bynode = c(0.1, 1),
  lambda_l1 = c(0, 2),
  lambda_l2 = c(0, 2),
  # Objective parameters 
  tweedie_variance_power = c(1, 1.99)
)


## 08. Run Bayesian optimization process ----

# To save memory
rm(tr_full)

# We are now ready to put this all into the bayesOpt function.
set.seed(0)

optObj <- bayesOpt(
  FUN = scoringFunction,
  bounds = bounds,
  saveFile = NULL,
  # initGrid,
  initPoints = 12,
  iters.n = 8,
  iters.k = 1,
  otherHalting = list(timeLimit = Inf, minUtility = 0),
  acq = "ei",  # "ucb", "ei", "eips", "poi"
  kappa = 2.576,
  eps = 0,
  parallel = FALSE,
  gsPoints = pmax(100, length(bounds)^3),
  convThresh = 1e+08,
  acqThresh = 1,
  errorHandling = "stop",
  plotProgress = T,
  verbose = 2
)

saveRDS(optObj, "data/optObj.rds")


# // TIMING INFO //
# 1. initPOitns * time_for_lightgbm_training : initial grid calculations
# 2. iters.n *  ~16 mins                     : "Running local optimum search..." for every iteration
# 3. iters.n * time_for_lightgbm_training    : bayesian opt steps


# Get results
optObj
View(optObj)
optObj$scoreSummary
plot(optObj)
getBestPars(optObj)


# Add more itarations
optObj <- addIterations(
  optObj,
  iters.n = 20,
  verbose=1)










