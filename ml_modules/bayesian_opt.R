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
setdiff(colnames(tr), features)  # Features to not include in model

# Which of these features should be treaten as categorical
categoricals
setdiff(categoricals, colnames(tr))  # Errors

# setdiff(categoricals, which(sapply(tr, function(x){class(x)=="integer"})) %>% names())


# Let's asuume that there is an available dataset

## 04. Define the scoring function for the Bayesian optimization  -----
 
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
                         valids = list(valid = valid_data), eval_freq = 50, early_stopping_rounds = 50, # Validation parameters
                         eval = custom_wrmsse_metric, # SOS:  wrmsse_den, weights, fh
                         metric = "rmse", 
                         nrounds = 100,  
                         categorical_feature = categoricals,
                         verbose = 1, record = TRUE, init_model = NULL, colnames = NULL,
                         callbacks = list(), reset_data = FALSE)
  
  
  ## rmse results
  # min(unlist(lgb_model$record_evals$valid$rmse$eval))
  # which.min(unlist(lgb_model$record_evals$valid$rmse$eval))

  ## wrmse results
  # min(unlist(lgb_model$record_evals$valid$wrmsse$eval))
  # which.min(unlist(lgb_model$record_evals$valid$wrmsse$eval))
  
  # NOTE:
  # ParBayesianOptimization will maximize the function you provide it, to change your problem to a maximization one, just multiply the RMSE (Score) being output by -1.
  
  # Get results (Score = wrmse and rmse)
  ls <- list(Score = - min(unlist(lgb_model$record_evals$valid$wrmsse$eval)),
             nrounds_wrmsse = which.min(unlist(lgb_model$record_evals$valid$wrmsse$eval)),
             rmse = min(unlist(lgb_model$record_evals$valid$rmse$eval)),
             nrounds_rmse = which.min(unlist(lgb_model$record_evals$valid$rmse$eval)))
  
  return(ls)
}



##  05. Define Bounds for parameters ----

# DOCUMENTATION: https://lightgbm.readthedocs.io/en/latest/Parameters.html

bounds <- list(
  # Core parameters
  learning_rate = c(0.01, 0.6),
  num_leaves = c(1L, 100000L),  
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


## 07. Run Bayesian optimization process

# We are now ready to put this all into the bayesOpt function.
set.seed(0)

optObj <- ParBayesianOptimization::BayesianOptimization(
  FUN = scoringFunction,
  bounds = bounds,
  initialize = TRUE,
  initPoints = 6,
  nIters = 24,
  kern = "Matern52",                # "Gaussian", "Exponential","Matern52", "Matern32"
  beta = 0,
  acq = "ucb",
  stopImpatient = list(newAcq = "ucb", rounds = Inf),
  kappa = 2.576,
  eps = 0,
  gsPoints = 100,
  convThresh = 1e+07,
  minClusterUtility = NULL,
  noiseAdd = 0.25,
  parallel = FALSE,
  verbose = 1
)

saveRDS(optObj, "optObj.rds")

# Get results
optObj
View(optObj)
optObj$scoreSummary
plot(optObj)
getBestPars(optObj)


# Add more itarations (I have to update the package in order to use this function)
optObj <- addIterations(
  optObj,
  iters.n = 20,
  verbose=1)


optObjSimp
optObj$scoreSummary
plot(optObjSimp)
getBestPars(optObj)









