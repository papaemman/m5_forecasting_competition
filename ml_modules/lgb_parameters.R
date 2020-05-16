###########################
#                         #
#   Lightgbm parameters   #
#                         #
###########################

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