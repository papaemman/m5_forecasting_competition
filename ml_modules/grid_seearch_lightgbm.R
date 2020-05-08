#############################
#                           #
#   grid search lightgbm    #    
#                           #
#############################



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

