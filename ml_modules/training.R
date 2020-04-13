##################################
#                                #
#  Modeling / Training           #
#                                #
##################################

library(lightgbm)
library(tictoc)


## 05. Machine Learning training -----

# It's time for LightGBM fun. Parameters are lent from Very fst Model notebook.

# Parameter choice
params = list(objective = "poisson",
              metric = "rmse",
              seed = 20,
              learning_rate = 0.075,
              lambda = 0.1,
              bagging_fraction = 0.66,
              bagging_freq = 1, 
              colsample_bytree = 0.77,
              num_leaves = 128,
              min_data_in_leaf = 20,
              force_row_wise = TRUE,
              nthread = 7)

p <- list(objective = "poisson",
          metric ="rmse",
          seed = 33,
          force_row_wise = TRUE,
          learning_rate = 0.075,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 7)


# Training
tic()
m_lgb <- lgb.train(params = p,
                   data = train,
                   nrounds = 4000, # More rounds needed
                   valids = list(valid = valid),
                   early_stopping_rounds = 400,
                   eval_freq = 200)

saveRDS(m_lgb, file = "model_lgbm_v2.rds")
cat("Best score:", m_lgb$best_score, "at", m_lgb$best_iter, "iteration")                         
lgb.plot.importance(lgb.importance(m_lgb), 20)


toc()

# Save model
saveRDS(object = fit, file = "data/pnt_model/lightgbm_v0.rds")

# Importance
imp <- lgb.importance(fit)
lgb.plot.importance(imp, top_n = Inf)




