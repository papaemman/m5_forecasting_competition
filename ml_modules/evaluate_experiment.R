##################################
#                                #
#  Create submission file        #
#                                #
##################################

# 1. Make forecasings (submission file)

#---------------------------
cat("Forecasting...\n")

View(test %>% filter())

test_one_item <- test %>% filter(item_id == 1, store_id ==1)

View(test_one_item)
test_one_item
colnames(test_one_item)

dim(test_one_item)
class(test_one_item)
test_one_item <- data.table(test_one_item)



# Make predicitons keeping NAs in many features
for (i in 1:56){
  
  i = 1
  cat(i, "\r")

  # Make predition
  test_one_item[i, "demand"] <- predict(lgb_model, data.matrix(test_one_item[i,features]))
  
  # Lag prediction
  test_one_item[i+1,"lag_t1"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t2"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t3"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t4"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t5"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t6"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t7"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t8"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t14"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t21"] <- test_one_item[i,"demand"]
  test_one_item[i+1,"lag_t28"] <- test_one_item[i,"demand"]
  
  if(i>=8){
    
    test_one_item[i,"mean_last_month"] <- (test_one_item[i,"lag_t7"] + lag_t14 + lag_t21 + lag_t28)/4 ,
    
    test_one_item[i, "rolling_mean_lag7_t7"] <- roll_meanr(test_one_item$lag_t7[1:i], 7),
    
  }
  
  
  }


pred <- predict(lgb_model, data.matrix(test[, features, with = FALSE]))
test[, demand := pmax(0, pred)]

test_long <- dcast(test, id ~ F, value.var = "demand")


# Sort based on sample_submissionion id column

sample_submission <- fread("data/pnt_submissions/sample_submission.csv")
submission <- merge(sample_submission[, .(id)], 
                    test_long[, colnames(sample_submission), with = FALSE], 
                    by = "id")


fwrite(submission, file = "data/pnt_submissions/experiment_lgb.csv", row.names = FALSE)


