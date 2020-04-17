##################################
#                                #
#  Create submission file        #
#                                #
##################################


head(train)
lgb <- readRDS(file = "model_lgbm_v2.rds")

## 06. Create Submission file -----
te <- create_dt(FALSE)

test

for (day in as.list(seq(fday, length.out = 2*h, by = "day"))){
  cat(as.character(day), " ")
  tst <- te[date >= day - max_lags & date <= day]
  create_fea(tst)
  tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
  # te[date == day, sales := predict(m_lgb, tst)]
  te[date == day, sales := 1.02*predict(m_lgb, tst)] # Instead of previous row use this magic multiplier
}




pred <- predict(lgb, data.matrix(test[, x, with = FALSE]))
test[, demand := pmax(0, pred)]

test_long <- dcast(test, id ~ F, value.var = "demand")

submission <- merge(sample_submission[, .(id)], 
                    test_long[, colnames(sample_submission), with = FALSE], 
                    by = "id")

fwrite(submission, file = "data/pnt_submissions/experiment_04.csv", row.names = FALSE)