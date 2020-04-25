##################################
#                                #
#  Create submission file        #
#                                #
##################################

## 06. Forecasting ----

cat("Forecasting...\n")


# Import dataset required for forecasting
te <- create_dt(is_train = FALSE)

# dim(te) 3536840      16


# SOS: For every day, using the te (test) dataset and create_fea() function,
#      create features, taking into consideration the predictions (as the ground truth value for the demand)


for (day in as.list(seq(fday, length.out = 2*h, by = "day"))){
  
  ## Test
  # day= as.Date("2016-04-25")
  cat(as.character(day), " ")
  
  
  # Get a subset from te (test) dataset, with the last 60 days
  # because all these days required to create the features (lag, roll_means etc)
  tst <- te[date >= day - max_lags & date <= day]
  
  # Create features for this dataset
  create_fea(tst)
  
  # Crate test dataset for the current date
  tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
  
  # Make predictions and save them in te (test) dataset. 
  te[date == day, sales := 1.02*predict(m_lgb, tst)] # magic multiplier by kyakovlev
  
  # Iteration: Use again the te dataset to subset the lad 60 days, to create features and make new predictions.
  
}



## 02. Create submission file ----

te[date >= fday
   ][date >= fday+h, id := sub("validation", "evaluation", id)
     ][, d := paste0("F", 1:28), by = id
       ][, dcast(.SD, id ~ d, value.var = "sales")
         ][, fwrite(.SD, "sub_dt_lgb.csv")]




