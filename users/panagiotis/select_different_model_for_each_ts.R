## My idea: Use different forecasting method for every time series.
# Which method has the minimum error for every time_series?
# Use a time series cross validation to select the best method for every time series
View(errors_total)
apply(X = errors_total, MARGIN = 1, function(x){b_names[which.min(x)]})

best_method_ids <- apply(X = errors_total, MARGIN = 1, function(x){which.min(x)})

errors_vec <- c()
for(i in 1:nrow(errors_total)){ # For every tsid in errors_total get the error from the best method
  j <- best_method_ids[i]
  errors_vec <- c(errors_vec, errors_total[i,j])
}

sum( errors_vec * errors_total$sales / sum(errors_total$sales) )  # WRMSSE