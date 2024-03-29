##################################
#                                #
#  Create submission file        #
#                                #
##################################


library(lightgbm)
library(data.table)
library(tictoc)

## Source dependencies
source("ml_modules/create_data.R")
source("ml_modules/fe_sales.R")


## Define parameters

nrows = Inf                        # How many sales data to import
fh = 28                            # Forecasting horizon
max_lags = 366                     # Max lags (features)
tr_last = 1913                     # Training last day
fday = as.IDate("2016-04-25")      # Forecasting day (first day)


## 06. Forecasting ----

# Import test dataset required for forecasting 
te <- create_dt(is_train = FALSE, nrows = nrows, fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday)

## Preprocessing steps and add features
te <- add_features(dt = te)
gc()


# SOS: For every forecasting day,
#      using the te (test) dataset and create_fea() function,
#      create features, taking into consideration the predictions (as the ground truth value for the demand)


for (day in as.list(seq(fday, length.out = 2*fh, by = "day"))){
  
  ## Test
  # day = as.Date("2016-04-25")
  cat(as.character(day), "\n")
  
  # Get a subset from te (test) dataset, with the last max_lags days
  # because these are the days required to create the prediction features (lag, roll_means etc)
  tst <- te[date >= day - max_lags & date <= day]
  
  # Create features for this dataset
  tst <- create_sales_features(tst)
  gc()
  
  # Create test dataset for the current date
  # Drop unused features
  tst <- data.matrix(tst[date == day][, c("id", "d", "sales", "date") := NULL])
  
  # Note: Some big lags may be NA for some products,
  # because these products may not be available during all this period of time.
  # sapply(as.data.frame(tst), function(x)sum(is.na(x))) %>% View()
  
  
  # Make predictions and save them in te (test) dataset. 
  # (use magic multiplier in prediction)
  te[date == day, sales := 1.01*predict(lgb_model, tst)] 
  
  
  # Iteration: Use again the te (test) dataset to subset the last max_lags days, to create features and make new predictions.
}

te


## 02. Create submission file ----

# - Filter the required days
# - For observations after fday + fh (=28), change (sub) the id name from validation to evaluation
# - Create a column d with values F1:F28 according to id column
# - Transform datasets from long to wide, using dcast, keeping only id, d, sales columns (.SD = subset data)
# - order according to sample submission data
# - write submission file

sample_submission <- fread("data/pnt_submissions/sample_submission.csv")

te[date >= fday
   ][date >= fday+fh, id := sub("validation", "evaluation", id)
     ][,d := as.character(d)
       ][, d := paste0("F", 1:28), by = id
         ][, dcast(.SD, id ~ d, value.var = "sales")
           ][sample_submission[,.(id)], on = "id"
             ][, j = c("id", paste0("F", 1:28))
               ][, fwrite(.SD, "data/pnt_submissions/sub_dt_lgb_v0_1.01.csv")]


