######################### 
#                       #
#  Ensemble with MLPs   #
#                       #
#########################

## Idea:

# Use multiple MLP models, with different ni = window_length to make predictions for the next observation (fh = 1)
# After the predictions, add the forecasted value into training set, retrain the model and make prediction for the next value etc.

# Import time series data
time_series_b <- readRDS("data/processed/time_series_b.rds")

tsid <- sample(x = length(time_series_b), size = 1)
input <- time_series_b[[tsid]]$x 
length(input)

# Forecast the last value
frcst_28 <- MLP_local(input = input[1:(length(input)-1)], fh = 1, ni = 28)
frcst_21 <- MLP_local(input = input[1:(length(input)-1)], fh = 1, ni = 21)
frcst_14 <- MLP_local(input = input[1:(length(input)-1)], fh = 1, ni = 14)
frcst_7 <- MLP_local(input = input[1:(length(input)-1)], fh = 1, ni = 7)

frcst_10 <- MLP_local(input = input[1:(length(input)-1)], fh = 1, ni = 10)
frcst_5 <- MLP_local(input = input[1:(length(input)-1)], fh = 1, ni = 15)

frcst_28; frcst_21; frcst_14; frcst_7; frcst_10; frcst_5
input[length(input)]
