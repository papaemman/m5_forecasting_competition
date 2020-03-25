### load a time series singal
sales_aggregation_level_1 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_1.rds")

#### forecasting based knn ####
library(tsfknn)

frc_knn <- function(x=sales_aggregation_level_1,h,k){
  sales_aggregation_level_1<-ts(sales_aggregation_level_1)
  pred<-tsfknn::knn_forecasting(sales_aggregation_level_1,h,k)
  pred$prediction
}

frc_knn(sales_aggregation_level_1,h=2 ,k=2)
######


#### forecasting using elm neural network ####
?elm
library(nnfor)
frc_mlp <-function(x,h){
  x <- ts(x)
  model <- elm(x)
  forecast(model,h)
}


frc_mlp(x=sales_aggregation_level_1,h=20)
########


?mlp()
function(x,h){
  x <- ts(x)

  z <- 1:(length(x)+h) # I add 28 extra observations for the forecasts
  z <- cbind(z) # Convert it into a column-array
  model <- mlp(x,xreg=z,xreg.lags=list(0),xreg.keep=list(TRUE),
              # Add a lag0 regressor and force it to stay in the model
              difforder=0) # Do not let mlp() to remove the stochastic trend
  forecast(model$fitted, h)
}




##### prophet forecasting #####
library(prophet)

prophet_frc <- function(x,x_length,h){
  df <- data.frame(ds = seq(as.Date('2015-01-01'),length.out = x_length, by = 'd'),y=x)
  model <-prophet(df)
  future <- make_future_dataframe(model, periods = h)
  forecast <- predict(model, future)
  return(forecast)
  }

prophet_frc(sales_aggregation_level_1,27,10)
#####

plot(model,forecast)
