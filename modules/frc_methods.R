#################################################
#                                               #
#  Statistical Univariate forecasting methods   #
#                                               #
#################################################

# Test
# time_series_b <- readRDS("data/processed/time_series_b.rds")
# x <- time_series_b[[23546]]$x


## 01. prophet pakage by Facebook ----

prophet_frc <- function(x, h = 28){
  
  df <- data.frame(ds = seq(from = as.Date('2016-06-19') - length(x), length.out = length(x), by = 'day'),
                   y = x)
  
  model <- prophet(df = df, yearly.seasonality = "auto",
                            weekly.seasonality = "auto", daily.seasonality = "auto")
  
  future <- make_future_dataframe(m = model, periods = h)
  forecast <- predict(object = model, df = future)
  return(tail(forecast$yhat, h))
}

# Test
# plot(model, forecast)
# prophet_frc(x = c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8), h = 28)

