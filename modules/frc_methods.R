#################################################
#                                               #
#  Statistical Univariate forecasting methods   #
#                                               #
#################################################

# Test
# time_series_b <- readRDS("data/processed/time_series_b.rds")
# x <- time_series_b[[23546]]$x


## 01. prophet pakage by Facebook ----

prophet_frc <- function(df_prophet, h = 28){
  
  # TEST
  # df_prophet = sales_df[,c("ds", "y")]
  # head(df_prophet)
  
  model <- prophet(
    df = df_prophet,
    growth = "linear",
    changepoints = NULL,
    n.changepoints = 25,
    changepoint.range = 0.8,
    yearly.seasonality = "auto",
    weekly.seasonality = "auto",
    daily.seasonality = FALSE,
    holidays = NULL,
    seasonality.mode = "additive",
    seasonality.prior.scale = 30,
    holidays.prior.scale = 30,
    changepoint.prior.scale = 0.05,
    mcmc.samples = 0,
    interval.width = 0.8,
    uncertainty.samples = 1000,
    fit = TRUE)
  
  
  future <- make_future_dataframe(m = model, periods = h)
  forecast <- predict(object = model, df = future)
  return(tail(forecast$yhat, h))
}



prophet_frc_v2 <- function(df_prophet, valid_prophet = valid_df, h = 28){
  
  ## TEST
  # df_prophet = sales_df_fold4
  # dim(df_prophet)
  # head(holidays_df)
  # df_prophet <- tail(df_prophet, 365*2)
  # head(df_prophet)
  # valid_prophet = valid_df_fold4
  
  # 1. Initialize the model
  model <- prophet(
    df = df_prophet,
    growth = "linear",
    changepoints = NULL,
    n.changepoints = 25,
    changepoint.range = 0.8,
    yearly.seasonality = T,
    weekly.seasonality = T,
    daily.seasonality = FALSE,
    holidays = NULL, # holidays_df,
    seasonality.mode = "additive",
    seasonality.prior.scale = 30,
    holidays.prior.scale = 30,
    changepoint.prior.scale = 0.05,
    mcmc.samples = 0,
    interval.width = 0.8,
    uncertainty.samples = 1000,
    fit = F)
  
  # 2. Add holidays
  model <- add_country_holidays(model, "US")
  # generated_holidays
  
  # 3. Add external regressor
  model <- add_regressor(model, name = "snap", prior.scale = NULL, standardize = "auto", mode = NULL)
  
  # 4. Add seasonality
  model <- add_seasonality(model, name = "wday", period = 7, fourier.order = 4, prior.scale = NULL, mode = "additive",
                           condition.name = NULL)
  
  # 5. Inspect the model
  # model
  
  # 6. Train the model
  model <- fit.prophet(m = model, df = df_prophet)
  
  # 7. Make future dataframe for forecasting
  future <- make_future_dataframe(m = model, periods = h, include_history = F)
  future$snap <- valid_prophet$snap
    
  # 8. Make forecastings
  forecast <- predict(object = model, df = future)
  frc <- tail(forecast$yhat, h)
  
  # rmsse(frc, valid_df$y, h, den)
  
  return(frc)
}


# Test
# plot(model, forecast)
# prophet_frc(x = c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8), h = 28)



## 02. Remix AutoTS() function

remix_autots <- function(x, h = 28, calendar, xreg){
  
  # TEST
  x = sales_fold1
  
  
  # // FOLD 1 //
  
  # Prepare data frame for AutoTS()
  calendar <- calendar[3:nrow(calendar), ]
  
  x <- data.frame(date = calendar$date[1:length(x)], sales = x)
  
  
  # Forecast for the next h=28 days
  autots_model <- RemixAutoML::AutoTS(
    data = x,
    TargetName = "sales",
    DateName = "date",
    FCPeriods = 28,
    HoldOutPeriods = 28,
    EvaluationMetric = "MSE",
    InnerEval = "AICc",
    TimeUnit = "day",
    Lags = 7,
    SLags = 1,
    MaxFourierPairs = 4,
    TSClean = TRUE,
    SkipModels = NULL,        # c("DSHW", "ARFIMA", "ARIMA", "ETS", "NNET", "TBATS", "TSLM")
    PrintUpdates = T,
    NumCores = 8
  )
  
  
  
  # Select the best model and return forecastings
  
  autots_model$EvaluationMetrics
  autots_model$TimeSeriesModel
  
  # Print best model
  print(paste("Best model:", autots_model$ChampionModel))
  
  # Return predictions
  preds = autots_model$Forecast[,2]
  
  return(preds) 
  
}

