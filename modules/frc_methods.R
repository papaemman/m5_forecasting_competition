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


prophet_frc_v3 <- function(df_prophet, valid_prophet = valid_df, h = 28, tsid){
  
  ## TEST
  # df_prophet = sales_df_fold4
  # dim(df_prophet)
  # head(holidays_df)
  # df_prophet <- tail(df_prophet, 365*2)
  # head(df_prophet)
  # valid_prophet = valid_df_fold4
  # tsid = 6
  
  cat(tsid, "\r")
  
  # 0. Set the approprate name for the time-serie to be forecasted
  df_prophet <- df_prophet[, c(1, 3, 4, tsid)]
  colnames(df_prophet)[4] <- "y"
  
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

remix_autots <- function(df, h = 28, calendar, xreg){
  
  # // TEST //
  # head(sales_df_fold1)
  # df = sales_df_fold1
  
  ## // Process Description //
  
  # - Step 1 is to build all the models and evaluate them on the number of HoldOutPeriods periods you specify.
  # - Step 2 is to pick the winner and rebuild the winning model on the full data set.
  # - Step 3 is to generate forecasts with the final model for FCPeriods that you specify.
  
  # AutoTS builds the best time series models for each type,
  # using optimized box-cox transformations and 
  # using a user-supplied frequency for the ts data conversion along with a model-based frequency for the ts data conversion,
  # compares all types, selects the winner, and generates a forecast.
  
  ## // Models //
  
  # 1. DSHW: Double Seasonal Holt Winters
  # 2. ARFIMA: Auto Regressive Fractional Integrated Moving Average
  # 3. ARIMIA: Stepwise Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
  # 4. ETS: Additive and Multiplicitive Exponential Smoothing and Holt Winters
  # 5. NNetar: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag
  #            compared to models with up to N lags and N seasonal lags
  # 6. TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
  # 7. TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data
  
  
  ## Forecast for the next h=28 days
  
  tic()
  autots_model <- RemixAutoML::AutoTS(
    data = df,
    TargetName = "y",
    DateName = "ds",
    FCPeriods = 28,
    HoldOutPeriods = 28,
    EvaluationMetric = "MSE",
    InnerEval = "AICc",
    TimeUnit = "day",
    Lags = 28,
    SLags = 4,
    MaxFourierPairs = 4,
    TSClean = TRUE,
    SkipModels = NULL,        # c("DSHW", "ARFIMA", "ARIMA", "ETS", "NNET", "TBATS", "TSLM")
    PrintUpdates = TRUE,
    NumCores = 8
  )
  
  toc()
  
  saveRDS(autots_model, "autots_model_v2.rds")
  autots_model <- readRDS("autots_model.rds")
  
  # Q:
  # - How I can access all the modles used? (the second best, the third best etc?)
  autots_model$EvaluationMetrics
  
  # Select the best model and return forecastings
  
  autots_model$EvaluationMetrics
  autots_model$TimeSeriesModel

  View(autots_model)
  
  # Print best model
  print(paste("Best model:", autots_model$ChampionModel))
  
  # Return predictions
  preds = autots_model$Forecast[,2]
  
  return(preds) 
  
}

