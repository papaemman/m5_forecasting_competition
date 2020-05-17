#######################################
#                                     # 
#   Time series forecasting and CV    #
#        using rmsse                  #     
#                                     #
#######################################

## 00. Source dependecnies ----

# Available models
source("modules/main.R")
source("modules/statistical_univariate_frc_methods.R")
source("modules/frc_methods.R")
source("modules/helpers.R")

library(RemixAutoML)


## 01. Import calendar data ----

# Calendar
calendar <- read.csv("data/raw/calendar.csv", na.strings = c("_",""), stringsAsFactors = F)
calendar$date <- as.Date(calendar$date)

# Create external regressors
x_var <- calendar
x_var$snap <- x_var$snap_CA + x_var$snap_WI + x_var$snap_TX
x_var$holiday <- 0
x_var[is.na(x_var$event_type_1)==F,]$holiday <- 1
x_var <- x_var[,c("snap","holiday")]
head(x_var, 10)


# holidays_df <- data.frame(ds = calendar$date,
#                           holiday = calendar$event_name_1,
#                           lower_window = -2,
#                           upper_window = 2,
#                           prior_scale = 1,
#                           stringsAsFactors = F)
# head(holidays_df)
# str(holidays_df)


## 02. Import time-series data ----

## // Bottom level time series //
time_series_b <- readRDS("data/processed/time_series_b.rds")
sales <- time_series_b[[100]]$x
length(sales)


## // Aggregated time series //

# Note: For every time-series (154 time series):
# - I have to run a cross-validation process, for 4 folds (the same month previous year, the same month pre-prebious year, last month, pre-last month)
# - with different datasets (from start, last 1 year, last 2 years)
# - rmsse as evaluation metric (with denominator/scaling calculated from all the dataset)


# HOW TO: Calculate denominator for wrmsse
# den <- sum(diff(sales)^2) / (length(sales)-1)


# H1
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_1.rds")
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)

# H2
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_2.rds")
View(sales)
sales <- as.numeric(sales[2, 2:ncol(sales)])
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)

# H3
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_3.rds")
View(sales)
sales <- as.numeric(sales[3, 2:ncol(sales)])
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)

# H4
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_4.rds")
View(sales)
sales <- as.numeric(sales[3, 2:ncol(sales)])
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)

# H5
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_4.rds")
View(sales)
sales <- as.numeric(sales[3, 2:ncol(sales)])
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)

# H6
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_6.rds")
View(sales)
sales <- as.numeric(sales[3, 3:ncol(sales)])
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)

# H7
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_7.rds")
View(sales)
sales <- as.numeric(sales[3, 3:ncol(sales)])
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)

# H8
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_8.rds")
View(sales)
sales <- as.numeric(sales[3, 3:ncol(sales)])
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)

# H9
sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_9.rds")
View(sales)
sales <- as.numeric(sales[3, 3:ncol(sales)])
length(sales)
den <- sum(diff(sales)^2) / (length(sales)-1)


## Create data frame withe dates and sales
df <- data.frame(ds = calendar[1:length(sales), "date"],
                 day = calendar[1:length(sales), "weekday"],
                 wday = calendar[1:length(sales), "wday"],
                 y = sales,
                 snap = x_var$snap[1:length(sales)],
                 holiday = x_var$holiday[1:length(sales)])

head(df)
dim(df)

## Plot sales
ggplot(df, aes(ds, y)) + geom_line()


## 03. Drop first observations ----

# 1. Drop first 2 days from sales data, to start from Monday
# df <- df[3:nrow(df), ]

# 2. Drop first 1000 days
# N = 1000
# df <- df[1:N, ]



## 04. k-fold time-series CV process ----

## // 01. Expanding window: Train and validation sets //

# Last month
sales_df_fold1 <- df[1:(nrow(df)-28*1),]
valid_df_fold1 <- df[(nrow(df)-28*1+1):nrow(df),]

# Pre-last month
sales_df_fold2 <- df[1:(nrow(df)-28*2),]
valid_df_fold2 <- df[(nrow(df)-28*2+1):(nrow(df)-28*1),]

# Same month as the real validation set but previous year - 2015 (from start)
sales_df_fold3 <- df[1:which(df$ds == "2015-04-24"),]
valid_df_fold3 <- df[(which(df$ds == "2015-04-24")+1):(which(df$ds == "2015-04-24")+28),]

# Same month as the real validation set but pre-previous year - 2014 (from start)
sales_df_fold4 <- df[1:which(df$ds == "2014-04-24"),]
valid_df_fold4 <- df[(which(df$ds == "2014-04-24")+1):(which(df$ds == "2014-04-24")+28),]


## // 02. Sliding window 1 year: Train and validation sets //

# Last month
sales_df_fold1 <- df[which(df$ds == "2015-03-28"):which(df$ds == "2016-03-27"),]
valid_df_fold1 <- df[(nrow(df)-28*1+1):nrow(df),]

# Pre-last month
sales_df_fold2 <- df[which(df$ds == "2015-03-01"):which(df$ds == "2016-02-28"),]
valid_df_fold2 <- df[(nrow(df)-28*2+1):(nrow(df)-28*1),]

# Same month as the real validation set but previous year - 2015 (from start)
sales_df_fold3 <- df[which(df$ds == "2014-04-25"):which(df$ds == "2015-04-24"),]
valid_df_fold3 <- df[(which(df$ds == "2015-04-24")+1):(which(df$ds == "2015-04-24")+28),]

# Same month as the real validation set but pre-previous year - 2014 (from start)
sales_df_fold4 <- df[which(df$ds == "2013-04-25"):which(df$ds == "2014-04-24"),]
valid_df_fold4 <- df[(which(df$ds == "2014-04-24")+1):(which(df$ds == "2014-04-24")+28),]


## // 02. Sliding window 2 years: Train and validation sets //

# Last month
sales_df_fold1 <- df[which(df$ds == "2014-03-28"):which(df$ds == "2016-03-27"),]
valid_df_fold1 <- df[(nrow(df)-28*1+1):nrow(df),]

# Pre-last month
sales_df_fold2 <- df[which(df$ds == "2014-03-01"):which(df$ds == "2016-02-28"),]
valid_df_fold2 <- df[(nrow(df)-28*2+1):(nrow(df)-28*1),]

# Same month as the real validation set but previous year - 2015 (from start)
sales_df_fold3 <- df[which(df$ds == "2013-04-25"):which(df$ds == "2015-04-24"),]
valid_df_fold3 <- df[(which(df$ds == "2015-04-24")+1):(which(df$ds == "2015-04-24")+28),]

# Same month as the real validation set but pre-previous year - 2014 (from start)
sales_df_fold4 <- df[which(df$ds == "2012-04-25"):which(df$ds == "2014-04-24"),]
valid_df_fold4 <- df[(which(df$ds == "2014-04-24")+1):(which(df$ds == "2014-04-24")+28),]


## CV Calculations ----
fold_1 <- ts_forecasting(sales = sales_df_fold1, valid = valid_df_fold1, h = 28, den = den)
fold_2 <- ts_forecasting(sales = sales_df_fold2, valid = valid_df_fold2, h = 28, den = den)
fold_3 <- ts_forecasting(sales = sales_df_fold3, valid = valid_df_fold3, h = 28, den = den)
fold_4 <- ts_forecasting(sales = sales_df_fold4, valid = valid_df_fold4, h = 28, den = den)



## // Check Results //

fold_4$rmsse_results
View(cv_results[[1]]$df_residuals)

cv_results <- list(fold_1, fold_2, fold_3, fold_4, fold_5)
cv_results <- list(fold_4, fold_5)


View(cv_results[[1]]$df_residuals)
View(cv_results[[2]]$df_residuals)

sapply(cv_results, function(x){x[[2]]})
sapply(cv_results, function(x){x[[2]]}) %>% rowSums()/3


df <- cv_results[[1]]$df_residuals
df <- fold_4$df_residuals

df$pos_residual <- apply(df, 1, function(x){sum(x>=0)})
df$neg_residual <- apply(df, 1, function(x){sum(x<0)})

df$best_method <- colnames(df %>% select(starts_with("res")))[apply(abs(df %>% select(starts_with("res"))), 1, which.min)]

View(df)
table(df$best_method)

cor(df %>%  select(starts_with("res")))

df
ggplot(df) + 
  geom_point(aes(res_snaive_month, res_prophet_v2))+
  geom_vline(xintercept = 0, col = "red", linetype = 3)+
  geom_hline(yintercept = 0, col = "red", linetype = 3)


## 05. Test indicidual methods ----
autots_model <- readRDS("autots_model.rds")
View(autots_model)

autots_model$ChampionModel
autots_model$TimeSeriesModel

library(forecast)

# FOLD 1
model <- nnetar(y = ts(sales_df_fold1$y, frequency = 365), p = 28, P = 4, size = 16, repeats = 20)
model
frc <- forecast(object = model, h = 28)
rmsse(preds = as.numeric(frc$mean), valid = valid_df_fold1$y, h = 28, den = den)

# FOLD 2
model <- nnetar(y = ts(sales_df_fold2$y, frequency = 365), p = 28, P = 4, size = 16, repeats = 20)
frc <- forecast(object = model, h = 28)
rmsse(preds = as.numeric(frc$mean), valid = valid_df_fold2$y, h = 28, den = den)

# FOLD 3
model <- nnetar(y = ts(sales_df_fold3$y, frequency = 365), p = 28, P = 4, size = 16, repeats = 20)
frc <- forecast(object = model, h = 28)
rmsse(preds = as.numeric(frc$mean), valid = valid_df_fold3$y, h = 28, den = den)

# FOLD 4
model <- nnetar(y = ts(sales_df_fold4$y, frequency = 365), p = 28, P = 4, size = 16, repeats = 20)
frc <- forecast(object = model, h = 28)
rmsse(preds = as.numeric(frc$mean), valid = valid_df_fold4$y, h = 28, den = den)


df <- data.frame(date = valid_df_fold4$ds,
                 preds = as.numeric(frc$mean),
                 preds_pr = preds_prophet_v2,
                 valid = valid_df_fold4$y)
ggplot(df) +
  geom_line(aes(date, valid),size = 1) + geom_point(aes(date, valid),size = 1) +
  geom_line(aes(date, preds), col = "cyan") + geom_point(aes(date, preds), col = "cyan")+
  geom_line(aes(date, preds_pr), col = "yellow") + geom_point(aes(date, preds_pr), col = "yellow")


rmsse(preds = (as.numeric(frc$mean) + df$preds_pr)/2, valid = valid_df_fold4$y, h = 28, den = den)
rmsse(preds =  df$preds_pr, valid = valid_df_fold4$y, h = 28, den = den)


# Ensemble

  
## 05. Define helper functions ----


## RMSE caclulations
rmse <- function(preds, valid, h){
  return ( sqrt( sum((valid - preds)^2) / h ) )
}


## RMSSE calculations
rmsse <- function(preds, valid, h, den){
  return ( sqrt( sum((valid - preds)^2) / ( h * den) ) )
}


## Time-series forecasting methods
ts_forecasting <- function(sales_df, valid_df, h = 28, den){
  
  ## TEST
  # sales_df = sales_df_fold4
  # valid_df = valid_df_fold4
  # tail(sales_df)
  # head(valid_df)
  # h = 28
  # den = den
  
  
  
  ## 1. Make Forcastings ----
  
  # SOS: Functions are define in statistical_univariate_frc_methods.R script
  print("Create forecastings...")
  
  
  ## - Separate forecastings models -
  
  preds_snaive <- Naive(x = sales_df$y, h = h, type = "seasonal")
  preds_snaive_month <- Naive(x = sales_df$y, h = h, type = "seasonal_month")
  
  preds_smooth_es <- smooth_es(x = sales_df$y, h = h)
  preds_smooth_es_v2 <- smooth_es_v2(x = sales_df$y, h = h, xreg = x_var)
  
  preds_auto_arima <- auto_arima(x = sales_df$y, h = h)
  
  preds_prophet <- prophet_frc(df_prophet = sales_df, h = h)
  
  preds_prophet_v2 <- prophet_frc_v2(df_prophet = sales_df, valid_prophet = valid_df, h = h)
  
  # preds_autots <- remix_autots(x = sales, h = h)
  # preds_autots <- autots_model$Forecast$Forecast_TBATS_ModelFreqTSC
  
  
  ## - Ensemble forecastings models -
  preds_ensemble_v1 <- (preds_snaive_month + preds_smooth_es + preds_auto_arima + preds_prophet)/4
  preds_ensemble_v2 <- (preds_snaive_month + preds_smooth_es)/2
  preds_ensemble_v3 <- (preds_snaive_month + preds_prophet + preds_prophet_v2)/3
  preds_ensemble_v4 <- (preds_smooth_es_v2 + preds_prophet_v2)/2
  preds_ensemble_v5 <- (preds_snaive_month + preds_prophet_v2)/2
  
  
  ## 2. Residual Analysis ----
  print("Residual analysis")
  
  # Check residuals per day
  df_residuals <- data.frame(
    res_snaive = preds_snaive - valid_df$y, 
    res_snaive_month = preds_snaive_month - valid_df$y,
    res_smooth_es = preds_smooth_es - valid_df$y,
    res_smooth_es_v2 = preds_smooth_es_v2 - valid_df$y,
    res_auto_arima = preds_auto_arima - valid_df$y,
    res_prophet = preds_prophet - valid_df$y,
    res_prophet_v2 = preds_prophet_v2 - valid_df$y,
    res_ensemble_v1 = preds_ensemble_v1 - valid_df$y,
    res_ensemble_v2 = preds_ensemble_v2 - valid_df$y,
    res_ensemble_v3 = preds_ensemble_v3 - valid_df$y,
    res_ensemble_v4 = preds_ensemble_v4 -  valid_df$y,
    res_ensemble_v5 = preds_ensemble_v5 -valid_df$y
  )
  
  # View(df_residuals)
  # 
  # sapply(X = df_residuals %>% mutate(week = rep(c(1,2,3,4), each = 7)) %>%  split(.$week),
  #        FUN = function(x){
  #          apply(X = x ,
  #                MARGIN = 2,
  #                FUN = function(col)sum(abs(col)))
  #        })
             
                            
  # mean(preds_snaive - valid_df$y)
  # mean(preds_snaive_month - valid_df$y)
  # mean(preds_smooth_es - valid_df$y)
  # mean(preds_smooth_es_v2 - valid_df$y)
  # mean(preds_auto_arima - valid_df$y)
  # mean(preds_prophet - valid_df$y)
  
  
  ## Plot forecastings

  df <- data.frame(date = valid_df$ds,
                   preds_snaive,
                   preds_snaive_month,
                   preds_smooth_es,
                   preds_smooth_es_v2,
                   preds_auto_arima,
                   preds_prophet,
                   preds_prophet_v2,
                   preds_ensemble_v1,
                   preds_ensemble_v2,
                   preds_ensemble_v3,
                   preds_ensemble_v4,
                   preds_ensemble_v5,
                   valid = valid_df$y)

  ## Check total RMSSE
  apply(df %>% select(starts_with("preds")), 2, FUN = function(x)rmsse(preds = x, valid = df$valid, h = 28, den))


  ## Check RMSSE per week
  sapply(X = df %>% mutate(week = rep(c(1,2,3,4), each = 7)) %>%  split(.$week),
         FUN = function(x){
           apply(X = x %>% select(starts_with("preds")),
                 MARGIN = 2,
                 FUN = function(col)rmsse(preds = col, valid = x[,"valid"], h = 7, den))
           }) %>% View()



  p <- ggplot(df) +
    geom_line(aes(date, valid),size = 1) + geom_point(aes(date, valid),size = 1) +
    geom_line(aes(date, preds_snaive), col = "cyan") + geom_point(aes(date, preds_snaive), col = "cyan")+
    geom_line(aes(date, preds_snaive_month), col = "yellow") + geom_point(aes(date, preds_snaive_month), col = "yellow")+
    geom_line(aes(date, preds_prophet_v2), col = "blue") + geom_point(aes(date, preds_prophet_v2), col = "blue")+
    geom_line(aes(date, preds_smooth_es), col = "purple") + geom_point(aes(date, preds_smooth_es), col = "purple")+
    geom_line(aes(date, preds_auto_arima), col = "seagreen1") + geom_point(aes(date, preds_auto_arima), col = "seagreen1")+
    ggtitle("Validation vs predictions")

  p
  ggplotly(p)
  

  
  ## 3. Evaluate forecastings ----
  print("evaluate forecastings...")
  
  
  ## // RMSSE //
  
  # rmse_snaive <- rmse(preds_snaive, valid_df$y, h)
  # rmse_snaive_month <- rmse(preds_snaive_month, valid_df$y, h)
  # 
  # rmse_smooth_es <- rmse(preds_smooth_es, valid_df$y, h)
  # rmse_smooth_es_v2 <- rmse(preds_smooth_es_v2, valid_df$y, h)
  # 
  # rmse_auto_arima <- rmse(preds_auto_arima, valid_df$y, h)
  # 
  # rmse_prophet <- rmse(preds_prophet, valid_df$y, h)
  # rmse_prophet_v2 <- rmse(preds_prophet_v2, valid_df$y, h)
  # 
  # # rmsse_autots <- rmsse(preds_autots, valid_df$y, h)
  # 
  # rmse_ensemble_v1 <- rmse(preds_ensemble_v1, valid_df$y, h)
  # rmse_ensemble_v2 <- rmse(preds_ensemble_v2, valid_df$y, h)
  # rmse_ensemble_v3 <- rmse(preds_ensemble_v3, valid_df$y, h)
  # rmse_ensemble_v4 <- rmse(preds_ensemble_v4, valid_df$y, h)
  # 
  # rmse_results <- c("rmse_snaive" = rmse_snaive, "rmse_snaive_month" = rmse_snaive_month,
  #                    "rmse_smooth_es" = rmse_smooth_es, "rmse_smooth_es_v2" = rmse_smooth_es_v2,
  #                    "rmse_auto_arima" = rmse_auto_arima,
  #                    "rmse_prophet" = rmse_prophet, "rmse_prophet_v2" = rmse_prophet_v2,
  #                    "rmse_ensemble_v1" = rmse_ensemble_v1, "rmse_ensemble_v2" = rmse_ensemble_v2,
  #                    "preds_ensemble_v3" = rmse_ensemble_v3, "preds_ensemble_v4" = rmse_ensemble_v4)
  # 
  # ls <- list("df_residuals" = df_residuals, "rmse_results" = rmse_results)
  
  ## // RMSSE //
  
  rmsse_snaive <- rmsse(preds_snaive, valid_df$y, h, den)
  rmsse_snaive_month <- rmsse(preds_snaive_month, valid_df$y, h, den)

  rmsse_smooth_es <- rmsse(preds_smooth_es, valid_df$y, h, den)
  rmsse_smooth_es_v2 <- rmsse(preds_smooth_es_v2, valid_df$y, h, den)

  rmsse_auto_arima <- rmsse(preds_auto_arima, valid_df$y, h, den)

  rmsse_prophet <- rmsse(preds_prophet, valid_df$y, h, den)
  rmsse_prophet_v2 <- rmsse(preds_prophet_v2, valid_df$y, h, den)

  # rmsse_autots <- rmsse(preds_autots, valid_df$y, h, den)

  rmsse_ensemble_v1 <- rmsse(preds_ensemble_v1, valid_df$y, h, den)
  rmsse_ensemble_v2 <- rmsse(preds_ensemble_v2, valid_df$y, h, den)
  rmsse_ensemble_v3 <- rmsse(preds_ensemble_v3, valid_df$y, h, den)
  rmsse_ensemble_v4 <- rmsse(preds_ensemble_v4, valid_df$y, h, den)
  rmsse_ensemble_v5 <- rmsse(preds_ensemble_v5, valid_df$y, h, den)

  rmsse_results <- c("rmsse_snaive" = rmsse_snaive, "rmsse_snaive_month" = rmsse_snaive_month,
                     "rmsse_smooth_es" = rmsse_smooth_es, "rmsse_smooth_es_v2" = rmsse_smooth_es_v2,
                     "rmsse_auto_arima" = rmsse_auto_arima,
                     "rmsse_prophet" = rmsse_prophet, "rmsse_prophet_v2" = rmsse_prophet_v2,
                     "rmsse_ensemble_v1" = rmsse_ensemble_v1, "rmsse_ensemble_v2" = rmsse_ensemble_v2,
                     "preds_ensemble_v3" = rmsse_ensemble_v3, "preds_ensemble_v4" = rmsse_ensemble_v4,
                     "rmsse_ensemble_v5" = rmsse_ensemble_v5)
  
  ls <- list("df_residuals" = df_residuals, "rmsse_results" = rmsse_results)
  ls
  
  ##  // Return results //
  
  return(ls)
}



## Time-series forecasting methods (old)
ts_forecasting_v0 <- function(sales, valid, h = 28, den){
  
  ## TEST
  # sales = sales_fold1
  # valid = valid_fold1
  # h = 28
  # den = den
  
  
  ## Make Forcastings
  print("Create forecastings...")
  
  # Constant forecastings
  preds_naive <- Naive(x = sales, h = h, type = "naive")
  preds_ses <- SES(a = 0.1, x =sales, h = h, job = "NA")$mean
  preds_sexps <- SexpS(x = sales, h = h)
  preds_ma <- MA(x = sales, h = h)
  preds_tsb <- TSB(x = sales, h = h)
  preds_adida <- ADIDA(x = sales, h = h)
  preds_imapa <- iMAPA(x = sales, h = h)
  
  # Non-constant forecastings
  preds_snaive <- Naive(x = sales, h = h, type = "seasonal")
  preds_ma_v2 <- MA_v2(x = sales, h = h)
  preds_smooth_es <- smooth_es(x = sales, h = h)
  preds_auto_arima <- auto_arima(x = sales, h = h)
  preds_prophet <- prophet_frc(x = sales, h = h)
  
  
  ## Evaluate forecastings
  print("evaluate forecastings...")
  
  rmsse_naive <- rmsse(preds_naive, valid, h, den)
  rmsse_ses <- rmsse(preds_ses, valid, h, den)
  rmsse_sexps <- rmsse(preds_sexps, valid, h, den)
  rmsse_ma <- rmsse(preds_ma, valid, h, den)
  rmsse_tsb <- rmsse(preds_tsb, valid, h, den)
  rmsse_adida <- rmsse(preds_adida, valid, h, den)
  rmsse_imapa <- rmsse(preds_imapa, valid, h, den)
  rmsse_snaive <- rmsse(preds_snaive, valid, h, den)
  rmsse_ma_v2 <- rmsse(preds_ma_v2, valid, h, den)
  rmsse_smooth_es <- rmsse(preds_smooth_es, valid, h, den)
  rmsse_auto_arima <- rmsse(preds_auto_arima, valid, h, den)
  rmsse_prophet <- rmsse(preds_prophet, valid, h, den)
  
  
  res <- c("rmsse_naive" = rmsse_naive, "rmsse_ses" = rmsse_ses, "rmsse_sexps" = rmsse_sexps, "rmsse_ma" = rmsse_ma,
           "rmsse_tsb" = rmsse_tsb, "rmsse_adida" = rmsse_adida, "rmsse_imapa" = rmsse_imapa, "rmsse_snaive" = rmsse_snaive,
           "rmsse_ma_v2" = rmsse_ma_v2, "rmsse_smooth_es" = rmsse_smooth_es, "rmsse_auto_arima" = rmsse_auto_arima, 
           "rmsse_prophet" = rmsse_prophet)

  return(res)
}






























