########################################
#                                      #
#  M5 ForecasteR v2 [Score = 0.51084]  #
#                                      #
########################################

## Kernel's link: https://www.kaggle.com/kailex/m5-forecaster-v2 

## // My Notes //
# General Approach: 
# - Bottom up (predictions fore every time series in aggreagation level 12)
# - one global ml model (lightgbm),
# - feature engineering: create lag and rolling mean features from sales, prices, dummy calendar events



## 00. Load packages ----

library(data.table)
library(lightgbm)

# 01. Define parameters -----
set.seed(0)

h <- 28 
max_lags <- 366
tr_last <- 1913
fday <- as.IDate("2016-04-25") 

free <- function() invisible(gc())


create_dt <- function(is_train = TRUE, nrows = Inf) {
  
  if (is_train) {
    dt <- fread("data/raw/sales_train_validation.csv", nrows = nrows)
    
  } else {
    
    dt <- fread("data/raw/sales_train_validation.csv", nrows = nrows,
                drop = paste0("d_", 1:(tr_last-max_lags)))
    dt[, paste0("d_", (tr_last+1):(tr_last+2*h)) := NA_real_]
  }
  
  
  dt <- melt(dt,
             measure.vars = patterns("^d_"),
             variable.name = "d",
             value.name = "sales")
  
  cal <- fread("../input/m5-forecasting-accuracy/calendar.csv")
  dt <- dt[cal, `:=`(date = as.IDate(i.date, format="%Y-%m-%d"), 
                     wm_yr_wk = i.wm_yr_wk,
                     event_name_1 = i.event_name_1,
                     event_name_2 = i.event_name_2,
                     snap_CA = i.snap_CA,
                     snap_TX = i.snap_TX,
                     snap_WI = i.snap_WI), on = "d"]
  
  prices <- fread("../input/m5-forecasting-accuracy/sell_prices.csv")
  dt[prices, sell_price := i.sell_price, on = c("store_id", "item_id", "wm_yr_wk")]
}

create_fea <- function(dt) {
  dt[, `:=`(d = NULL,
            wm_yr_wk = NULL)]
  
  lag <- c(7, 28)
  lag_cols <- paste0("lag_", lag)
  dt[, (lag_cols) := shift(.SD, lag), by = id, .SDcols = "sales"]
  
  win <- c(7, 28)
  roll_cols <- paste0("rmean_", t(outer(lag, win, paste, sep="_")))
  dt[, (roll_cols) := frollmean(.SD, win, na.rm = TRUE), by = id, .SDcols = lag_cols]   
  
  cols <- c("item_id", "store_id", "state_id", "dept_id", "cat_id", "event_name_1", "event_name_2")   
  dt[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols]
  
  dt[, `:=`(wday = wday(date),
            mday = mday(date),
            week = week(date),
            month = month(date),
            quarter = quarter(date),
            year = year(date))]
}

#---------------------------
cat("Preprocessing datasets...\n")

tr <- create_dt()
free()

create_fea(tr)
free()

tr <- na.omit(tr)
y <- tr$sales
tr[, c("id", "sales", "date") := NULL]
free()

tr <- data.matrix(tr)
free()

cats <- c("item_id", "store_id", "state_id", "dept_id", "cat_id", 
          "wday", "mday", "week", "month", "quarter", "year", 
          "snap_CA", "snap_TX", "snap_WI")
xtr <- lgb.Dataset(tr, label = y, categorical_feature = cats)

rm(tr, y, cats)
free()

#---------------------------
cat("Training model...\n")

p <- list(objective = "poisson",
          metric ="rmse",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)
m_lgb <- lgb.train(p, xtr, 4000)

imp <- lgb.importance(m_lgb)
lgb.plot.importance(imp, 20, cex = 0.9) 

rm(xtr, p)
free()

#---------------------------
cat("Forecasting...\n")

te <- create_dt(FALSE)

for (day in as.list(seq(fday, length.out = 2*h, by = "day"))){
  cat(as.character(day), " ")
  tst <- te[date >= day - max_lags & date <= day]
  create_fea(tst)
  tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
  te[date == day, sales := 1.02*predict(m_lgb, tst)] # magic multiplier by kyakovlev
}

te[date >= fday
   ][date >= fday+h, id := sub("validation", "evaluation", id)
     ][, d := paste0("F", 1:28), by = id
       ][, dcast(.SD, id ~ d, value.var = "sales")
         ][, fwrite(.SD, "sub_dt_lgb.csv")]