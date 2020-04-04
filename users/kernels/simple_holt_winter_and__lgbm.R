######################################################
#                                                    #
#  Simple Holt-Winters and LGBM  [Score: 0.62399]    #
#                                                    #
######################################################

## Kernel's link: https://www.kaggle.com/nxrprime/m5-simple-holt-winters-and-lgbm

## // My Notes //
# General Approach: 
# - Bottom up (predictions fore every time series in aggreagation level 12)
# - one global ml model (lightgbm),
# - feature engineering: create lag and rolling mean features from sales, prices, dummy calendar events



ry(data.table)
library(lightgbm)

set.seed(0)

h <- 28 
max_lags <- 400
tr_last <- 1913
fday <- as.IDate("2016-04-25") 
df_sales <- read.csv("../input/m5-forecasting-accuracy/sales_train_validation.csv", stringsAsFactors=F)

#---------------------------
cat("Creating auxiliary functions...\n")

free <- function() invisible(gc())

create_dt <- function(is_train = TRUE, nrows = Inf) {
  
  prices <- fread("../input/m5-forecasting-accuracy/sell_prices.csv")
  cal <- fread("../input/m5-forecasting-accuracy/calendar.csv", drop = "weekday")
  cal[, date := as.IDate(date, format="%Y-%m-%d")]
  
  if (is_train) {
    dt <- fread("../input/m5-forecasting-accuracy/sales_train_validation.csv", nrows = nrows)
  } else {
    dt <- fread("../input/m5-forecasting-accuracy/sales_train_validation.csv", nrows = nrows,
                drop = paste0("d_", 1:(tr_last-max_lags)))
    dt[, paste0("d_", (tr_last+1):(tr_last+2*h)) := NA_real_]
  }
  
  dt <- melt(dt,
             measure.vars = patterns("^d_"),
             variable.name = "d",
             value.name = "sales")
  
  dt <- dt[cal, `:=`(date = i.date, 
                     wm_yr_wk = i.wm_yr_wk,
                     event_name_1 = i.event_name_1,
                     snap_CA = i.snap_CA,
                     snap_TX = i.snap_TX,
                     snap_WI = i.snap_WI), on = "d"]
  
  dt[prices, sell_price := i.sell_price, on = c("store_id", "item_id", "wm_yr_wk")]
}

create_fea <- function(dt) {
  
  lag <- c(7, 28, 29)
  dt[, (paste0("lag_", lag)) := shift(.SD, lag), .SDcols = "sales", by = "id"]
  
  win <- c(7, 30, 90, 180)
  dt[, (paste0("roll_mean_28_", win)) := frollmean(lag_28, win), by = "id"]
  
  win <- c(7, 30)
  dt[, (paste0("roll_max_28_", win)) := frollapply(lag_28, win, max), by = "id"]
  
  dt[, price_change_1 := sell_price / shift(sell_price) - 1, by = "id"]
  dt[, price_change_365 := sell_price / frollapply(shift(sell_price), 365, max) - 1, by = "id"]
  
  cols <- c("item_id", "state_id", "dept_id", "cat_id", "event_name_1")   
  dt[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols]
  
  dt[, `:=`(wday = wday(date),
            mday = mday(date),
            week = week(date),
            month = month(date),
            year = year(date),
            store_id = NULL,
            d = NULL,
            wm_yr_wk = NULL)]
}

cat("Creating training set with features...\n")

tr <- create_dt()
free()

create_fea(tr)
free()

tr <- na.omit(tr)
y <- tr$sales

idx <- tr[date <= max(date)-h, which = TRUE] 

tr[, c("id", "sales", "date") := NULL]
free()

tr <- data.matrix(tr)
free()

cat("Constructing training and validation sets for GBM...\n")

cats <- c("item_id", "state_id", "dept_id", "cat_id",
          "wday", "mday", "week", "month", "year",  
          "snap_CA", "snap_TX", "snap_WI")

xtr <- lgb.Dataset(tr[idx, ], label = y[idx], categorical_feature = cats)
xval <- lgb.Dataset(tr[-idx, ], label = y[-idx], categorical_feature = cats)

rm(tr, y, idx)
free()

cat("Training model...\n")

p <- list(objective = "regression_l2",
          metric ="rmse",
          learning_rate = 0.05,
          sub_feature = 0.75,
          sub_row = 0.75,
          bagging_freq = 10,
          lambda = 0.1,
          alpha = 0.1,
          nthread = 4)

m_lgb <- lgb.train(params = p,
                   data = xtr,
                   nrounds = 6000,
                   valids = list(train = xtr, valid = xval),
                   early_stopping_rounds = 1000,
                   eval_freq = 10,
                   verbose = 0)

lgb.plot.importance(lgb.importance(m_lgb), 20)

rm(xtr, xval, p)
free()

#---------------------------

te <- create_dt(FALSE)

for (day in as.list(seq(fday, length.out = 2*h, by = "day"))){
  cat(as.character(day), " ")
  tst <- te[date >= day - max_lags & date <= day]
  create_fea(tst)
  tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
  te[date == day, sales := predict(m_lgb, tst)]
}

te[date >= fday
   ][date >= fday+h, id := sub("validation", "evaluation", id)
     ][, d := paste0("F", 1:28), by = id
       ][, dcast(.SD, id ~ d, value.var = "sales")
         ][, fwrite(.SD, "sub_dt_lgb.csv")]


# %% [code]
library(stringr)

generate_forecasts <- function(df_sales, f_method, h=28, f_trim=NA) {
  
  if (is.na(f_trim)) {
    f_trim <- function(x) {x}
  } else if (is.numeric(f_trim)) {
    n_tail <- f_trim
    f_trim <- function(x) {tail(x, n_tail)}
  }
  
  d <- dim(df_sales)
  
  ls_forecasts = list()
  
  for (ii in 1:nrow(df_sales)) {
    
    if (ii %% 1000 == 0) {
      cat(paste0(ii, "...\n"))
    }
    
    id <- df_sales$id[ii]
    x <- as.numeric(df_sales[ii, 7:d[2]])
    x <- f_trim(x)
    x <- ts(x, frequency=7)
    fcst <- f_method(x, h)
    ls_forecasts[[id]] <- fcst
  }
  
  ls_forecasts
}


create_output_df <- function(ls_forecasts) {
  
  ids <- names(ls_forecasts)
  
  # create empty matrix for output
  M_fcst <- matrix(NA_real_, nrow=length(ids), ncol=28)
  colnames(M_fcst) <- paste0("F", 1:28)
  
  # populate data:
  for (ii in 1:length(ids)) {
    id <- ids[ii]
    fcst <- ls_forecasts[[id]]
    M_fcst[ii,] <- pmax(0, fcst)
  }
  
  # we've calculated '_validation', but since '_evaluation' is required as
  # well for a valid submission, we just copy '_validation' forecasts and
  # rename them.
  df <- data.frame(
    id = ids,
    M_fcst,
    stringsAsFactors=F
  )
  df2 <- data.frame(
    id = str_replace(ids, "_validation", "_evaluation"),
    M_fcst,
    stringsAsFactors=F
  )
  rbind(df, df2)
}     

library(forecast)

SeasonalityTest <- function(input, ppy){
  #Used to determine whether a time series is seasonal
  tcrit <- 1.645
  if (length(input) < 3*ppy) {
    test_seasonal <- FALSE
  } else {
    xacf <- acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit / sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )
    
    if (is.na(test_seasonal)==TRUE) {
      test_seasonal <- FALSE
    }
  }
  
  return(test_seasonal)
}


# Still working on the Holt-Winters part.