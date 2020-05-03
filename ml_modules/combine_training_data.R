##################################
#                                #
# Combining training datasets    #
#                                #
##################################

## 00. Load packages

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(RcppRoll)
library(data.table)
library(lightgbm)


## 01. Import preprocessd data
source("ml_modules/fe_calendar.R")
source("ml_modules/fe_prices.R")
source("ml_modules/fe_sales.R")


fh <- 28 
max_lags <- 366
tr_last <- 1913
fday <- as.IDate("2016-04-25") 


## Function create_dt(): Import and merge datasets

create_dt <- function(is_train = TRUE, nrows = Inf, fh, max_lags, tr_last, fday) {
  
  ## Test
  # is_train = T
  # nrows = Inf
  
  
  ## Sales dataset
  
  if (is_train) { # Complete sales train validation
    
    sales <- fread("data/raw/sales_train_validation.csv", nrows = nrows)
    
  } else {
    sales <- fread("data/raw/sales_train_validation.csv",
                nrows = nrows,
                drop = paste0("d_", 1:(tr_last-max_lags)))
    
    sales[, paste0("d_", (tr_last+1):(tr_last+2*fh)) := NA_real_]
  }
  
  
  sales <- melt(dt,
             measure.vars = patterns("^d_"),
             variable.name = "d",
             value.name = "sales")
  
  ## Calendar dataset
  cal <- fread("data/raw/calendar.csv")
  
  # merge
  dt <- dt[cal, `:=`(date = as.IDate(i.date, format="%Y-%m-%d"), 
                     wm_yr_wk = i.wm_yr_wk,
                     event_name_1 = i.event_name_1,
                     event_name_2 = i.event_name_2,
                     snap_CA = i.snap_CA,
                     snap_TX = i.snap_TX,
                     snap_WI = i.snap_WI), on = "d"]
  
  ## Prices
  prices <- fread("data/raw/sell_prices.csv")
  
  # merge
  dt[prices, sell_price := i.sell_price, on = c("store_id", "item_id", "wm_yr_wk")]
}


# Function create_fea(): create features
create_fea <- function(dt) {
  
  # Drop d, wm_yr_wk
  dt[, `:=`(d = NULL,
            wm_yr_wk = NULL)]
  
  
  ## Sales Features
  
  # Lags
  lag <- c(7, 28)
  lag_cols <- paste0("lag_", lag)
  dt[, (lag_cols) := shift(.SD, lag), by = id, .SDcols = "sales"]
  
  # Window features
  win <- c(7, 28)
  roll_cols <- paste0("rmean_", t(outer(lag, win, paste, sep="_")))
  dt[, (roll_cols) := frollmean(.SD, win, na.rm = TRUE), by = id, .SDcols = lag_cols]   
  
  ## Transform characters to integers
  cols <- c("item_id", "store_id", "state_id", "dept_id", "cat_id", "event_name_1", "event_name_2")   
  dt[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols]
  
  
  ## Calendar feature
  dt[, `:=`(wday = wday(date),
            mday = mday(date),
            week = week(date),
            month = month(date),
            quarter = quarter(date),
            year = year(date))]
  
}







# calendar <- create_calendar_data()
calendar <- readRDS("data/raw/calendar_full.rds")

# prices <- create_prices_data()
prices <- readRDS("data/raw/prices_full.rds")

sales <- create_sales_data()

View(calendar)
View(prices)
View(sales)

dim(calendar)  # 1969     55
dim(prices)    # 7125900  12
dim(sales)     # 53723380 27


## 02. Merge all datasets -----

# Merge calendar to sales
sales <- calendar[sales, on = "d"]
rm(calendar)
gc()

# Merge selling prices to sales and drop key
sales <- prices[sales, on = c('store_id', 'item_id', 'wm_yr_wk')][, wm_yr_wk := NULL]
rm(prices)
gc()


## Rename dataset
train <- sales  
dim(train)      # 40150972       88
rm(sales)
gc()



# 03. Features

# Turn non-numerics to integers
cols <- c("store_id", "item_id", "dept_id", "cat_id", "state_id")
train[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]

# Extra column snap
train[, snap := ifelse(state_id == "CA", snap_CA, ifelse(state_id == "TX", snap_TX, snap_WI))]


## 04. Save train dataset and remove other datasets

head(train)
format(object.size(train), unit = "auto") # "21.1 Gb"
saveRDS(object = train, file = "../kitematic/temp/train.rds")

# train <- readRDS("../kitematic/temp/train.rds")





