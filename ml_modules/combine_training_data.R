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

calendar <- create_calendar_data()
prices <- create_prices_data()
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





