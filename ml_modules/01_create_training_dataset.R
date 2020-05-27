##################################
#                                #
#  Modeling / Training           #
#                                #
##################################

# Note: Features from calendar and Features from prices are already created and saved.
#       Check scirpts in ml_modules/feature_engineering/fe_calandar - fe_prices to see how they are created.


## 00. Load packages ----

library(tidyverse)
library(data.table)
library(RcppRoll)
library(tictoc)


## Source dependencies
source("ml_modules/feature_engineering/create_data.R")
source("ml_modules/feature_engineering/fe_sales.R")


## Define parameters
nrows = Inf                        # How many sales data to import
fh = 28                            # Forecasting horizon
max_lags = 366                     # Max lags (features)
tr_last = 1913                     # Training last day
fday = as.IDate("2016-04-25")      # Forecasting day (first day)



## 01. Create training dataset ----

# dt <- create_dt(is_train = TRUE, nrows = nrows,
#                 fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday,
#                 filter_params = "dept_id == 'HOBBIES_1' ")

# 1. Load and merge datasets (96 features | 46.027.957 rows)
dt <- create_dt(is_train = TRUE, nrows = nrows,
                fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday,
                filter_params = NA)
gc()


# 2. Create hierarhy sales features (107 features | 46.027.957 rows)
dt <- create_hierarhy_sales_features(dt)
gc()

# 3. Preprocessing steps and add more features (143 features)
dt <- add_more_features(dt = dt)
gc()


# 4. Add categorical encoding features (181 features | 46.027.957 rows)
dt <- create_categorical_encoding_features(dt)


# 5. Create Sales features (232 features, 46.027.957 rows) 245
dt <- create_sales_features(dt)
gc()


## 02. Save train dataset ----

# Sanity checks
colnames(dt)
str(dt)

sales_raw <- fread("data/raw/sales_train_validation.csv")
View(sales_raw)
sales_raw %>% filter(item_id == "FOODS_3_123") %>% View()

View(dt[item_id == "FOODS_1_001" & store_id == 'CA_1', c("d", "sales", "Max", "lag_t1", "lag_t2")])
View(dt[item_id == "HOBBIES_1_001" & store_id == 'CA_1', c("d", "sales", "Max", "lag_t1", "lag_t2")])
View(dt[item_id == "HOBBIES_1_008" & store_id == 'CA_1', c("d", "sales", "Max", "lag_t1", "lag_t2", "mean_last_month","sell_price")])
View(dt[item_id == "HOUSEHOLD_1_002" & store_id == 'CA_1', c("d", "sales", "Max", "lag_t1", "lag_t2")])
View(dt[item_id == "HOBBIES_1_008" & store_id == 'CA_1', c("d", "sales", "snap","snap_CA", "snap_TX", "snap_WI")])

View(dt[item_id == "FOODS_3_123" & store_id == 'WI_3', c("d", "sales", "Max", "lag_t1", "lag_t2", "lag_t49","has_event_lead_t2")])

View(dt[item_id == "FOODS_3_123" & store_id == 'WI_3', c("d", "sales", "Max", "lag_t1", "lag_t2", "sell_price", "sell_price_diff")])

View(dt[item_id == "FOODS_3_123" & store_id == 'WI_3', c("d", "sales", "sell_price", "sell_price_diff", "sell_price_to_max_lead_1")])


# 1. Save train dataset without drop NAs

dim(dt)  # 46027957      232
dt[,.N, by = c("store_id", "item_id")] %>% nrow() #  [1] 30490

format(object.size(dt), unit = "auto") #  "67 Gb"
saveRDS(object = dt, file = "../kitematic/temp/dt_full_train.rds")
gc()

# dt <- readRDS(file = "../kitematic/temp/dt_full_train.rds")


# 2. Save train dataset after drop NAs from first days (Use the column with maximum lags)

# Check NAs
sum(is.na(dt$rolling_mean_lag28_t180))


## READ data (~ 5 mins)

# tic()
# dt <- readRDS("../kitematic/temp/dt_full_train.rds")
# toc()



