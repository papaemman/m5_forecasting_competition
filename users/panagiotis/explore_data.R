#############################
#                           #
#  Explore data (EDA)       #
#                           #
#############################

library(dplyr)

## 01. Read data/raw/   ----

# 1. Sales data
sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F)

# 2. Calendar data
calendar <- read.csv("data/raw/calendar.csv", stringsAsFactors = F)

# 3. Prices data
prices <- read.csv("data/raw/sell_prices.csv", stringsAsFactors = F)


## 02. Read data/processed/ ----

# time_series_b.rds object
time_series_b <- readRDS("data/processed/time_series_b.rds")
str(time_series_b)
length(time_series_b)

# statistics_bottom_level_series.rds object
statistics_bottom_level_series <- readRDS("data/processed/statistics_bottom_level_series.rds")

