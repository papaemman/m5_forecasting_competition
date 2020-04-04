#############################
#                           #
#  Data reshape and merge   #  
#                           #
#############################

# Load packages
library(tidyverse)


# Import data
sales <- read.csv("data/raw/sales_train_validation.csv")
calendar <- read.csv("data/raw/calendar.csv")
prices <- read.csv("data/raw/sell_prices.csv")

# Check data
dim(sales)
dim(price)
dim(calendar)

View(head(sales, 100))
View(head(prices, 100))
View(head(calendar, 100))


# Select one item_id for one store id
sales <- sales[1, ]
empty_dt = matrix(NA_integer_, ncol = 2 * 28, nrow = 1, dimnames = list(NULL, paste("d", 1913 + 1:(2 * 28), sep = "_")))
sales <- cbind(sales, empty_dt)

# Reshape sales data 
sales_long <- pivot_longer(data = sales, cols = d_1:d_1969)

dim(sales_long)
View(sales_long)

# Merge calendar
sales_long <- merge(x = sales_long, calendar, by.x = "name", by.y = "d")

# Merge prices
sales_long <- merge(x = sales_long, prices, by.x = c("item_id", "store_id", "wm_yr_wk"), by.y = c("item_id", "store_id", "wm_yr_wk"))



sales_long <- sales_long %>% select(id, item_id, dept_id, cat_id, store_id, state_id, cat_id,
                                    date, wm_yr_wk, weekday, wday, month, year, event_name_1, event_type_1, event_name_2, event_type_2,
                                    snap_CA, snap_TX, snap_WI, name,
                                    sell_price, value) %>% arrange(date)

View(sales_long)
  



## Features prototype

## // Sales //

# Lags
sales_long$lag_t7 <- lag(sales_long$value, 7)
sales_long$lag_t14 <- lag(sales_long$value, 14)
sales_long$lag_t21 <- lag(sales_long$value, 21)
sales_long$lag_t28 <- lag(sales_long$value, 28)
sales_long$lag_t35 <- lag(sales_long$value, 35)

# Mean of the last 4 same days
sales_long <- sales_long %>% mutate( mean_lag = (lag_t7 + lag_t14 + lag_t21 + lag_t28)/4 ) 

# Rolling mean
library(RcppRoll)

sales_long$rolling_mean_t7 = roll_meanr(sales_long$lag_t28, 7)
sales_long$rolling_mean_t14 = roll_meanr(sales_long$lag_t28, 14)
sales_long$rolling_mean_t30 = roll_meanr(sales_long$lag_t28, 30)
sales_long$rolling_mean_t60 = roll_meanr(sales_long$lag_t28, 60)
sales_long$rolling_mean_t90 = roll_meanr(sales_long$lag_t28, 90)
sales_long$rolling_mean_t120 = roll_meanr(sales_long$lag_t28, 120)
sales_long$rolling_mean_t150 = roll_meanr(sales_long$lag_t28, 150)
sales_long$rolling_mean_t180 = roll_meanr(sales_long$lag_t28, 180)

# Rolling sds
rolling_sd_t7 = roll_sdr(lag_t28, 7)
rolling_sd_t7 = roll_sdr(lag_t28, 7)
rolling_sd_t7 = roll_sdr(lag_t28, 7)
rolling_sd_t7 = roll_sdr(lag_t28, 7)
rolling_sd_t7 = roll_sdr(lag_t28, 7)


## // Prices //

sales_long$sell_price_diff = sales_long$sell_price - lag(sales_long$sell_price, 7)
sales_long$sell_price_rel_diff = sales_long$sell_price / lag(sales_long$sell_price, 7) - 1
sales_long$sell_price_cumrel = (sales_long$sell_price - cummin(sales_long$sell_price)) / (1 + cummax(sales_long$sell_price) - cummin(sales_long$sell_price))

sell_price_roll_sd7 = roll_sdr(sales_long$sell_price, n = 7)

sales_long %>% select(contains("price"), value) %>% View()


## // Calendar //

# Drop columns
sales_long$wm_yr_wk <- NULL
sales_long$date <- NULL
sales_long$weekday <- NULL
  
# Day
u = 2*pi/7  
sales_long <- sales_long %>% mutate(theta_wday = wday * u)
sales_long <- sales_long %>% mutate(sin_wday = sin(theta_wday), cos_wday = cos(theta_wday))

sales_long %>% select(wday, theta_wday, sin_val, cos_val) %>% View()

# Month
v = 2*pi/12  
sales_long <- sales_long %>% mutate(theta_month = month * v)
sales_long <- sales_long %>% mutate(sin_month = sin(theta_month), cos_month = cos(theta_month))


# Events
sales_long$event_name_1 %>% table()
sales_long$event_name_2 %>% table()

## // IDs //
sales_long$id <- NULL
sales_long$item_id <- NULL



# Maybe
# - Keep only relevant snap column






