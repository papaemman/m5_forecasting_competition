####################################
#                                  #
# Grouped time series forecasting  #
#                                  #
####################################

# Note:
# A grouped time series, can be thought of as hierarchical time series without a unique hierarchical structure. 
# In other words, the order by which the series can be grouped is not unique. 
# The sales data can be first disaggregated by State and then by Category, 
# or they can be disaggregated by Category first, and then by States, so it falls in grouped time series category.


## 00. Load packages ----
library(hts)
library(ggplot2)
library(plotly)

## 01. Import data ----
sales_raw <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F) 
calendar <- read.csv("data/raw/calendar.csv", stringsAsFactors = F, na.strings = "")

## Keep the last nb_days from sales
# nb_days <- 365*2
nb_days <- ncol(sales_raw)-7
sales <- sales_raw[, c(1, (1919-nb_days+1):1919)]
dim(sales)
# View(sales)

rm(sales_raw)

## 02. Define bts (bottom time series) matrix ----

# - bts is a time series matrix containing the bottom-level series
# - each column corresponds to a different time series (sales for item_id - store_id)
# - each row corresponds to a different day

sales_long <- t(sales[,-1])
colnames(sales_long) <- sales[,1]
bts <- ts(data = sales_long, start = 1, end = nb_days, frequency = 1)

dim(bts)
# [1]   100 30490
# 100 days (rows), for 30490 bottom level time series (columns)
bts[1:5, 1:10]



## Define groups and hierarhies

head(sales$id)

# 1.  Total
# 2.  State
# 3.  Store
# 4.  Category
# 5.  Department
# 6.  State - Category
# 7.  State - Department
# 8.  Store - Category
# 9.  Store - Department
# 10. Product
# 11. Product - State
# 12. Product - Store

state_id <- sapply(strsplit(sales$id, split = "_"), function(x){x[4]})
store_id <- sapply(strsplit(sales$id, split = "_"), function(x){paste(x[4], x[5], sep = "_")})
cat_id <- sapply(strsplit(sales$id, split = "_"), function(x){x[1]})
dept_id <- sapply(strsplit(sales$id, split = "_"), function(x){paste(x[1], x[2], sep = "_")})
state_cat_id <- paste(state_id, cat_id, sep = "_")
state_dept_id <- paste(state_id, dept_id, sep = "_")
store_cat_id <- paste(store_id, cat_id, sep = "_")
store_dept_id <- paste(store_id, dept_id, sep = "_")
item_id <- sapply(strsplit(sales$id, split = "_"), function(x){paste(x[1], x[2], x[3], sep = "_")})
item_state_id <- paste(item_id, state_id, sep = "_")


groups_names <- matrix(data = c(state_id, store_id, cat_id, dept_id, state_cat_id, state_dept_id, store_cat_id, store_dept_id,
                                item_id, item_state_id),
                       ncol = 30490, byrow = TRUE)

groups_names[, 1:6]

dim(bts)

y <- gts(bts, groups = groups_names)

y

## Grouped Time Series 
# 12 Levels 
# Number of groups at each level: 1 3 10 3 7 9 21 30 70 3049 9147 30490 
# Total number of series: 42840 
# Number of observations per series: 100 
# Top level series: 
#   Time Series:
#   Start = 1 
# End = 100 
# Frequency = 1 
# [1] 41790 51146 45533 38340 31988 32684 31779 36307 48927 47832 34389 30446 31349 32466 37639 49104 50831 41714 34928 39340 35424 40193
# [23] 55040 46893 36804 38513 39353 44584 44222 53715 48337 46152 37702 36545 36436 40392 50857 52081 37328 33981 34068 34615 37649 47887
# [45] 50915 37259 36536 38458 41459 39325 52322 57218 40562 37727 37032 38267 40887 52711 51421 42035 40117 36606 35009 39652 46181 47825
# [67] 37360 35475 34786 34003 45611 53863 46360 36041 33857 32359 34681 45536 52672 56425 40418 39683 39134 38116 43220 56340 53856 42427
# [89] 39069 35193 37529 41789 48362 51640 38059 37570 35343 35033 40517 48962


# Check data
aggts(y, levels = 1)
aggts(y, levels = 2)
aggts(y, levels = c(3,4))

str(y)

y$labels$G1
y$labels$G10

aggts(y, levels = "G3")


# Plot data
plot(aggts(y, levels = 2))
plot(aggts(y, levels = "G1"))


# Get external regressors
x_var <- calendar
x_var$snap <- x_var$snap_CA+x_var$snap_WI+x_var$snap_TX
x_var$holiday <- 0
x_var[!is.na(x_var$event_type_1),]$holiday <- 1
x_var <- x_var[,c("snap","holiday")]


## Test forecasting methods

# prophet

# auto.arima
model <- auto.arima(y = aggts(y, levels = 0), xreg = as.matrix(head(x_var,nb_days)))

frc <- forecast(model, h = 28, xreg = as.matrix(tail(x_var, 28)))$mean

# frc <- forecast(model, h = 28, xreg = as.matrix(tail(x_var, 28)))$lower[,2]
# frc <- forecast(model, h = 28, xreg = as.matrix(tail(x_var, 28)))$upper[,1]

df <- data.frame(index = 1:(nb_days+28),
                 demand = c(aggts(y, levels = 0), rep(NA,28)),
                 frc = c(rep(NA, nb_days), frc))

p <- ggplot(df) +
  geom_line(aes(index, demand)) + geom_point(aes(index, demand), size = 0.8) + 
  geom_line(aes(index, frc), col = "red", linetype = 4) + geom_point(aes(index, frc), col = "red", size = 0.8) + 
  ggtitle("Past sales and forecastings")

ggplotly(p)


## 03. Make forecastings ----

# https://rdrr.io/cran/hts/man/forecast.gts.html

?forecast
# Description: forecast is a generic function for forecasting from time series or time series models.
# The function invokes particular methods which depend on the class of the first argument.

methods(forecast)
View(forecast.gts)
?forecast.gts


# Forecast 28-step-ahead using the optimal combination method
fcasts <- forecast(y,
                   h = 28,
                   method = "comb",     
                   weights = "ols",     # c("wls", "ols", "mint", "nseries")
                   fmethod = "arima",
                   algorithms = "lu",   # c("lu", "cg", "chol", "recursive", "slm")
                   covariance = "shr",  # c("shr", "sam")
                   nonnegative = TRUE,
                   keep.fitted = FALSE,
                   keep.resid = FALSE, 
                   positive = FALSE,
                   lambda = "auto",
                   xreg = as.matrix(head(x_var,nb_days)),
                   newxreg = as.matrix(tail(x_var, 28)),
                   parallel = TRUE, num.cores = 6)

saveRDS(fcasts, file = "fcasts.rds")

# plot the forecasts including the last ten historical years
plot(fcasts, include = 10, levels = c(1,2))


## 04. Create submission file  ----

fcasts
class(fcasts)

fcasts$histy

# Get forecastings
forecasts <- t(fcasts$bts)
dim(forecasts)

# Make id column and names
forecasts <- as.data.frame(forecasts)
forecasts$id <- rownames(forecasts)
rownames(forecasts) <- NULL
colnames(forecasts) <- c(paste0("F", 1:28), "id")
forecasts <- forecasts[, c("id",paste0("F", 1:28))]

# Create evaluation forecastings
forecasts_eval <- forecasts
forecasts_eval$id <- gsub(pattern = "validation", replacement = "evaluation", x = forecasts_eval$id)

# Combine forecastings
forecasts <- rbind(forecasts, forecasts_eval)

# Check if the id column has required order
sample_submission <- read.csv("data/pnt_submissions/sample_submission.csv", stringsAsFactors = F)

sum(sample_submission$id != forecasts$id)
dim(sample_submission)
dim(forecasts)

write.csv(forecasts, file = "data/pnt_submissions/experiment_hts_comb.csv",row.names = F)
