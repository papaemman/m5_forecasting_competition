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
library(dplyr)
library(plotly)
library(smooth)

## 01. Import data ----
sales_raw <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F) 

calendar <- read.csv("data/raw/calendar.csv", stringsAsFactors = F, na.strings = "")
calendar$date <- as.Date(calendar$date)


## Keep the last nb_days from sales
# nb_days <- 365
nb_days <- ncol(sales_raw)-6

sales <- sales_raw[, c(1, (1919-nb_days+1):1919)]
dim(sales)

# View(head(sales))
rm(sales_raw)

## Drop extra day in leap year (2016)
calendar <- calendar %>% dplyr::filter(!date == as.Date("2016-02-29"))
sales$d_1858 <- NULL
nb_days <- nb_days -1


## 02. Define bts (bottom time series) matrix ----

# - bts is a time series matrix containing the bottom-level series
# - each column corresponds to a different time series (sales for item_id - store_id)
# - each row corresponds to a different day

sales_long <- t(sales[,-1])
colnames(sales_long) <- sales[,1]


calendar_short <- calendar %>% filter(d %in% rownames(sales_long))

bts <- ts(data = sales_long,
          start =  c(as.numeric(format(calendar_short[1,"date"], "%Y")), as.numeric(format(calendar_short[1,"date"], "%j"))),
          frequency = 365)
 
dim(bts)

# nrow : number of days
# ncol : number of bottom level timeseries


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

## Note: Be carefull fro the leap year!
# y
# 
# head(sales_long[,1])
# calendar %>% dplyr::filter(d == "d_1")
# format(as.Date("2011-01-29"), "%j") # 029  #
# 
# tail(sales_long[,1])
# calendar %>% dplyr::filter(d == "d_1913")
# format(as.Date("2016-04-24"), "%j") # 115  #



## 03. External regressors ----

str(y)

y$labels$G1
y$labels$G10

## Check data
aggts(y, levels = 1)
aggts(y, levels = 2)
aggts(y, levels = c(3,4))
aggts(y, levels = "G3")

# Plot data
plot(aggts(y, levels = 2))
plot(aggts(y, levels = "G1"))


## Get external regressors
x_var <- calendar_short
x_var$snap <- x_var$snap_CA+x_var$snap_WI+x_var$snap_TX
x_var$holiday <- 0
x_var[!is.na(x_var$event_type_1),]$holiday <- 1
x_var <- x_var[,c("snap","holiday")]


## 04. Test forecasting methods -----

## 1. auto.arima
model <- auto.arima(y = aggts(y, levels = 0), xreg = as.matrix(head(x_var, nb_days)))
model <- auto.arima(y = aggts(y, levels = 5)[,3], xreg = as.matrix(head(x_var, nb_days)))

frc <- forecast(model, h = 28, xreg = as.matrix(tail(x_var, 28)))$mean

# frc <- forecast(model, h = 28, xreg = as.matrix(tail(x_var, 28)))$lower[,2]
# frc <- forecast(model, h = 28, xreg = as.matrix(tail(x_var, 28)))$upper[,1]

## 2. smooth::es (wrong model)
model <- es(y = aggts(y, levels = 0), xreg = as.matrix(head(x_var, nb_days)))
frc <- forecast(model, h = 28, xreg = as.matrix(tail(x_var, 28)))$mean

## 3. prophet



## Plot forecastings
df <- data.frame(index = 1:(nb_days+28),
                 demand = c(aggts(y, levels = 0), rep(NA,28)),
                 frc = c(rep(NA, nb_days), frc))

p <- ggplot(df) +
  geom_line(aes(index, demand)) + geom_point(aes(index, demand), size = 0.8) + 
  geom_line(aes(index, frc), col = "red", linetype = 4) + geom_point(aes(index, frc), col = "red", size = 0.8) + 
  ggtitle("Past sales and forecastings")

ggplotly(p)


## 05. Make forecastings ----

# https://rdrr.io/cran/hts/man/forecast.gts.html

?forecast
# Description: forecast is a generic function for forecasting from time series or time series models.
# The function invokes particular methods which depend on the class of the first argument.

methods(forecast)
View(forecast.gts)
?forecast.gts


# Forecast 28-step-ahead using the optimal combination method
fcasts <- hts::forecast.gts(y,
                            h = 28,
                            method = "comb",     
                            weights = "ols",       # c("wls", "ols", "mint", "nseries")
                            fmethod = "arima",     # c("ets", "arima", "rw")
                            algorithms = "lu",     # c("lu", "cg", "chol", "recursive", "slm")
                            covariance = "shr",    # c("shr", "sam"), used only for weight = "mint"
                            keep.fitted = FALSE,
                            keep.resid = FALSE, 
                            positive = FALSE,
                            lambda = "auto",
                            FUN = NULL, 
                            xreg = as.matrix(head(x_var,nb_days)),
                            newxreg = as.matrix(tail(x_var, 28)),
                            parallel = TRUE, num.cores = 8)

saveRDS(fcasts, file = "fcasts.rds")


## Custom forecasting function

# fcasts <- hts::forecast.gts(y,
#                             h = 28,
#                             method = "comb",     
#                             weights = "ols",       # c("wls", "ols", "mint", "nseries")
#                             algorithms = "lu",     # c("lu", "cg", "chol", "recursive", "slm")
#                             covariance = "shr",    # c("shr", "sam")
#                             keep.fitted = FALSE,
#                             keep.resid = FALSE, 
#                             positive = FALSE,
#                             lambda = "auto",
#                             FUN = function(x) tbats(x, use.parallel = FALSE),
#                             parallel = TRUE, num.cores = 8)


# plot the forecasts including the last 56 historical observations
plot(fcasts, include = 56, levels = c(1,2))


## 06. Create submission file  ----

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

## 07. Forecastings Optimal combination  ----

y
h <- 28
ally <- aggts(y)
allf <- matrix(NA, nrow = h, ncol = ncol(ally))


# Make forecastings

for(i in 1:ncol(ally)){
  
  # i = 1
  cat(i, "\r")
  
  model <- auto.arima(y = ally[,i],
                      allowdrift = T,
                      allowmean = T,
                      lambda = "auto", 
                      xreg = as.matrix(head(x_var, nb_days)),
                      parallel = T, num.cores = 8)
  
  allf[,i] <- forecast(model, h = h, xreg = as.matrix(tail(x_var, 28)))$mean

}
  
allf <- ts(allf, start =  c(2016, 116), frequency = 365)


weigths <- read.csv("data/raw/weights_validation.csv")
weights_vec <- weigths$Weight
  
y.f <- combinef(allf,
                groups = get_groups(y),
                weights = NULL ,         #  NULL or weights_vec
                keep ="bottom",          # c("gts", "all", "bottom")
                algorithms = "lu"        # c("lu", "cg", "chol", "recursive", "slm")
                )

str(y.f)
plot(y.f)



