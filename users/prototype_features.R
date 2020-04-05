##################################################
#                                                #
#  Merging all datasets and Prototype Features   #  
#                                                #
##################################################

getwd()


## 00. Load packages -----
library(tidyverse)
library(tidyselect)
library(RcppRoll)
library(foreach)
library(doSNOW)
library(tictoc)


## 01. Import data ----

sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F)

calendar <- read.csv("data/raw/calendar.csv", na.strings = c("", "_"), stringsAsFactors = F)
calendar$date <- as.Date(calendar$date)

prices <- read.csv("data/raw/sell_prices.csv", stringsAsFactors = F)


## 02. Check data -----
dim(sales)
dim(price)
dim(calendar)

View(head(sales, 100))
View(head(prices, 100))
View(head(calendar, 100))


## 03. Make Features in prices -----
head(prices)
str(prices)

prices_wide <- pivot_wider(data = prices, names_from = store_id, values_from =  sell_price)
View(prices_wide)

prices_wide <- prices_wide %>% mutate(
  
  # Diff from mean sale prices of other stores
  CA_1_diff_from_mean = CA_1 - rowMeans(select(., CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
  CA_2_diff_from_mean = CA_2 - rowMeans(select(., CA_1, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
  CA_3_diff_from_mean = CA_3 - rowMeans(select(., CA_1, CA_2, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
  CA_4_diff_from_mean = CA_4 - rowMeans(select(., CA_1, CA_2, CA_3, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
  TX_1_diff_from_mean = TX_1 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
  TX_2_diff_from_mean = TX_2 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_3, WI_1, WI_2, WI_3), na.rm = T),
  TX_3_diff_from_mean = TX_3 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, WI_1, WI_2, WI_3), na.rm = T),
  WI_1_diff_from_mean = WI_1 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_2, WI_3), na.rm = T),
  WI_2_diff_from_mean = WI_2 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_3), na.rm = T),
  WI_3_diff_from_mean = WI_3 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2), na.rm = T),
  
  # Has the best price
  min_price = pmap(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), min, na.rm = T),
  max_price = pmap(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), max, na.rm = T),
  
  CA_1_best_price = CA_1 == min_price,
  CA_2_best_price = CA_2 == min_price,
  CA_3_best_price = CA_3 == min_price,
  CA_4_best_price = CA_4 == min_price,
  TX_1_best_price = TX_1 == min_price,
  TX_2_best_price = TX_2 == min_price,
  TX_3_best_price = TX_3 == min_price,
  WI_1_best_price = WI_1 == min_price,
  WI_2_best_price = WI_2 == min_price,
  WI_3_best_price = WI_3 == min_price
  
)

View(prices_wide)


## 04. Make Features in calendar -----

# Keep only the number of day
calendar$d <- as.integer(substring(calendar$d, 3))

## Sesonal features 

# Day
u = 2*pi/7
calendar <- calendar %>% mutate(theta_wday = wday * u)
calendar <- calendar %>% mutate(sin_wday = sin(theta_wday), cos_wday = cos(theta_wday))
calendar$theta_wday <- NULL

# Month
v = 2*pi/12  
calendar <- calendar %>% mutate(theta_month = month * v)
calendar <- calendar %>% mutate(sin_month = sin(theta_month), cos_month = cos(theta_month))
calendar$theta_month <- NULL

# Season
yq <- zoo::as.yearqtr(x = calendar$date)
calendar$season <- as.numeric(format(yq, "%q"))

w = 2*pi/4 
calendar <- calendar %>% mutate(theta_season = season * w)
calendar <- calendar %>% mutate(sin_season = sin(theta_season), cos_season = cos(theta_season))
calendar$theta_season <- NULL

head(calendar)


## Events
calendar$event_name_1 %>% table()
calendar$event_name_2 %>% table()
# calendar$event_type_1 %>% table()
# calendar$event_type_2 %>% table()

# Days with 2 events
# I can double these examples, to add them in training data both with event_name_1 and event_type_2
# calendar %>% filter(event_name_1 != "", event_name_2 !="") %>% select(contains("event"))

# NBAFinals start - NBAFinals end (How many days)
nba_start <- which(calendar$event_name_1 == "NBAFinalsStart")
nba_end <-  which(calendar$event_name_1 == "NBAFinalsEnd")
nba_duration <- nba_end - nba_start
nba_duration

for(i in 1:length(nba_start)){
  calendar$event_name_1[nba_start[i]:nba_end[i]] <- "NBAFinals"
}

# NewYearsEve
new_year <- which(calendar$event_name_1 == "NewYear")
new_year

# calendar$event_name_1[new_year-1] <- "NewYearsEve"

for(i in 1:length(new_year)){
  calendar$event_name_1[(new_year[i]-2):(new_year[i]-1)] <- "new_years_eve"
}

# Christmas
christmas <- which(calendar$event_name_1 == "Christmas")
christmas

for(i in 1:length(christmas)){
  calendar$event_name_1[(christmas[i]-5):(christmas[i]-1)] <- "christmas"
}


#  IndependenceDay 
independeceday <- which(calendar$event_name_1 == "IndependenceDay")
independeceday

for(i in 1:length(independeceday)){
  calendar$event_name_1[(independeceday[i]-2):(independeceday[i]-1)] <- "independeceday_before"
}


# Thanksgiving 
thanksgiving <- which(calendar$event_name_1 == "Thanksgiving")
thanksgiving

for(i in 1:length(thanksgiving)){
  calendar$event_name_1[(thanksgiving[i]-2):(thanksgiving[i]-1)] <- "thanksgiving_before"
}

# SuperBowl
superbowl <- which(calendar$event_name_1 == "SuperBowl")
superbowl

for(i in 1:length(superbowl)){
  calendar$event_name_1[(superbowl[i]-2):(superbowl[i]-1)] <- "superbowl_before"
}

# Encode events as integers
calendar$event_name <- as.integer(as.factor(calendar$event_name_1))
calendar$event_type <- as.integer(as.factor(calendar$event_type_1))

calendar$event_name[is.na(calendar$event_name)] <- 0 
calendar$event_type[is.na(calendar$event_type)] <- 0 

table(calendar$event_name)
table(calendar$event_type)

# Drop columns
calendar$date <- NULL
calendar$weekday <- NULL

calendar$event_name_1 <- NULL
calendar$event_type_1 <- NULL
calendar$event_name_2 <- NULL
calendar$event_type_2 <- NULL

head(calendar)


## 05. Prepare sales ----

# Add validation rows
empty_dt = matrix(NA_integer_, ncol = 2 * 28, nrow = 1, dimnames = list(NULL, paste("d", 1913 + 1:(2 * 28), sep = "_")))
sales <- cbind(sales, empty_dt)
View(sales)

# Remove _validation from id name
sales$id <- gsub(pattern = "_validation",replacement = "", x = sales$id)


## 06. Merge dataset for one item (Function) -----

create_training_data <- function(item_id_char, store_id_char, sales, calendar, prices){
  
  
  ## Test Select one item_id for one store id
  # item_id_char <- "HOUSEHOLD_1_165"
  # store_id_char <- "CA_1"
  
  cat("\r", "Store_id:", store_id_char, "| Item_id:", item_id_char)
  
  sales_temp <- sales[sales$item_id== item_id_char & sales$store_id == store_id_char, ]
  
  # 1. Sales data  (reshape)
  sales_long <- pivot_longer(data = sales_temp, cols = d_1:d_1969)
  sales_long$name <- as.integer(substring(sales_long$name, 3))
  
  # 2. Calendar (merge)
  sales_long <- merge(x = sales_long, calendar, by.x = "name", by.y = "d")
  
  # 3. Prices (select aprropriate columns and merge)
  item_prices <- prices_wide %>%
    select(item_id, wm_yr_wk, !!(store_id_char), CA_1_best_price, CA_1_diff_from_mean) %>%
    filter(item_id == !!(item_id_char)) %>% na.omit()
  
  sales_long <- merge(x = sales_long, item_prices, by.x = c("item_id", "wm_yr_wk"), by.y = c("item_id", "wm_yr_wk"), all.y = T)
  
  
  # Rename columns
  sales_long <- sales_long %>% rename(sell_price = !!(store_id_char))
  colnames(sales_long)[(ncol(sales_long)-1):ncol(sales_long)] <- c("best_price", "diff_from_mean")
  
  # Drop features
  sales_long$wm_yr_wk <- NULL
  
  # Rearrange features
  sales_long <- sales_long %>%select(id, item_id, dept_id, cat_id, store_id, state_id, cat_id,
                                     name,  wday, sin_wday, cos_wday, month, sin_month, cos_month, season, sin_season, cos_season,
                                     year, event_name, event_type, snap_CA, snap_TX, snap_WI, sell_price, best_price,
                                     diff_from_mean, value)
  
  # View(sales_long)
  
  
  # 06. Prototype Features
  
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
  sales_long$rolling_mean_t7 = roll_meanr(sales_long$lag_t28, 7)
  sales_long$rolling_mean_t14 = roll_meanr(sales_long$lag_t28, 14)
  sales_long$rolling_mean_t30 = roll_meanr(sales_long$lag_t28, 30)
  sales_long$rolling_mean_t60 = roll_meanr(sales_long$lag_t28, 60)
  sales_long$rolling_mean_t90 = roll_meanr(sales_long$lag_t28, 90)
  sales_long$rolling_mean_t120 = roll_meanr(sales_long$lag_t28, 120)
  sales_long$rolling_mean_t150 = roll_meanr(sales_long$lag_t28, 150)
  sales_long$rolling_mean_t180 = roll_meanr(sales_long$lag_t28, 180)
  
  # Rolling sds
  sales_long$rolling_sd_t7 = roll_sdr(sales_long$lag_t28, 7)
  sales_long$rolling_sd_t28 = roll_sdr(sales_long$lag_t28, 7)
  sales_long$rolling_sd_t35 = roll_sdr(sales_long$lag_t28, 7)
  sales_long$rolling_sd_t120 = roll_sdr(sales_long$lag_t28, 7)
  sales_long$rolling_sd_t180 = roll_sdr(sales_long$lag_t28, 7)
  
  
  ## // Prices //
  sales_long$sell_price_diff = sales_long$sell_price - lag(sales_long$sell_price, 7)
  sales_long$sell_price_rel_diff = sales_long$sell_price / lag(sales_long$sell_price, 7) - 1
  sales_long$sell_price_cumrel = (sales_long$sell_price - cummin(sales_long$sell_price)) / (1 + cummax(sales_long$sell_price) - cummin(sales_long$sell_price))
  sales_long$sell_price_roll_sd7 = roll_sdr(sales_long$sell_price, n = 7)
  
  # Drop NAs
  sales_long <- sales_long %>% filter(!is.na(rolling_mean_t180))
  
  # dim(sales_long)
  # View(sales_long)
  # format(object.size(sales_long)*35000, units = "auto")
  
  return(sales_long)
  
}

# Test
# create_training_data(item_id = "FOODS_1_001", store_id = "CA_1", sales = sales, calendar = calendar, prices = prices)
# create_training_data(item_id = "FOODS_1_001", store_id = "CA_2", sales = sales, calendar = calendar, prices = prices)


## 06. Create training data -----

stat_total <- readRDS("kitematic/data/processed/stat_total.rds")
sales <- read.csv("kitematic/data/raw/sales_train_validation.csv")

pairs <- stat_total %>% select(item_id, store_id)
dim(pairs)
head(pairs)


# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 8, type = "SOCK", outfile = ""))

tic()

train_data <- foreach(i = 1:nrow(pairs), 
                      .combine = 'rbind', 
                      .packages = c("zoo","tidyverse","RcppRoll")) %dopar%
  create_training_data(item_id_char = pairs[i,"item_id"], store_id_char = pairs[i, "store_id"], sales = sales, calendar = calendar, prices = prices)

toc() # ~ 9 hours

# View(frc_total)

saveRDS(object = train_data, file = "kitematic/data/train_data.rds")


## 07. Create train, validation and test datasets ----

train <- readRDS("kitematic/data/train_data.rds")

# Separate submission data and reconstruct id columns
test <- train[d >= 1914]
test <- data.table(test)

test[, id := paste(id, ifelse(d <= 1941, "validation", "evaluation"), sep = "_")]
test[, F := paste0("F", d - 1913 - 28 * (d > 1941))]


# Keep 1 month of validation data
flag <- train$d < 1914 & train$d >= 1914 - 28
valid <- lgb.Dataset(data.matrix(train[flag, x, with = FALSE]), 
                     label = train[["demand"]][flag])


# Final preparation of training data
flag <- train$d < 1914 - 28
y <- train[["demand"]][flag]
train <- data.matrix(train[flag, x, with = FALSE])
igc()
train <- lgb.Dataset(train, label = y)
igc()


## 05. Machine Learning training -----

# It's time for LightGBM fun. Parameters are lent from Very fst Model notebook.

# Covariables used
x <- c("wday", "month", "year", 
       "event_name_1", "event_type_1", #"event_name_2", "event_type_2", 
       "snap_CA", "snap_TX", "snap_WI",
       "sell_price", "sell_price_rel_diff", "sell_price_cumrel", "sell_price_roll_sd7",
       "lag_t28", "rolling_mean_t7", "rolling_mean_t30", "rolling_mean_t60", 
       "rolling_mean_t90", "rolling_mean_t180", "rolling_sd_t7", "rolling_sd_t30",
       "item_id", "dept_id", "cat_id", "store_id", "state_id")

features <- c ("dept_id", "cat_id", "store_id", "state_id",
               "wday","sin_wday", "cos_wday", "month", "sin_month", "cos_month", "season", "sin_season", "cos_season", "year",
               "event_name", "event_type", 
               "snap_CA", "snap_TX", "snap_WI",
               "sell_price", "best_price", "diff_from_mean", "sell_price_diff", "sell_price_rel_diff", "sell_price_cumrel", "sell_price_roll_sd7",
               "mean_lag", "lag_t7", "lag_t14", "lag_t21", "lag_t21", "lag_t28", "lag_t35",
               "rolling_mean_t7", "rolling_mean_t14", "rolling_mean_t30", "rolling_mean_t60", "rolling_mean_t90", 
               "rolling_mean_t120", "rolling_mean_t150", "rolling_mean_t180",
               "rolling_sd_t7", "rolling_sd_t28", "rolling_sd_t35", "rolling_sd_t120","rolling_sd_t180", "value")

# Parameter choice
params = list(objective = "poisson",
              metric = "rmse",
              seed = 20,
              learning_rate = 0.08,
              lambda = 0.1,
              num_leaves = 63,
              bagging_fraction = 0.66,
              bagging_freq = 1, 
              colsample_bytree = 0.77)

# Training
fit <- lgb.train(params = params, data = train, num_boost_round = 2000, 
                 eval_freq = 100, early_stopping_rounds = 400, 
                 valids = list(valid = valid))

# Importance
imp <- lgb.importance(fit)
lgb.plot.importance(imp, top_n = Inf)



## 06. Create Submission file -----

pred <- predict(fit, data.matrix(test[, x, with = FALSE]))
test[, demand := pmax(0, pred)]

test_long <- dcast(test, id ~ F, value.var = "demand")

submission <- merge(sample_submission[, .(id)], 
                    test_long[, colnames(sample_submission), with = FALSE], 
                    by = "id")

fwrite(submission, file = "submission.csv", row.names = FALSE)


