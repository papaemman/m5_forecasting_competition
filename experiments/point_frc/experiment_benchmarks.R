######################################
#                                    #
#  Experiment: M5 Benchmark models   #
#                                    #
######################################

# Source dependencies
lapply(X = paste0("modules/", list.files(path = "modules/")), FUN = source)

# Import datasets
sales <- read.csv("data/raw/sales_train_validation.csv")
calendar <- read.csv("data/raw/calendar.csv")
time_series_b <- readRDS(file = "data/processed/time_series_b.rds")


# // Methods //
# 01. Bottom-up models
# - local 
# - global 
# 02. Top - down models




## 01. Get forecasts for bottom-up models ----

# 1. Local time series (get training data just from one time series and make predicitons for this time series)

# Define benchmark names to be used from benchmarks_f() function
b_names <- c("Naive", "sNaive",
             "SES", "MA", 
             "Croston", "optCroston",
             "SBA", "TSB", 
             "ADIDA",
             "iMAPA",
             "ES_bu",
             "ARIMA_bu",
             "MLP_l",
             "RF_l")

# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 4, type = "SOCK", outfile = ""))

# Get forecasts for local models
frc_total <- foreach(tsi = 1:nrow(sales),
                     .combine = 'rbind', 
                     .packages = c('zoo','randomForest','RSNNS','forecast','smooth')) %dopar% benchmarks_f(x = time_series_b[[tsi]], fh = 28, b_names = b_names)

saveRDS(object = frc_total, file = "data/processed/frc_total_l.RDS")
frc_total <- readRDS("data/processed/frc_total_l.RDS")



# 2. Global time series forecasting (get training data from multiple time series, but make predictions for every time series in bottom level)

# Get forecasts for global models
frc_total_g <- ML_Global(fh = 28, ni = 12, nwindows = 3)


# 3. Combine forecasts from local and global models
frc_total$MLP_g <- frc_total_g$MLP_g
frc_total$RF_g <- frc_total_g$RF_g
frc_total_g <- NULL

saveRDS(object = frc_total, file = "frc_total_bu.RDS")
frc_total <- readRDS("frc_total_bu.RDS")


## 02. Get forecasts for top-down models ----

# Count the total sales for each day for all products in all stores (aggreagation level 1)
insample_top <- ts(as.numeric(colSums(sales[,7:ncol(sales)])), frequency = 7)

# Get calendar events
x_var <- calendar
x_var$snap <- x_var$snap_CA+x_var$snap_WI+x_var$snap_TX
x_var$holiday <- 0
x_var[is.na(x_var$event_type_1)==F,]$holiday <- 1
x_var <- x_var[,c("snap","holiday")]

# 1. Exponential smoothing
es_f <- smooth_es(x = insample_top, h = 28)

# 2. Exponential smoothing with external variables
esx_f <- es(insample_top, xreg=x_var, h=28)$forecast

# 3. ARIMA
arima_f <- auto_arima(x = insample_top, h = 28)

# 4. ARIMA with external variables
arimax_f <- forecast(auto.arima(insample_top,
                                xreg = as.matrix(head(x_var, length(insample_top)))),
                     h=28, xreg=as.matrix(tail(x_var, 28)))$mean 

## Note: My functions smooth_es() and auto_arima(), can't get external regressor as arguments, so I have to call the code here


## Import historical proportions data
proportions <- readRDS("data/processed/historical_proportions_level_1.rds")

# Mulpiply the forecasting values for total predictions, with historical proportions to get the forecasting for each item_id
frc_total$ES_td <- unlist(lapply(c(1:length(time_series_b)), function(x) es_f*proportions[x]))
frc_total$ESX <- unlist(lapply(c(1:length(time_series_b)), function(x) esx_f*proportions[x]))
frc_total$ARIMA_td <- unlist(lapply(c(1:length(time_series_b)), function(x) arima_f*proportions[x]))
frc_total$ARIMAX <- unlist(lapply(c(1:length(time_series_b)), function(x) arimax_f*proportions[x]))


## Ensembles: Get forecasts for combination approaches (Average  the TOP-Down + Bottom-Up approaches)
frc_total$Com_b <- (frc_total$ES_bu+frc_total$ARIMA_bu)/2
frc_total$Com_t <- (frc_total$ES_td+frc_total$ARIMA_td)/2
frc_total$Com_tb <- (frc_total$ES_bu+frc_total$ES_td)/2
frc_total$Com_lg <- (frc_total$MLP_l+frc_total$MLP_g)/2



## 3. Combine all forecasts and save them ----

# Add Top-down forecastings in frc_total (forecasting total object)

# update b_names = benchmark names vector
b_names <- c("Naive", "sNaive", "SES", "MA", 
             "Croston", "optCroston","SBA", "TSB", 
             "ADIDA", "iMAPA",
             "ES_bu", "ARIMA_bu",
             "MLP_l", "RF_l", "MLP_g", "RF_g",
             "ES_td","ESX","ARIMA_td","ARIMAX", # ARIMAX can't fit in the data (remove it before define the b_names vector )
             "Com_b","Com_t","Com_tb","Com_lg")

frc_total <- frc_total[,c(b_names,"item_id","dept_id","cat_id","store_id","state_id","fh")]

saveRDS(object = frc_total, file = "data/processed/frc_total.RDS")


