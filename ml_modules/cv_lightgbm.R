##################################
#                                #
#  Modeling / Training           #
#                                #
##################################

## 00. Load packages

library(tidyverse)
library(data.table)
library(RcppRoll)
library(lightgbm)
library(tictoc)

## Source dependencies
source("ml_modules/create_data.R")
source("ml_modules/fe_sales.R")


## Define parameters

nrows = Inf                        # How many sales data to import
fh = 28                            # Forecasting horizon
max_lags = 366                     # Max lags (features)
tr_last = 1913                     # Training last day
fday = as.IDate("2016-04-25")      # Forecasting day (first day)


## Create (merge) datasets
dt <- create_dt(is_train = TRUE, nrows = nrows,
                fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday)
gc()


## Create hierarhy sales features
dt <- create_hierarhy_sales_features(dt)
gc()

## Preprocessing steps and add features
dt <- add_features(dt = dt)
gc()

## Create Sales features
dt <- create_sales_features(dt)
gc()


## Save train dataset and remove other datasets
dim(dt) # [1] 46027957      163
dt[,.N, by = c("store_id", "item_id")] %>% View()

format(object.size(dt), unit = "auto") #  "23 Gb"
saveRDS(object = dt, file = "../kitematic/temp/train.rds")


## Drop zero-demand periods
dt <- drop_zero_demand_periods(dt, nb_days = 60)
gc()


## Drop NAs from first days (Use the column with maximum lags)
dt <- dt[!is.na(rolling_mean_lag28_t180), ]  # faster than na.omit()
gc()
anyNA(dt)







## Select Features to be use in training

# tr = dt
# rm(dt)

library(lightgbm)
library(data.table)
library(tictoc)

tr <- readRDS("../kitematic/temp/train.rds")
# tr <- as.data.table(tr)
# gc()

tr[1:3, 1:10]
dim(tr)
str(tr)


features <- c(
  
  # "id",
  
  # "item_id",  "dept_id", "cat_id", "store_id", "state_id", 
  
  # "d", "sales",
  
  "wday", "month", "year",
  "event_name_1", "event_type_1", 
  
  # "event_name_2", "event_type_2",
  
  "snap_CA", "snap_TX", "snap_WI",   
  
  "day_month", "day_quarter", "day_year",
  "week_month", "week_quarter", "week_year", "weekend",
  "month_quarter", "quart", "semester",
  
  "wday_s1", "wday_c1", "wday_s2" , "wday_c2", "wday_s3", "wday_c3",
  "month_s1", "month_c1", "month_s2", "month_c2", "month_s3","month_c3",
  
  "wday_rbf_1", "wday_rbf_2", "wday_rbf_3", "wday_rbf_4", "wday_rbf_5", "wday_rbf_6", "wday_rbf_7",
  
  "month_rbf_1", "month_rbf_2", "month_rbf_3", "month_rbf_4", "month_rbf_5", "month_rbf_6", "month_rbf_7" ,"month_rbf_8", "month_rbf_9",
  "month_rbf_10", "month_rbf_11", "month_rbf_12",
  
  "sporting_event", "cultural_event", "national_event", "religious_event",
  
  "has_event", "has_event_lead_t1", "has_event_lead_t2", "has_event_lead_t3", "week_events",
  
  "sporting_event_lead_1" , "cultural_event_lead_1" , "national_event_lead_1", "religious_event_lead_1", 
  "sporting_event_lead_3",  "cultural_event_lead_3" , "national_event_lead_3",   "religious_event_lead_3",
  
  
  "sell_price", "sell_price_diff", "sell_price_rel_diff", "sell_price_cumrel", "sell_price_roll_sd7",
  
  # "nb_stores", 
  
  # "unique_sell_prices", "sell_price_changes", "mean_sell_price", "min_sell_price", "max_sell_price", "max_diff",
  
  "sell_price_to_mean", "sell_price_to_min", "sell_price_to_max", "sell_price_diff_to_max_diff",
  
  "diff_from_mean_price", "rel_diff_from_mean_price", "best_price",
  
  "sell_price_to_min_lead_1", "sell_price_to_min_lead_2", "sell_price_to_max_lead_1", "sell_price_to_max_lead_2",
  
  
  "total_sales", "total_state_sales", "total_store_sales", "total_cat_sales", "total_dept_sales",           
  "total_state_cat_sales", "total_state_dept_sales", "total_store_cat_sales", "total_store_dept_sales",   
  "total_item_sales", "total_item_state_sales",
  
  "snap",
  
  "ADI", "CV2", "pz","Type", "Low25", "Mean", "Median", "Up25", "Max",
  
  "lag_t1", "lag_t2", "lag_t3", "lag_t4", "lag_t5", "lag_t6", "lag_t7", "lag_t8",
  "lag_t14", "lag_t21", "lag_t28", "lag_t35", "lag_t42", "lag_t49",
  
  "mean_last_month", "mean_last_2_monts", "mean_previous_month",
  
  "rolling_mean_lag7_t7", "rolling_mean_lag7_t30", "rolling_mean_lag7_t60" ,
  "rolling_mean_lag7_t90", "rolling_mean_lag7_t120", "rolling_mean_lag7_t180", 
  
  "rolling_sd_lag7_t7", "rolling_sd_lag7_t30", 
  "rolling_sum_lag7_t7","rolling_min_lag7_t7",
  
  "rolling_min_lag7_t30", "rolling_max_lag7_t7",
  
  "rolling_max_lag7_t30", "rolling_mean_lag28_t7",
  
  "rolling_mean_lag28_t30", "rolling_mean_lag28_t60", "rolling_mean_lag28_t90", "rolling_mean_lag28_t120", "rolling_mean_lag28_t180" ,   
  "rolling_sd_lag28_t7", "rolling_sd_lag28_t30", 
  "rolling_sum_lag28_t7", 
  
  "rolling_min_lag28_t7", "rolling_min_lag28_t30",
  "rolling_max_lag28_t7", "rolling_max_lag28_t30"
  
  # "orig", "rleLength" 
  )



setdiff(features, colnames(tr))
setdiff(colnames(tr), features)
# Don't include: [1] "id"        "d"         "sales"     "date"      "rleLength"



# Which of these features should be treaten as categorical

categoricals <- c(
  
  "wday", "month", "year",
  "event_name_1", "event_type_1", 

  "snap_CA", "snap_TX", "snap_WI",   
  
  "day_month", "day_quarter", "day_year",
  "week_month", "week_quarter", "week_year", "weekend",
  "month_quarter", "quart", "semester",
  
  "sporting_event", "cultural_event", "national_event", "religious_event",
  
  "has_event", "has_event_lead_t1", "has_event_lead_t2", "has_event_lead_t3", "week_events",
  
  "sporting_event_lead_1" , "cultural_event_lead_1" , "national_event_lead_1", "religious_event_lead_1", 
  "sporting_event_lead_3",  "cultural_event_lead_3" , "national_event_lead_3",   "religious_event_lead_3",
  
  "best_price",
  
  "snap",
  
  "Type", 
  
  "Low25", "Mean", "Median", "Up25", "Max"
  
)


## Build multiple different models
stat_total <- read.csv("data/processed/stat_total.csv", stringsAsFactors = F)
stores <- unique(stat_total$store_id)
departments <- unique(stat_total$dept_id)
df <- expand.grid(stores,departments, stringsAsFactors = F)
colnames(df) <- c("store_id", "dept_id")
head(df)

tr_full = tr

# Define the order in tr dataset (alphabetical order)
stores <- c("CA_1", "CA_2", "CA_3", "CA_4", "TX_1", "TX_2", "TX_3", "WI_1", "WI_2", "WI_3")
departments <- c("FOODS_1", "FOODS_2", "FOODS_3", "HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2")


for (i in 1:nrow(df)){
  
  # Test
  # i=1
  
  store <- df[i,"store_id"]
  dept <- df[i,"dept_id"]
  print(paste("Store:", store, "| Dept:", dept))
  
  
  # Subset training data
  tr <- tr_full[store_id == which(store == stores) & dept_id == which(dept == departments),] # & !is.na(rolling_mean_lag28_t180)
  
  anyNA(tr)
  sapply(tr, function(x){sum(is.na(x))})
  unique(tr[,j = c("item_id")])
  
  dim(tr) 
  gc()
  
  
  ## // Keep 1 month of validation data
  
  # Validation data
  flag <- tr$d >= 1914 - 28
  valid_data <- data.matrix(tr[flag, ..features])
  valid <- lgb.Dataset(data = valid_data, categorical_feature = categoricals, label = tr[["sales"]][flag])
  
  # Training data
  flag <- tr$d < 1914 - 28
  y <- tr[["sales"]][flag]
  tr <- data.matrix(tr[flag,..features,])
  tr <- lgb.Dataset(data = tr, categorical_feature = categoricals, label = y)
  
  
  ## // Train without validation data // 
  
  # y <- tr$sales
  # tr <- data.matrix(tr[,..features,])
  # tr <- lgb.Dataset(tr, categorical_feature = categoricals, label = y)
  # gc()
  
  
  ## Define parameters for training
  params <- list(objective = "tweedie",
                 tweedie_variance_power= 1.2,
                 metric ="rmse",
                 subsample = 0.5,
                 subsample_freq = 1,
                 learning_rate = 0.03,
                 num_leaves = 2**11-1,
                 min_data_in_leaf = 2**7-1,
                 feature_fraction = 0.5,
                 max_bin = 100,
                 n_estimators = 1400,
                 boost_from_average = F,
                 verbose = NULL,
                 nthread = 8)
  

  
  
  ## With validation set
  tic()
  lgb_model <- lgb.train(params = params,
                         data = tr,
                         num_boost_round = 4000,
                         eval_freq = 200, 
                         early_stopping_rounds = 600,
                         valids = list(valid = valid))

  toc()
  
  ## // Without validation set //
  # tic()
  # lgb_model <- lgb.train(params = params, data = tr)
  # toc()
  # 
  
  ## Save model
  # cat("Best score:", lgb_model$best_score, "at", lgb_model$best_iter, "iteration") 
  saveRDS.lgb.Booster(lgb_model, paste0("models/", store, "_", dept, "_lgb_model.rds"))
  gc()
  ## Check importances
  # imp <- lgb.importance(lgb_model)
  # View(imp)
  # lgb.plot.importance(imp, 20, cex = 0.9) 
  
  
}






