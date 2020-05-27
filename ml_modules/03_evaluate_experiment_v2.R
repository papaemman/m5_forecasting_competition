##################################
#                                #
#  Create submission file        #
#                                #
##################################


## 00. Load packages  -----
library(tidyverse)
library(data.table)
library(RcppRoll)
library(lightgbm)
library(tictoc)


# Source dependencies
source("ml_modules/feature_engineering/create_data.R")
source("ml_modules/feature_engineering/fe_sales.R")

# Define parameters
nrows = Inf                        # How many sales data to import
fh = 28                            # Forecasting horizon
max_lags = 366                     # Max lags (features)
tr_last = 1913                     # Training last day
fday = as.IDate("2016-04-25")      # Forecasting day (first day)



## 01. Load data  ----

# 1. Load and merge datasets (96 features)

# dt <- create_dt(is_train = TRUE, nrows = nrows,
#                 fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday,
#                 filter_params = "dept_id == 'HOBBIES_1' ")

dt <- create_dt(is_train = FALSE, nrows = nrows,
                fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday,
                filter_params = NA)
gc()


# 2. Create hierarhy sales features (107 features)
dt <- create_hierarhy_sales_features(dt)
gc()


# 3. Add categorical encoding features (129 features)

# Be carefull here! 
# These features must be calculated from all historic sales.
dt <- add_categorical_encoding_features_test(dt)
gc()

# 4. Preprocessing steps and add more features (143 features)
dt <- add_more_features(dt = dt)
gc()

dim(dt) # 12.834.328      143


##  // Sanity check //

# Test data start from the first necessary day to compute lag features for test days
# (28 + 180 days before the first test day)

anyNA(dt)
sapply(dt , function(x)sum(is.na(x))) %>% View() # only sales column (and total_sales)
1707440/(28*2) # 30.490 item_id - store_id

dt[item_id == "FOODS_1_340" & store_id == "CA_1", c("d", "sales", "Max", "Mean")] %>% View()



## 03. Iterative Forecasting ----


# // PROCESS //

# For every forecasting day:
# Create itarative the sales laging features,
# using the dt (test dataset) and create_sales_features() function,
# taking into consideration the previous predictions (as the ground truth value for the sales/demand).


## Parameters to load different models
stat_total <- read.csv("data/processed/stat_total.csv", stringsAsFactors = F)
stores <- unique(stat_total$store_id)
departments <- unique(stat_total$dept_id)

# Define the rights order in tr dataset (alphabetical order)
# stores <- c("CA_1", "CA_2", "CA_3", "CA_4", "TX_1", "TX_2", "TX_3", "WI_1", "WI_2", "WI_3")
# departments <- c("FOODS_1", "FOODS_2", "FOODS_3", "HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2")

df <- expand.grid(departments, stringsAsFactors = F)
colnames(df) <- c("dept_id")
df


# Load features
source("ml_modules/features.R")

setdiff(features, colnames(dt))  # the sales lagging features (89 features)
setdiff(colnames(dt), features)  # Features to not include in model (20 features)



## // Make forecastings //

for (day in seq(from = tr_last + 1, to = tr_last + 2*fh, by = 1) ){ # 2*fh

  ## Test
  # day = 1914
  cat(as.character(day), "\n")
  
  
  ## 1. Keep a subset of the test data with with the neccessary data to compute lags for the specific forecasting day
  
  # Get a subset from dt (test dataset), with the last max_lags days
  # because these are the days required to create the lagging sales features.

  tst <- dt[d >= day - max_lags & d <= day]
  
  
  ## 2. Create Sales features - lags (232 features)
  tst <- create_sales_features(tst)
  gc()
  
  
  ## 3. Sanity check of feature vectors for the specific test day - NA-imputations
  
  
  # tst[d==day, ] %>% nrow()         # 30490 (all item_id, store_id)
  # anyNA(tst[d==day, ..features])   # TRUE
  
  # tst[item_id == "FOODS_1_001" & store_id == "CA_1", c("d", "sales", "lag_t1", "lag_t2", "lag_t7", "rolling_mean_lag28_t180")] %>% View()
  
  
  # // PROBLEM //
  # Some items  don't have the necessary historical sales values, to calculate large window features, such as rolling_mean_lag28_t180.
  # SOLUTION  : I will use the first available smaller-window lag feature.
  # eg. lag_t180 <- lag_t120 
  
  # sapply(tst[d==day, ..features], function(x)sum(is.na(x))) %>% View()  
  # 8 features with NAs
  # sapply(tst[d==day,], function(x)sum(is.na(x))) %>% View() 
  
  ## Sort and check at the last rows how many NAs features exists
  
  # tst[d==1914, c("id", "rolling_mean_lag28_t180", "rolling_mean_lag28_t120", "rolling_mean_lag28_t90", "rolling_mean_lag28_t60", "rolling_mean_lag28_t30")] %>% View("tst")
  # tst[d==1914, c("id",  "rolling_mean_lag1_t120")] %>% View("tst")
  
  
  ## Tests with NAs (check them before and after)
  
  # tst[d==1914 & item_id == "HOUSEHOLD_2_342", c("id", "rolling_mean_lag28_t180","rolling_mean_lag28_t120")]
  # tst[d==1914 & item_id == "HOUSEHOLD_2_342" & store_id == "CA_4", c("id", "rolling_mean_lag28_t180","rolling_mean_lag28_t120")]
  # tst[d==1914 & item_id == "HOUSEHOLD_1_183" & store_id == "CA_4", c("id", "rolling_mean_lag28_t180","rolling_mean_lag28_t120", "rolling_mean_lag28_t60")]
  # tst[d==1914 & item_id == "HOUSEHOLD_1_512" & store_id == "CA_3", c("id", "rolling_mean_lag28_t180","rolling_mean_lag28_t120", "rolling_mean_lag28_t90", "rolling_mean_lag28_t60", "rolling_mean_lag28_t30")]
  # tst[d==1914 & item_id == "FOODS_3_595" & store_id == "CA_3", c("id", "rolling_mean_lag28_t180","rolling_mean_lag28_t120",  "rolling_mean_lag28_t60", "rolling_mean_lag28_t30")]
  # tst[d==1914 & item_id == "FOODS_3_296" & store_id == "CA_1", c("id", "rolling_mean_lag1_t120","rolling_mean_lag1_t7",  "rolling_mean_lag28_t60")]


  
  tst[d==day & is.na(rolling_mean_lag28_t60), rolling_mean_lag28_t60 := rolling_mean_lag28_t30
      #][d==day & is.na(rolling_mean_lag7_t60), rolling_mean_lag7_t180 := rolling_mean_lag7_t30
        ][d==day & is.na(rolling_mean_lag28_t90), rolling_mean_lag28_t90 := rolling_mean_lag28_t60
          ][d==day & is.na(rolling_mean_lag7_t90), rolling_mean_lag7_t90 := rolling_mean_lag7_t60
            ][d==day & is.na(rolling_mean_lag28_t120), rolling_mean_lag28_t120 := rolling_mean_lag28_t90
              ][d==day & is.na(rolling_mean_lag7_t120), rolling_mean_lag7_t120 := rolling_mean_lag7_t90
                ][d==day & is.na(rolling_mean_lag1_t120), rolling_mean_lag1_t120 := (rolling_mean_lag1_t7 + rolling_mean_lag28_t60 + rolling_mean_lag28_t30)/3
                  ][d==day & is.na(rolling_mean_lag28_t180), rolling_mean_lag28_t180 := rolling_mean_lag28_t120  # OR (rolling_mean_lag28_t120 + rolling_mean_lag28_t90 + rolling_mean_lag28_t60)/3
                    ][d==day & is.na(rolling_mean_lag7_t180), rolling_mean_lag7_t180 := rolling_mean_lag7_t120]
  
  
  # anyNA(tst[d==day,..features])
  
  
  # 4. Make forecastings for the specific day, using different model for each department
  
  for (i in 1:nrow(df)){
    
    # Test
    # i=1
    
    store <- df[i,"store_id"]
    dept <- df[i,"dept_id"]
    print(paste("Store:", store, "| Dept:", dept))
    
    
    ## Load model
    lgb_model <- readRDS.lgb.Booster(paste0("models/", dept, "_lgb_model.rds"))
    
    # Create test dataset for the current date - dept
    # (select only used features) 
    tst_day_dept <- data.matrix(tst[d == day & dept_id == dept, ..features])
    
    # anyNA(tst_day_dept)
    
    
    ## Make predictions and save them directly in dt (test dataset) in dedicated sales column
    
    # Predictions processing steps: 
    # 1. predictions as it is (float)
    # 2. round predictions
    # 3. floor predictions
    # 4. ceil predictions
    
    dt[d == day & dept_id == dept, sales := round(predict(lgb_model, tst_day_dept))] 
    
    # dt[d == day & dept_id == dept, ] %>% nrow()
    # dt[d==day & dept_id == dept,list(id, item_id, dept_id, cat_id, store_id, state_id,d) ]
    # tst[d == day & dept_id == dept,list(id, item_id, dept_id, cat_id, store_id, state_id,d) ]
    
    ## Sanity check predictions
    
    # dim(tst_day_dept)     # products of dept * 1 day
    # temp_df <- data.frame(tst_day_dept[,c("Max","Mean", "mean_last_month", "rolling_mean_lag28_t180", "lag_t28", "lag_t21" ,"lag_t14", "lag_t8", "lag_t7", "lag_t6","lag_t5","lag_t4","lag_t3","lag_t2","lag_t1")])
    # temp_df$preds <- predict(lgb_model, tst_day_dept)
    # View(temp_df)
    
    gc()
    }
  
  
  # // Next Iteration: next day //
  # Use again the dt (test dataset) to:
  # 1. subset the last max_lags days,
  # 2. create saels lagging features 
  # 3. make predictions 
  
  gc()
}

# Temp Save predictions
temp <- dt
gc()

## Check model importances

# dt
# imp <- lgb.importance(lgb_model)
# View(imp) # 192 rows



## 02. Create submission file ----

# - Filter the required days
# - For observations after fday + fh (=28), change (sub) the id name from validation to evaluation
# - Create a column d with values F1:F28 according to id column
# - Transform datasets from long to wide, using dcast, keeping only id, d, sales columns (.SD = subset data)
# - order according to sample submission data
# - write submission file

sample_submission <- fread("data/pnt_submissions/sample_submission.csv")

day = 1914


submission_file <- dt[d >= day, list(id, d, sales)
                      ][d >= day+fh, id := sub("validation", "evaluation", id)
                            ][, dcast(.SD, id ~ d, value.var = "sales")
                              ][sample_submission[,.(id)], on = "id"
                                ][, j = c("id", paste0("F", 1:28))]


dt[d>day & id == "HOBBIES_1_001_CA_1_validation", ] %>% View()
submission_file <- dt[d >= day, list(id, d, sales)
                      ][d >= day+fh, id := sub("validation", "evaluation", id)
                        ][,d := as.character(d)
                          ][, d := paste0("F", 1:28), by = id
                            ][, dcast(.SD, id ~ d, value.var = "sales")
                              ][sample_submission[,.(id)], on = "id"
                                ][, j = c("id", paste0("F", 1:28))]



# For the test days (If I don't have any predictions)
# submission_file[is.na(submission_file)] <- 0

dim(submission_file)
write.csv(x = submission_file, file = "lgbm_v0.csv", row.names = F)


