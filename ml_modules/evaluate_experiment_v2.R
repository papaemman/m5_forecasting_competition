##################################
#                                #
#  Create submission file        #
#                                #
##################################


library(lightgbm)
library(data.table)
library(tictoc)
library(RcppRoll)

## Source dependencies
source("ml_modules/create_data.R")
source("ml_modules/fe_sales.R")


## Define parameters

nrows = Inf                        # How many sales data to import
fh = 28                            # Forecasting horizon
max_lags = 366                     # Max lags (features)
tr_last = 1913                     # Training last day
fday = as.IDate("2016-04-25")      # Forecasting day (first day)


## 06. Forecasting ----

# Import test dataset required for forecasting 
te <- create_dt(is_train = FALSE, nrows = nrows, fh = fh, max_lags = max_lags, tr_last = tr_last, fday = fday)

## Preprocessing steps and add features
te <- add_features(dt = te)
gc()


# SOS: For every forecasting day,
#      using the te (test) dataset and create_fea() function,
#      create features (for sales column - lags, rolling features etc),
#      taking into consideration the predictions (as the ground truth value for the demand)

## Build multiple different models
stat_total <- read.csv("data/processed/stat_total.csv", stringsAsFactors = F)
stores <- unique(stat_total$store_id)
departments <- unique(stat_total$dept_id)
df <- expand.grid(stores,departments, stringsAsFactors = F)
colnames(df) <- c("store_id", "dept_id")
head(df)

# Define the order in tr dataset (alphabetical order)
stores <- c("CA_1", "CA_2", "CA_3", "CA_4", "TX_1", "TX_2", "TX_3", "WI_1", "WI_2", "WI_3")
departments <- c("FOODS_1", "FOODS_2", "FOODS_3", "HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2")


for (day in as.list(seq(fday, length.out = 2*fh, by = "day"))){
  
  ## Test
  # day = as.Date("2016-04-25")
  cat(as.character(day), "\n")
  
  # Get a subset from te (test) dataset, with the last max_lags days
  # because these are the days required to create the prediction features (lag, roll_means etc)

  tst <- te[date >= day - max_lags & date <= day]
  
  # Create features for this dataset
  tst <- create_sales_features(tst)
  gc()
  
  
  for (i in 1:nrow(df)){
    
    # Test
    # i=1
    
    store <- df[i,"store_id"]
    dept <- df[i,"dept_id"]
    print(paste("Store:", store, "| Dept:", dept))
    
    
    ## Load model
    lgb_model <- readRDS.lgb.Booster(paste0("models/", store, "_", dept, "_lgb_model.rds"))
    
    # Create test dataset for the current date
    # (select only used features) 
    tst_day_store_dept <- data.matrix(tst[date == day & store_id == which(store == stores) & dept_id == which(dept == departments), ..features])
    
    # Note: Some big lags may be NA for some products,
    # because these products may not be available during all this period of time.
    # sapply(as.data.frame(tst), function(x)sum(is.na(x))) %>% View()
    
    
    # Make predictions and save them in te (test) dataset. 
    # (use magic multiplier in prediction)
    te[date == day  & store_id == which(store == stores) & dept_id == which(dept == departments), sales := predict(lgb_model, tst_day_store_dept)] 
    
    
    # Iteration: Use again the te (test) dataset to subset the last max_lags days, to create features and make new predictions.
    
    
    
    }
  
}

te


## 02. Create submission file ----

# - Filter the required days
# - For observations after fday + fh (=28), change (sub) the id name from validation to evaluation
# - Create a column d with values F1:F28 according to id column
# - Transform datasets from long to wide, using dcast, keeping only id, d, sales columns (.SD = subset data)
# - order according to sample submission data
# - write submission file

sample_submission <- fread("data/pnt_submissions/sample_submission.csv")

te[date >= fday
   ][date >= fday+fh, id := sub("validation", "evaluation", id)
     ][,d := as.character(d)
       ][, d := paste0("F", 1:28), by = id
         ][, dcast(.SD, id ~ d, value.var = "sales")
           ][sample_submission[,.(id)], on = "id"
             ][, j = c("id", paste0("F", 1:28))
               ][, fwrite(.SD, "data/pnt_submissions/sub_dt_lgb_v0_1.01.csv")]


