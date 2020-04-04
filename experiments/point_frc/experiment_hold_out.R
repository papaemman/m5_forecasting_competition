#######################################
#                                     #
#  Experiment on hold out dataset     #
#                                     #
#######################################

## Objective:
# Evaluate all univariate forecasting methods based on hold out dataset
# - last 28 days of sales_train_validation
# - the same 28 validation days for the previous year

# Source dependencies 
lapply(X = paste0("modules/", list.files(path = "modules/")), FUN = source)


## Define methods names to be passed as argument to wrapper_frc_methods() function
b_names <- c("naive", "s_naive", "SexpS", "MA",
             "croston_cl", "croston_opt", "croston_sba",
             "tsb", "adida", "imapa", "smooth_es", "arima",
             "mlp_local", "rf_local", "prophet", "ma_v2", "mlp_local_v2", "rf_local_v2")


## Import datasets
sales <- read.csv("data/raw/sales_train_validation.csv")
calendar <- read.csv("data/raw/calendar.csv")
time_series_b <- readRDS(file = "data/processed/time_series_b.rds")


## Make and save forecastings 

# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 6, type = "SOCK", outfile = ""))

# Get forecasts using the wrapper_frc_methods(x, fh, b_names)
frc_total <- foreach(tsi = 1:12, # 1:length(time_series_b)
                     .combine = 'rbind', 
                     .packages = c('zoo','randomForest','RSNNS','forecast','smooth','prophet')) %dopar% wrapper_frc_methods_template(x = time_series_b[[tsi]], fh = 28, b_names = b_names)

# Save forecastings
saveRDS(object = frc_total, file = "data/forecastings/frc_12_items_all_methods.RDS")

# Stop cluster and clean the memory
stopCluster(cl = cl) # This error happens when code from library(doSNOW) and library(parallel) is mixed.
showConnections()
.rs.restartR()


## Read forecastings object and check its validity
frc_total <- readRDS(file = "data/forecastings/frc_12_items_all_methods.RDS")
dim(frc_total) # 853720      (6 + number of forecasting methods)
View(head(frc_total, 1000))
colnames(frc_total)


plot_forecastings(forecasting_file = frc_total, method = "ma_v2",
                  item_id = "HOBBIES_1_008", store_id = "CA_1")


  ## Evaluate experiment's forecastings
tic()
scores <- evaluate_experiment(frc_total = frc_total, b_names = b_names, evaluation_file_name_prefix = "exp_naive")
toc() # ~ 20 minutes


## Make submission file
tic()
make_submission_files(frc_total = frc_total, b_names = b_names, submission_file_name_prefix = "exp")
toc() # ~ 30 minutes

## Transform submission file
exp <- read.csv("data/submissions/", stringsAsFactors = F)
exp <- rbind_submission_file(submission_file = exp)
write.csv(x = exp, file = "data/submissions/", row.names = F)
