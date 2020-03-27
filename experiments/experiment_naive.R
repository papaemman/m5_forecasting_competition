##########################################
#                                        #
#  Experiment: Prophet facebook package  #
#                                        #
##########################################

## Source dependencies 
lapply(X = paste0("modules/", list.files(path = "modules/")), FUN = source)


## Define methods names to be passed as argument to wrapper_frc_methods() function
b_names <- c("Naive")


## Import datasets
sales <- read.csv("data/raw/sales_train_validation.csv")
calendar <- read.csv("data/raw/calendar.csv")
time_series_b <- readRDS(file = "data/processed/time_series_b.rds")


## Make and save forecastings 

# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 6, type = "SOCK", outfile = ""))

# Get forecasts using the wrapper_frc_methods(x, fh, b_names)
frc_total <- foreach(tsi = 1:nrow(sales),
                     .combine = 'rbind', 
                     .packages = c('zoo','randomForest','RSNNS','forecast','smooth')) %dopar% wrapper_frc_methods(x = time_series_b[[tsi]], fh = 28, b_names = b_names)

# Save forecastings
saveRDS(object = frc_total, file = "data/forecastings/frc_total_naive.RDS")

# Stop cluster and clean the memory
stopCluster(cl = cl) # This error happens when code from library(doSNOW) and library(parallel) is mixed.
showConnections()
gc()


## Read forecastings object and check its validity
frc_total <- readRDS(file = "data/forecastings/frc_total_naive.RDS")
dim(frc_total) # 853720      (6 + number of forecasting methods)
View(head(frc_total, 1000))


## Evaluate experiment's forecastings
tic()
scores <- evaluate_experiment(frc_total = frc_total, b_names = b_names, evaluation_file_name_prefix = "exp_naive")
toc() # ~ 20 minutes


## Make submission file
tic()
make_submission_files(frc_total = frc_total, b_names = b_names, submission_file_name_prefix = "exp")
toc() # ~ 30 minutes


## Transform submission file
exp_naive <- read.csv("data/submissions/exp_Naive_submission.csv", stringsAsFactors = F)
exp_naive <- rbind_submission_file(submission_file = exp_naive)
write.csv(x = exp_naive, file = "data/submissions/exp_Naive_submission_final.csv", row.names = F)


