##########################################
#                                        #
#  Experiment: Prophet facebook package  #
#                                        #
##########################################

# Source dependencies 
lapply(X = paste0("modules/", list.files(path = "modules/")), FUN = source)


## Define methods names to be passed as argument to wrapper_frc_methods() function
b_names <- c("prophet")


## Import datasets
sales <- read.csv("data/raw/sales_train_validation.csv")
calendar <- read.csv("data/raw/calendar.csv")
time_series_b <- readRDS(file = "data/processed/time_series_b.rds")


## Make and save forecastings 

# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 6, type = "SOCK", outfile = ""))

# Get forecasts using the wrapper_frc_methods(x, fh, b_names)
tic()
frc_total <- foreach(tsi = 1:length(time_series_b), 
                     .combine = 'rbind', 
                     .packages = c('zoo','randomForest','RSNNS','forecast','smooth', 'prophet')) %dopar% prophet_wrapper(x = time_series_b[[tsi]], fh = 28, b_names = b_names)
toc() # ~ 9 hours

# Save forecastings
saveRDS(object = frc_total, file = "data/forecastings/frc_total_prophet.RDS")
    
# Stop cluster and clean the memory
stopCluster(cl = cl) # This error happens when code from library(doSNOW) and library(parallel) is mixed.
showConnections()
gc()


## Read forecastings object and check its validity
frc_total <- readRDS(file = "data/forecastings/frc_total_prophet.RDS")
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

# Raw predictions
exp_prophet <- read.csv("data/submissions/exp_prophet_submission.csv", stringsAsFactors = F)

format(object.size(exp_prophet), units = "auto")
View(exp_prophet)

exp_prophet <- rbind_submission_file(submission_file = exp_prophet)
write.csv(x = exp_prophet, file = "data/submissions/exp_prophet_submission_final.csv", row.names = F)


# Round
exp_prophet <- read.csv("data/submissions/exp_prophet_submission.csv", stringsAsFactors = F)
exp_prophet[, setdiff(colnames(exp_prophet), "id") ] <- round(x = exp_prophet[, setdiff(colnames(exp_prophet), "id")] )
exp_prophet <- rbind_submission_file(submission_file = exp_prophet)
write.csv(x = exp_prophet, file = "data/submissions/exp_prophet_submission_final_round.csv", row.names = F)


# Floor
exp_prophet <- read.csv("data/submissions/exp_prophet_submission.csv", stringsAsFactors = F)
exp_prophet[, setdiff(colnames(exp_prophet), "id") ] <- floor(x = exp_prophet[, setdiff(colnames(exp_prophet), "id")] )
exp_prophet <- rbind_submission_file(submission_file = exp_prophet)
write.csv(x = exp_prophet, file = "data/submissions/exp_prophet_submission_final_floor.csv", row.names = F)


# Ceiling
exp_prophet <- read.csv("data/submissions/exp_prophet_submission.csv", stringsAsFactors = F)
exp_prophet[, setdiff(colnames(exp_prophet), "id") ] <- ceiling(x = exp_prophet[, setdiff(colnames(exp_prophet), "id")] )
exp_prophet <- rbind_submission_file(submission_file = exp_prophet)
write.csv(x = exp_prophet, file = "data/submissions/exp_prophet_submission_final_ceiling.csv", row.names = F)


# Div 2
exp_prophet <- read.csv("data/submissions/exp_prophet_submission.csv", stringsAsFactors = F)
exp_prophet[, setdiff(colnames(exp_prophet), "id") ] <- exp_prophet[, setdiff(colnames(exp_prophet), "id")]/2
exp_prophet <- rbind_submission_file(submission_file = exp_prophet)
write.csv(x = exp_prophet, file = "data/submissions/exp_prophet_submission_final_div_2.csv", row.names = F)

