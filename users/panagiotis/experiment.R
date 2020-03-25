##########################################
#                                        #
#  Experiment: Prophet facebook package  #
#                                        #
##########################################

# Source dependencies 
lapply(X = paste0("modules/", list.files(path = "modules/")), FUN = source)

# Import datasets
sales <- read.csv("data/raw/sales_train_validation.csv")
calendar <- read.csv("data/raw/calendar.csv")
time_series_b <- readRDS(file = "data/processed/time_series_b.rds")