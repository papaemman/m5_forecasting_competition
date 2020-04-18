######################
#                    #
# Naive submissions  #
#                    #
######################

library(dplyr)

# Import data 
sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F)
calendar <- read.csv("data/raw/calendar.csv", stringsAsFactors = F)
calendar$date <- as.Date(calendar$date)


## 01. Submit the same month of the previous year ------

# Find forecasting calendar days

forecasting_days <- tail(calendar,28*2) 

forecasting_days[1,]    # First day: 2016-04-25 | d_1914 (validation set - 28 days)
forecasting_days[28,]   # Last day: 2016-05-22  | d_1941
forecasting_days[29,]   # First day: 2016-05-23 | d_1942 (test set - 28 days)
forecasting_days[2*28,] # Last day: 2016-06-19  | d_1969 


# Get the sales from the previous year
calendar %>% filter(date == "2015-04-25" | date == "2015-06-19") # d_1548 - d_1603

naive_forecastings <- sales %>% select(id, d_1548:d_1603)

naive_forecastings_evaluation <- naive_forecastings[, c(1, 30:ncol(naive_forecastings))]
naive_forecastings_evaluation$id <- gsub(naive_forecastings_evaluation$id, pattern = "validation",replacement = "evaluation")
colnames(naive_forecastings_evaluation) <- c("id", paste0("F", 1:28))
head(naive_forecastings_evaluation)

naive_forecastings_validation <- naive_forecastings[, 1:29]
colnames(naive_forecastings_validation) <- c("id", paste0("F", 1:28))
head(naive_forecastings_validation)

naive_forecastings <- rbind(naive_forecastings_validation, naive_forecastings_evaluation)
dim(naive_forecastings) # 60980    29
write.csv(naive_forecastings, file = "data/pnt_submissions/same_month_previous_year.csv",row.names = F)

## Terminal commands to make submission
# $ kaggle competitions submit -c m5-forecasting-accuracy -f data/pnt_submissions/same_month_previous_year.csv -m "Same month, previous year"
# $ kaggle competitions submissions -c m5-forecasting-accuracy

                               
## 02. Submit the exact previous month, before the validation data -----

# Get the sales from the previous month
naive_forecastings_validation <- sales[, c("id", paste0("d_", 1886:1913))]

naive_forecastings_evaluation <- naive_forecastings_validation
naive_forecastings_evaluation$id <- gsub(naive_forecastings_evaluation$id, pattern = "validation",replacement = "evaluation")

naive_forecastings <- rbind(naive_forecastings_validation, naive_forecastings_evaluation)
colnames(naive_forecastings) <- c("id", paste0("F", 1:28))

write.csv(naive_forecastings, file = "data/pnt_submissions/previous_month_same_year.csv", row.names = F)

## Terminal commands to make submission
# $ kaggle competitions submit -c m5-forecasting-accuracy -f data/pnt_submissions/previous_month_same_year.csv -m "Previous month, same year"
# $ kaggle competitions submissions -c m5-forecasting-accuracy


## 03. Submit the month before the previous month, before the validation data ----

# Get the sales from the month before the previous month
naive_forecastings_validation <- sales[, c("id", paste0("d_", 1858:1885))]

naive_forecastings_evaluation <- naive_forecastings_validation
naive_forecastings_evaluation$id <- gsub(naive_forecastings_evaluation$id, pattern = "validation",replacement = "evaluation")

naive_forecastings <- rbind(naive_forecastings_validation, naive_forecastings_evaluation)
colnames(naive_forecastings) <- c("id", paste0("F", 1:28))

write.csv(naive_forecastings, file = "data/pnt_submissions/month_before_the_previous_month_of_the_same_year.csv", row.names = F)

## Terminal commands to make submission
# $ kaggle competitions submit -c m5-forecasting-accuracy -f data/pnt_submissions/month_before_the_previous_month_of_the_same_year.csv -m "Month before the previous month, same year"
# $ kaggle competitions submissions -c m5-forecasting-accuracy

