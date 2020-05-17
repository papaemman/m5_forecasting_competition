##############################################################
#                                                            # 
# Calculate historical proportion for TOP-DOWN approaches    #
#                                                            #
##############################################################

# If I use top-down forecasting approaches,
# meaning that I make forecastings for time series in aggregation level greater than 12, 
# then I have to caclulate historical proportions gor every time series in lower aggregation level.


# Import datasets
sales <- read.csv("data/raw/sales_train_validation.csv")
time_series_b <- readRDS("data/processed/time_series_b.rds")


## Aggregation level 1: Count the total sales for each day for all products in all stores ----

insample_top <- ts(as.numeric(colSums(sales[,7:ncol(sales)])), frequency = 7)

# How to calculate proportions:
# Divide the total sales for each product for the last 28 days, with the total sales of the last 28 days
proportions <- unlist(lapply(X = c(1:length(time_series_b)), 
                             FUN = function(x) {
                               cat("\r",paste("ts id:", x))
                               sum(tail(as.numeric(sales[x,7:ncol(sales)]),28))/sum(tail(insample_top, 28))
                             }))

head(proportions)
str(proportions)

saveRDS(object = proportions, file = "data/processed/historical_proportions_level_1.rds")



## Aggregation level 2: Count the total sales for each day aggregated for each State ----


aggregated_sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_2.rds")

# How to calculate proportions:
# Divide the total sales for each product for the last 28 days, with the total sales of the last 28 days for every State

proportions <- unlist(lapply(X = c(1:length(time_series_b)), 
                             
                             FUN = function(x) {
                               
                               # x = 1
                               cat("\r",paste("ts id:", x))
                               aggregated_sales_state <- aggregated_sales["state_id" == time_series_b[[x]]$state_id, 7:ncol(aggregated_sales)]
                               sum(tail(as.numeric(sales[x,7:ncol(sales)]),28))/sum(tail(aggregated_sales_state, 28))
                             }))

head(proportions)
str(proportions)

saveRDS(object = proportions, file = "data/processed/historical_proportions_level_1.rds")





