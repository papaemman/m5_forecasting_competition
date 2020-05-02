######################################
#                                    #
#  Experiment: AutoTS                #
#                                    #
######################################


# Load packages
library(RemixAutoML)
library(data.table)
library(dplyr)

# Import datasets
sales <- fread("data/raw/sales_train_validation.csv")
calendar <- fread("data/raw/calendar.csv")


# Get forecasts for top-down models ----

sales <- sales[,7:ncol(sales)] %>% colSums()
sales <- as.data.table(sales, keep.rownames = T)
colnames(sales) <- c("d", "sales")
sales <- sales[calendar[j=c("d", "date")], on="d", nomatch=0][,d:= NULL]

View(sales)


# Get calendar events (external regressors)
x_var <- calendar
x_var$snap <- x_var$snap_CA+x_var$snap_WI+x_var$snap_TX
x_var$holiday <- 0
x_var[is.na(x_var$event_type_1)==F,]$holiday <- 1
x_var <- x_var[,c("snap","holiday")]



# forecast for the next 28 days
frc <- RemixAutoML::AutoTS(
  data = sales,
  TargetName = "sales",
  DateName = "date",
  FCPeriods = 28,
  HoldOutPeriods = 28,
  TimeUnit = "day",
  Lags = 28,
  SLags = 28,
  MaxFourierPairs = 12,
  PrintUpdates = T,
  NumCores = 5 
)

saveRDS(frc, "frc.rds")

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





