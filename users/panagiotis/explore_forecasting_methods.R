#######################################
#                                     #
#  Explore new forecasting method     #
#                                     #
#######################################

## Source dependencies 
lapply(X = paste0("modules/", list.files(path = "modules/")), FUN = source)

## Import data
# raw
calendar <- read.csv("data/raw/calendar.csv", stringsAsFactors = F, na.strings=c("","NA"))

# processsed
time_series_b <- readRDS("data/processed/time_series_b.rds")
stat_total <- readRDS("data/processed/stat_total.rds")

View(stat_total)


## Plotting functions

# Select a time series
x <- time_series_b[[sample(1:length(time_series_b), size = 1)]]
x <- time_series_b[[8]]
x

plot_ts(x = x, calendar = calendar, events = F, snap = F)
plot_ts(x = x, calendar = calendar, events = T, snap = T)


plot_ts <- function(x, calendar, events = F, snap = F){
  
  # Keep only relevant dates from calendar
  calendar <- calendar[(nrow(calendar)-length(x$x)+1):nrow(calendar), ]
  calendar$is_event <- ifelse(is.na(calendar$event_name_1), 0, 1) + ifelse(is.na(calendar$event_name_2), 0, 1)
  
  # Create data frame for ggplot2
  sales_cal_df <- data.frame(day = as.Date(calendar$date),
                             events = ifelse(calendar$is_event == 0, F, T),
                             snap = calendar[, paste0("snap_", x$state_id)],
                             sales = x$x)
  
  p <- ggplot(sales_cal_df, aes(x = day, y = sales)) + 
    geom_line() +
    {if(snap) geom_point(data = sales_cal_df %>% filter(snap!=0), aes(x = day, y = sales), col = "red", shape = 3) } +
    {if(events)geom_point(data = sales_cal_df %>% filter(events!=0), aes(x = day, y = sales), col = "grey") }+
    ggtitle(paste("Item id:", x$item_id, "| Store id:", x$store_id))

  return(p)
}


## Prophet
library(prophet)


