#############################
#                           #
#   Plotting functions      #
#                           # 
#############################

## Develop functions: Import data and forecastings
# sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F)
# calendar <- read.csv("data/raw/calendar.csv")
# frc_total <- readRDS("data/forecastings/frc_12_items_all_methods.RDS")

## Test (Parameters)
# method <- "naive"
# item_id <- "HOBBIES_1_001"
# store_id <- "CA_1"


## Plot forecastings for one time series

plot_forecastings_one_ts <- function(time_series, frc){
  
  item_sales <- c(time_series, frc)
  
  if(!exists("calendar")){calendar <- read.csv("data/raw/calendar.csv")}
  
  df <- data.frame(date = as.Date(calendar[1:length(item_sales),c("date")]),
                   item_sales = item_sales,
                   is_frc = c(rep(x = F, length(time_series)), rep(T, length(frc))) )
  
  p <- ggplot(df, aes(x = date, y = item_sales, col = is_frc)) +
    geom_point(size = 0.5) + geom_line(size = 0.2, alpha = 0.5) +
    scale_color_manual(values = c("#E7B800", "#00AFBB"))+
    theme(legend.position = "none")
  
  return(p)
}


## Plot forecastings from frc_total file

plot_forecastings <- function(forecasting_file, method, item_id, store_id){
  
  frc <- forecasting_file[forecasting_file$item_id == item_id & forecasting_file$store_id == store_id, method]
  past_sales <- as.numeric(sales[sales$item_id == item_id & sales$store_id == store_id, 7:ncol(sales)])
  
  item_sales <- c(past_sales, frc)
  
  df <- data.frame(item_sales = item_sales,
                   date = as.Date(calendar[1:length(item_sales),c("date")]),
                   frc = c(rep(x = F, length(past_sales)), rep(T, length(frc))) )
  
  
  p <- ggplot(df, aes(x = date, y = item_sales, col = frc)) +
    geom_point(size = 0.5) + geom_line(size = 0.2, alpha = 0.5) +
    scale_color_manual(values = c("#E7B800", "#00AFBB"))+
    theme(legend.position = "none")+
    ggtitle(paste("Store id:", store_id,  "| Item id:", item_id), method)
  
  return(p)
}






