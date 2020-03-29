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


## Plot forecastings

plot_forecastings <- function(forecasting_file, method, item_id, store_id){
  
  frc <- frc_total[frc_total$item_id == item_id & frc_total$store_id == store_id, method]
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






