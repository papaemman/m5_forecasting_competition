#######################
#                     #
#  Explore datasets   #  
#                     # 
#######################

getwd()


## 00. Load packages -----
library(tidyverse)


## 01. Import data ----

sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F)

calendar <- read.csv("data/raw/calendar.csv", na.strings = c("", "_"), stringsAsFactors = F)
calendar$date <- as.Date(calendar$date)

prices <- read.csv("data/raw/sell_prices.csv", stringsAsFactors = F)

## 02. Calendar
View(calendar)

sales <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_1.rds")

acf(sales)

frc <- readRDS("data/forecastings/point/frc_total_prophet.RDS")
head(frc)
frc_wide <- frc %>% pivot_wider(names_from  = fh, values_from = prophet)

frcs <- colSums(frc_wide[6:ncol(frc_wide)])

plot(frcs, type = "l")
df <- data.frame(val = c(sales[(length(sales)-200): length(sales)], frcs),
                 type = c(rep(T, 201), rep(F, 28)))

ggplot(df, aes(1:nrow(df), val)) + geom_line()

plot(sales[(length(sales)-100): length(sales)], type = "l")
head(calendar)


library(forecast)
temp <- tbats(y = AirPassengers)
plot(temp$fitted.values, type = "l")
