#############################
#                           #
#   WRMSSE:                 #
#                           #
#############################

time_series_b <- readRDS("data/processed/time_series_b.rds")


## 01. Denominator for WRMSSE calculations (for all the training dataset)

den <- c()
store_id <- c()
item_id <- c()

for (i in 1:length(time_series_b)) {
  
  den[i] <- sum(diff(time_series_b[[i]]$x)^2)/length(time_series_b[[i]]$x)
  store_id[i] <- time_series_b[[i]]$store_id
  item_id[i] <- time_series_b[[i]]$item_id
}

wrmsse_den <- data.table(den, store_id, item_id)

# cols <- c("item_id", "store_id")
# wrmsse_den[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]

write.csv(wrmsse_den, file = "data/wrmsse_den_v2.csv", row.names = F, quote = F)



## 02. Denominator for WRMSSE calculations (without the last 28 days from the training dataset)

den <- c()
store_id <- c()
item_id <- c()

for (i in 1:length(time_series_b)) {
  
  temp <- time_series_b[[i]]$x[1:(length(time_series_b[[i]]$x)-28)]
  
  den[i] <- sum(diff(temp)^2)/length(temp)
  store_id[i] <- time_series_b[[i]]$store_id
  item_id[i] <- time_series_b[[i]]$item_id
}

wrmsse_den_without_last_28 <- data.table(den, store_id, item_id)

# cols <- c("item_id", "store_id")
# wrmsse_den_without_last_28[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]

write.csv(wrmsse_den_without_last_28, file = "data/wrmsse_den_without_last_28_v2.csv", row.names = F, quote = F)



## Get the Weights  -----

stat_total <- readRDS("data/processed/stat_total.rds")

total_sales <- sum(stat_total$dollar_sales)

options(scipen = 10)
stat_total <- stat_total %>%
  select(item_id, store_id, dollar_sales) %>% 
  mutate(weight = dollar_sales / total_sales) %>% 
  select(item_id, store_id, weight)

stat_total <- as.data.table(stat_total)
stat_total

# cols <- c("item_id", "store_id")
# stat_total[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]

write.csv(stat_total, file = "data/bts_weights_v2.csv", row.names = F, quote = F)




