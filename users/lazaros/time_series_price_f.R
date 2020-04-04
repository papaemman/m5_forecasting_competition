#################################################
#                                               #
# Add price statistics for every time series    #
#                                               #
#################################################

library(dplyr)
library(ggplot2)
library(patchwork)

# Import data
sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F)
stat_total <- readRDS("data/processed/stat_total.rds")

prices <- read.csv("data/raw/sell_prices.csv", stringsAsFactors = F)
dim(prices)

head(prices)

# Get category id
prices$cat_id <- sapply(strsplit(prices$item_id, split = "_"), function(x){x[1]})


# Calculate price statistics for every item_id, store_id
prices_aggregation <- prices %>% 
  group_by(item_id, store_id, cat_id) %>% 
  mutate(diff = sell_price - lag(sell_price)) %>% 
  summarise(min_price = min(sell_price),
            max_price = max(sell_price),
            mean_price = mean(sell_price),
            sd_price = sd(sell_price),
            max_diff = max(sell_price) - min(sell_price),
            relative_diff_mean = max_diff/mean(sell_price),
            relative_diff_min = max_diff/min(sell_price),
            relative_diff_max = max_diff/max(sell_price),
            n_of_changes = sum(diff > 0, na.rm = TRUE))
  


# Save stat_total
stat_total <- merge(stat_total, prices_aggregation)

View(stat_total)

saveRDS(stat_total, file = "data/processed/stat_total.rds")


## Visualizations


prices_aggregation <- prices_aggregation %>% mutate(value = case_when(mean_price < 2 ~ "low",
                                                                      mean_price < 10 ~ "medium",
                                                                      T ~ "high"))

prices_aggregation$value %>% table

ggplot(stat_total) + geom_point(aes(dollar_sales, mean_price, color = cat_id))
 
prices$cat_id %>% table

ggplot(prices_aggregation) + geom_histogram(aes(mean_price, fill = cat_id))

head(stat_total)

ggplot(stat_total) + geom_jitter(aes(mean_price, n_of_changes, col = cat_id))                

ggplot(stat_total) + geom_histogram(aes(max_diff, fill = cat_id))     
summary(stat_total$max_diff)

stat_total[which.max(stat_total$max_diff),]

colnames(sales)
item <- sales %>% filter(item_id == "HOUSEHOLD_2_406", store_id == "WI_3")

df_temp <- data.frame(sales = as.numeric(item[7:ncol(item)]),
                 sell_price = rep(sell_price[1:(length(sell_price)-8)], each = 7),
                 index = 1:1913)

head(prices)
sell_price <- prices %>% filter(item_id == "HOUSEHOLD_2_406", store_id == "WI_3") %>% pull(sell_price)
length(sell_price)

ggplot(df, aes(index, sales)) + geom_point() -> q


a <- prices %>% filter(item_id == "HOUSEHOLD_1_001", store_id == "WI_3") %>% select(store_id, sell_price)
b <- prices %>% filter(item_id == "HOUSEHOLD_1_001", store_id == "WI_1") %>% select(store_id, sell_price)
d <- prices %>% filter(item_id == "HOUSEHOLD_1_001", store_id == "WI_2") %>% select(store_id, sell_price)

a <- rbind(NA,a)
d <- rbind(rep(c(NA,NA),10), d)

df <- cbind(a,b,d)
head(df)
colnames(df) <- c("store_id_3", "sell_price_3", "store_id_2",  "sell_price_1", "store_id_1" , "sell_price_2", "index"  )   
df$index <- 1:nrow(df)

ggplot(df)+
  geom_line(aes(index, sell_price_1), col = "red")+
  geom_line(aes(index, sell_price_2), col = "green")+
  geom_line(aes(index, sell_price_3), col = "blue") -> p

p
p/q
