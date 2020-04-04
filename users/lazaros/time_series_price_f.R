stat_total <- readRDS("data/processed/stat_total.rds")
prices <- read_csv("data/raw/sell_prices.csv")

stat_total$item_id %>% unique() %>% length()



prices$item_id %>% sample(2) -> items

prices %>% filter(item_id %in% items) -> item
prices$cat_id <- sapply(strsplit(prices$item_id,"_"),function(x){x[1]})
prices %>% 
  group_by(item_id,store_id,cat_id) %>% 
  mutate(diff = sell_price - lag(sell_price)) %>% 
  summarise(min_price = min(sell_price),
            max_price = max(sell_price),
            mean_price = mean(sell_price),
            sd_price = sd(sell_price),
            max_diff = max(sell_price) - min(sell_price),
            relative_diff_mean = max_diff/mean(sell_price),
            relative_diff_min = max_diff/min(sell_price),
            relative_diff_max = max_diff/max(sell_price),
            n_of_changes =sum(diff > 0, na.rm = TRUE)) -> prices_aggregation
  

prices_aggregation %<>% mutate(value = case_when(mean_price < 2 ~ "low",
                                    mean_price < 10 ~ "medium",
                                    T ~ "high"
                                    ))



data <- merge(stat_total,prices_aggregation)

# ggplot(data) + geom_point(aes(dollar_sales,mean_price, color =cat_id))
# prices_aggregation$value %>% table
# 
# prices$cat_id %>% table
# ggplot(prices_aggregation) + geom_histogram(aes(mean_price,fill =cat_id))


# item %>% group_by(store_id,item_id) %>% summarise(max_price = max(sell_price),
#                                                   min_price = min(sell_price),
#                                                   sd_price = sd(sell_price),
#                                                   mean_price = mean(sell_price),
#                                                   max_diff = max(sell_price) - min(sell_price))


                                 
