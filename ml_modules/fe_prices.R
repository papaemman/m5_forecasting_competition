##################################
#                                #
# Feature Engineering: Prices    #
#                                #
##################################

## Source dependencies
source("modules/main.R")

library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(RcppRoll)

# TODO Να βάλω σαν feature την σχετική διαφορά από την μέση τιμή των άλλων
# και όχι μόνο την διαφορά από την απόλυτη τιμή σαν απόλυτο αριθμό.


create_prices_data <- function(){
  
  ## Import data ----
  prices <- fread("data/raw/sell_prices.csv", stringsAsFactors = F)
  
  # head(prices)
  # dim(prices) # 6841121, 4
  prices[ , .N, by = c("item_id", "store_id")]
  
  
  ## FE 01: Item prices compared with other stores ----
  
  prices_wide <- tidyr::pivot_wider(data = prices, names_from = store_id, values_from =  sell_price)
  
  prices_wide <- prices_wide %>% mutate(
    
    # Diff from mean sale prices of other stores
    CA_1_diff_from_mean = CA_1 - rowMeans(select(., CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
    CA_2_diff_from_mean = CA_2 - rowMeans(select(., CA_1, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
    CA_3_diff_from_mean = CA_3 - rowMeans(select(., CA_1, CA_2, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
    CA_4_diff_from_mean = CA_4 - rowMeans(select(., CA_1, CA_2, CA_3, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
    TX_1_diff_from_mean = TX_1 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_2, TX_3, WI_1, WI_2, WI_3), na.rm = T),
    TX_2_diff_from_mean = TX_2 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_3, WI_1, WI_2, WI_3), na.rm = T),
    TX_3_diff_from_mean = TX_3 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, WI_1, WI_2, WI_3), na.rm = T),
    WI_1_diff_from_mean = WI_1 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_2, WI_3), na.rm = T),
    WI_2_diff_from_mean = WI_2 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_3), na.rm = T),
    WI_3_diff_from_mean = WI_3 - rowMeans(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2), na.rm = T),
    
    # Has the best price
    min_price = pmap(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), min, na.rm = T),
    max_price = pmap(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), max, na.rm = T),
    
    CA_1_best_price = CA_1 == min_price,
    CA_2_best_price = CA_2 == min_price,
    CA_3_best_price = CA_3 == min_price,
    CA_4_best_price = CA_4 == min_price,
    TX_1_best_price = TX_1 == min_price,
    TX_2_best_price = TX_2 == min_price,
    TX_3_best_price = TX_3 == min_price,
    WI_1_best_price = WI_1 == min_price,
    WI_2_best_price = WI_2 == min_price,
    WI_3_best_price = WI_3 == min_price
    
  )
  
  prices_long <- prices_wide %>% select(item_id, wm_yr_wk, contains("diff_from_mean")) %>%
    pivot_longer(., cols = contains("diff_from_mean")) %>% 
    mutate(store_id = gsub(pattern = "_diff_from_mean", replacement = "", x = name), name = NULL) %>% 
    rename(diff_from_mean = value)
  
  prices_long <- as.data.table(prices_long)
  
  # head(prices_long)
  
  
  # Selling prices                   
  prices[, `:=`(
    sell_price_diff = sell_price - dplyr::lag(sell_price),
    sell_price_rel_diff = sell_price / dplyr::lag(sell_price) - 1,
    sell_price_cumrel = (sell_price - cummin(sell_price)) / (1 + cummax(sell_price) - cummin(sell_price)),
    sell_price_roll_sd7 = roll_sdr(sell_price, n = 7)
  ), by = c("store_id", "item_id")]
  
  prices[ , stores := .N , by = c("item_id","wm_yr_wk")]
  
  prices <- prices[prices_long, on = c("store_id", "item_id", "wm_yr_wk")]
  
  rm(prices_long); rm(prices_wide);gc();
  
  return(prices)
  
}







#################### DEAD CODE #########################################



# CA_1
prices_CA_1 <- merge(prices %>% filter(store_id == "CA_1"), prices_wide %>% select(item_id, wm_yr_wk, contains("CA_1")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_CA_1$CA_1 <- NULL
colnames(prices_CA_1)[11:12] <- c("diff_from_mean", "best_price")

# CA_2
prices_CA_2 <- merge(prices %>% filter(store_id == "CA_2"), prices_wide %>% select(item_id, wm_yr_wk, contains("CA_2")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_CA_2$CA_2 <- NULL
colnames(prices_CA_2)[11:12] <- c("diff_from_mean", "best_price")

# CA_3
prices_CA_3 <- merge(prices %>% filter(store_id == "CA_3") , prices_wide %>% select(item_id, wm_yr_wk, contains("CA_3")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_CA_3$CA_3 <- NULL
colnames(prices_CA_3)[11:12] <- c("diff_from_mean", "best_price")

# CA_4
prices_CA_4 <- merge(prices %>% filter(store_id == "CA_4") , prices_wide %>% select(item_id, wm_yr_wk, contains("CA_4")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_CA_4$CA_4 <- NULL
colnames(prices_CA_4)[11:12] <- c("diff_from_mean", "best_price")

# TX_1
prices_TX_1 <- merge(prices %>% filter(store_id == "TX_1") , prices_wide %>% select(item_id, wm_yr_wk, contains("TX_1")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_TX_1$TX_1 <- NULL
colnames(prices_TX_1)[11:12] <- c("diff_from_mean", "best_price")

# TX_2
prices_TX_2 <- merge(prices %>% filter(store_id == "TX_2") , prices_wide %>% select(item_id, wm_yr_wk, contains("TX_2")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_TX_2$TX_2 <- NULL
colnames(prices_TX_2)[11:12] <- c("diff_from_mean", "best_price")

# TX_3
prices_TX_3 <- merge(prices %>% filter(store_id == "TX_3") , prices_wide %>% select(item_id, wm_yr_wk, contains("TX_3")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_TX_3$TX_3 <- NULL
colnames(prices_TX_3)[11:12] <- c("diff_from_mean", "best_price")

# WI_1
prices_WI_1 <- merge(prices %>% filter(store_id == "WI_1") , prices_wide %>% select(item_id, wm_yr_wk, contains("WI_1")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_WI_1$WI_1 <- NULL
colnames(prices_WI_1)[11:12] <- c("diff_from_mean", "best_price")

# WI_2
prices_WI_2 <- merge(prices %>% filter(store_id == "WI_2") , prices_wide %>% select(item_id, wm_yr_wk, contains("WI_2")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_WI_2$WI_2 <- NULL
colnames(prices_WI_2)[11:12] <- c("diff_from_mean", "best_price")

# WI_3
prices_WI_3 <- merge(prices %>% filter(store_id == "WI_3") , prices_wide %>% select(item_id, wm_yr_wk, contains("WI_3")), by = c("item_id", "wm_yr_wk"), all.y = T)
prices_WI_3$WI_3 <- NULL
colnames(prices_WI_3)[11:12] <- c("diff_from_mean", "best_price")


## Rowbind all datasets
prices <- rbind(prices_CA_1, prices_CA_2, prices_CA_3, prices_CA_4,
                prices_TX_1, prices_TX_2, prices_TX_3,
                prices_WI_1, prices_WI_2, prices_WI_3)


head(prices_full)
head(prices)

dim(prices)
dim(prices_full) # 7125900, 12

rm(list = c("prices_CA_1", "prices_CA_2", "prices_CA_3", "prices_CA_4","prices_TX_1", "prices_TX_2", "prices_TX_3",
            "prices_WI_1", "prices_WI_2", "prices_WI_3",
            "prices_wide"))
gc()

format(object.size(prices), units = "auto") # "598.2 Mb"
saveRDS()
