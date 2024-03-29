##################################
#                                #
# Feature Engineering: Prices    #
#                                #
##################################

## TASK: Create features from prices and save them.

## Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(RcppRoll)


## Crate extra prices features ans save them

prices <- fread("data/raw/sell_prices.csv")
prices <- create_prices_features(prices)
saveRDS(prices, "data/raw/prices_full.rds")


create_prices_features <- function(prices){
  
  # TEST
  # prices <- fread("data/raw/sell_prices.csv")
  
  ## FE 01: Compare Item prices between stores ----
  
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
    
    # Relative diff from mean
    CA_1_rel_diff_from_mean = CA_1_diff_from_mean / CA_1,
    CA_2_rel_diff_from_mean = CA_2_diff_from_mean / CA_2,
    CA_3_rel_diff_from_mean = CA_3_diff_from_mean / CA_3,
    CA_4_rel_diff_from_mean = CA_4_diff_from_mean / CA_4,
    TX_1_rel_diff_from_mean = TX_1_diff_from_mean / TX_1,
    TX_2_rel_diff_from_mean = TX_2_diff_from_mean / TX_2,
    TX_3_rel_diff_from_mean = TX_3_diff_from_mean / TX_3,
    WI_1_rel_diff_from_mean = WI_1_diff_from_mean / WI_1,
    WI_2_rel_diff_from_mean = WI_2_diff_from_mean / WI_2,
    WI_3_rel_diff_from_mean = WI_3_diff_from_mean / WI_3,
    
    # Has the best price
    min_price = pmap(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), min, na.rm = T),
    max_price = pmap(select(., CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3), max, na.rm = T),
    
    CA_1_best_price = as.numeric(CA_1 == min_price),
    CA_2_best_price = as.numeric(CA_2 == min_price),
    CA_3_best_price = as.numeric(CA_3 == min_price),
    CA_4_best_price = as.numeric(CA_4 == min_price),
    TX_1_best_price = as.numeric(TX_1 == min_price),
    TX_2_best_price = as.numeric(TX_2 == min_price),
    TX_3_best_price = as.numeric(TX_3 == min_price),
    WI_1_best_price = as.numeric(WI_1 == min_price),
    WI_2_best_price = as.numeric(WI_2 == min_price),
    WI_3_best_price = as.numeric(WI_3 == min_price)
    
  )
  
  # Round results
  col_names <- prices_wide %>% select_if(is.numeric) %>% colnames()
  prices_wide[,col_names] <- round(prices_wide[,col_names], digits = 3)
  
  # View(prices_wide)
  
  
  ## Transform to long format again: prices_long_price
  prices_long_price <- prices_wide %>%
    select(item_id, wm_yr_wk, CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3) %>% 
    pivot_longer(., cols = c(CA_1, CA_2, CA_3, CA_4, TX_1, TX_2, TX_3, WI_1, WI_2, WI_3))
  
  colnames(prices_long_price) <- c("item_id", "wm_yr_wk", "store_id", "sell_price")

  # prices_long_diff_from_mean_price
  prices_long_diff_from_mean_price <- prices_wide %>% select(item_id, wm_yr_wk,
                                              CA_1_diff_from_mean, CA_2_diff_from_mean, CA_3_diff_from_mean, CA_4_diff_from_mean,
                                              TX_1_diff_from_mean, TX_2_diff_from_mean, TX_3_diff_from_mean,
                                              WI_1_diff_from_mean, WI_2_diff_from_mean, WI_3_diff_from_mean) %>% 
    pivot_longer(., cols = c(CA_1_diff_from_mean, CA_2_diff_from_mean, CA_3_diff_from_mean, CA_4_diff_from_mean,
                             TX_1_diff_from_mean, TX_2_diff_from_mean, TX_3_diff_from_mean,
                             WI_1_diff_from_mean, WI_2_diff_from_mean, WI_3_diff_from_mean) )
  
  
  prices_long_diff_from_mean_price$name <- gsub(prices_long_diff_from_mean_price$name, pattern = "_diff_from_mean", replacement = "")
  colnames(prices_long_diff_from_mean_price) <- c("item_id",  "wm_yr_wk", "store_id", "diff_from_mean_price")
  
  
  # prices_long_rel_diff_from_mean_price
  prices_long_rel_diff_from_mean_price <- prices_wide %>% select(item_id, wm_yr_wk,
                                                             CA_1_rel_diff_from_mean, CA_2_rel_diff_from_mean, CA_3_rel_diff_from_mean, CA_4_rel_diff_from_mean,
                                                             TX_1_rel_diff_from_mean, TX_2_rel_diff_from_mean, TX_3_rel_diff_from_mean,
                                                             WI_1_rel_diff_from_mean, WI_2_rel_diff_from_mean, WI_3_rel_diff_from_mean) %>% 
    pivot_longer(., cols = c( CA_1_rel_diff_from_mean, CA_2_rel_diff_from_mean, CA_3_rel_diff_from_mean, CA_4_rel_diff_from_mean,
                              TX_1_rel_diff_from_mean, TX_2_rel_diff_from_mean, TX_3_rel_diff_from_mean,
                              WI_1_rel_diff_from_mean, WI_2_rel_diff_from_mean, WI_3_rel_diff_from_mean) )
  
  
  prices_long_rel_diff_from_mean_price$name <- gsub(prices_long_rel_diff_from_mean_price$name, pattern = "_rel_diff_from_mean", replacement = "")
  colnames(prices_long_rel_diff_from_mean_price) <- c("item_id",  "wm_yr_wk", "store_id", "rel_diff_from_mean_price")
  
  # prices_long_best_price
  prices_long_best_price <- prices_wide %>% select(item_id, wm_yr_wk,
                                                   CA_1_best_price, CA_2_best_price, CA_3_best_price, CA_4_best_price,
                                                   TX_1_best_price, TX_2_best_price, TX_3_best_price,
                                                   WI_1_best_price, WI_2_best_price, WI_3_best_price) %>% 
    pivot_longer(., cols = c( CA_1_best_price, CA_2_best_price, CA_3_best_price, CA_4_best_price,
                              TX_1_best_price, TX_2_best_price, TX_3_best_price,
                              WI_1_best_price, WI_2_best_price, WI_3_best_price) )
  
  prices_long_best_price$name <- gsub(prices_long_best_price$name, pattern = "_best_price", replacement = "")
  colnames(prices_long_best_price) <- c("item_id",  "wm_yr_wk", "store_id", "best_price")
  
  rm(prices_wide)
  
  ## Combine all datasets
  
  # Check if all objects has the same dimension (and the rows are in the same order! SOS)
  lapply(list(prices_long_price, prices_long_diff_from_mean_price, prices_long_rel_diff_from_mean_price,prices_long_best_price), dim)
  
  prices_long <- cbind(prices_long_price, prices_long_diff_from_mean_price[, "diff_from_mean_price"],
                       prices_long_rel_diff_from_mean_price[,"rel_diff_from_mean_price"],
                       prices_long_best_price[,"best_price"])
  
  rm(prices_long_price);rm(prices_long_diff_from_mean_price);rm(prices_long_rel_diff_from_mean_price);rm(prices_long_best_price)
  gc()
  
  prices_long <- as.data.table(prices_long)
  # View(head(prices_long,100)) 
  
  
  ## FE 02: Selling prices Features ----
  
  prices[, `:=`(
    sell_price_diff = sell_price - dplyr::lag(sell_price),
    sell_price_rel_diff = sell_price / dplyr::lag(sell_price) - 1,
    sell_price_cumrel = (sell_price - cummin(sell_price)) / (1 + cummax(sell_price) - cummin(sell_price)),
    sell_price_roll_sd7 = roll_sdr(sell_price, n = 7)), by = c("store_id", "item_id")
    ][is.na(sell_price_diff), sell_price_diff := 0
      ][is.na(sell_price_rel_diff), sell_price_rel_diff := 0
        ][is.na(sell_price_roll_sd7), sell_price_roll_sd7 := 0]
  
  prices[ , nb_stores := .N , by = c("item_id","wm_yr_wk")]
  
  
  prices_aggregations <- prices[, .(unique_sell_prices = length(unique(sell_price)), 
                                    sell_price_changes = sum(sell_price_diff != 0),
                                    mean_sell_price = mean(sell_price),
                                    min_sell_price = min(sell_price),
                                    max_sell_price = max(sell_price),
                                    max_diff = max(sell_price) -  min(sell_price)), by = c("store_id", "item_id")]
  
  
  prices <- prices[prices_aggregations, on = c("store_id", "item_id"), nomatch = 0]
  
  
  prices[, `:=`(sell_price_to_mean = sell_price / mean_sell_price,
                sell_price_to_min = sell_price / min_sell_price,
                sell_price_to_max = max_sell_price / sell_price,
                sell_price_diff_to_max_diff = sell_price_diff / max_diff) ]
  
  prices[is.nan(sell_price_diff_to_max_diff), sell_price_diff_to_max_diff:=0]
  
  # View(head(prices,200))
  
  
  ## Merge the features from prices_long ----

  prices <- prices[prices_long, on = c("store_id", "item_id", "wm_yr_wk"), nomatch = 0]
  prices[,i.sell_price := NULL]
  rm(prices_long);gc();
  
  
  ## FE 03: Lead Features ----
  prices[ , `:=`(sell_price_to_min_lead_1 = lead(sell_price_to_min, 1),
                 sell_price_to_min_lead_2 = lead(sell_price_to_min, 2),
                 sell_price_to_max_lead_1 = lead(sell_price_to_max, 1),
                 sell_price_to_max_lead_2 = lead(sell_price_to_max, 2)), by = c("store_id", "item_id")]
  
  ## Deal with NA / NaN
  prices[is.na(prices)] <- 0
  
  # View(head(prices, 200))
  
  return(prices)
}




## Sanity check

# prices_raw <- fread("data/raw/sell_prices.csv")
# prices <- create_prices_features(prices_raw)
# 
#  merge(prices_raw, prices %>% select(store_id, item_id, wm_yr_wk, sell_price), by = c("store_id", "item_id", "wm_yr_wk")) %>% 
#    filter(sell_price.x != sell_price.y)
# # Empty


## Explore sell prices data

# library(ggplot2)
# 
# # 1. Discount feature
# 
# dt <- prices[, sell_price_diff := sell_price - lag(sell_price)
#              ][is.na(sell_price_diff), sell_price_diff := 0
#                ][, .(unique_sell_price = length(unique(sell_price)), 
#                      sell_price_changes = sum(sell_price_diff != 0),
#                      mean_sell_price = mean(sell_price),
#                      min_sell_price = min(sell_price),
#                      max_sell_price = max(sell_price),
#                      max_diff = max(sell_price) -  min(sell_price)), by = c("store_id", "item_id")]
#               
# View(dt)
# 
# ggplot(dt) + geom_histogram(aes(sell_price_changes), binwidth = 1) + ggtitle("sell_price_changes")
# table(dt$sell_price_changes)
# 
# 
# df <- prices[store_id == "TX_1" & item_id ==	"FOODS_3_092", list(sell_price, wm_yr_wk)]
# ggplot(df) + geom_line(aes(wm_yr_wk, sell_price)) + ggtitle("sell_price")
