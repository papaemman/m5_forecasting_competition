###########################
#                         #
# Stat total exploration  #
#                         #
###########################

## 00. Load libraries ----
library(tidyverse)
library(ggthemes)

## 01. Import adn Explore data ----
stat_total <- read_csv("data/processed/stat_total.csv")

stat_total$X1 <- NULL

View(head(stat_total, 100))
colnames(stat_total)

## 1. How many time series there are? 
# Every row corresponds to an item_id
nrow(stat_total) # 30.490


## 2. Item_id

stat_total %>% count(item_id) %>% View()
# Every item_id code there is 10 times in the dataset, one fore every store


## 3. How many items there are in every dept_id / cat_id / store_id / state_id
stat_total %>% count(dept_id) %>%
  ggplot(., aes(x = dept_id, y = n)) +
  geom_col() + ggtitle("Total products per department") + theme_economist() + scale_color_economist()

stat_total %>% count(cat_id) %>%
  ggplot(., aes(x = cat_id, y = n)) +
  geom_col() + ggtitle("Total products per category") + theme_economist() + scale_color_economist()

stat_total %>% count(dept_id, cat_id)

stat_total %>% count(store_id) %>%
  ggplot(., aes(x = store_id, y = n)) +
  geom_col() + ggtitle("Total products per store") + theme_economist() + scale_color_economist()

stat_total %>% count(store_id, state_id)


## 3. Length of time series

# Remove the leading zeros from time series, because that means that the particular item_id wasn't available 
# at this store.

stat_total %>% group_by(dept_id) %>% summarise(mean(lngth))
stat_total %>% group_by(cat_id) %>% summarise(mean(lngth))
stat_total %>% group_by(state_id) %>% summarise(mean(lngth))

ggplot(stat_total, aes(x = "", y = lngth)) + geom_boxplot()

summary(stat_total$lngth)


## 4. ADI
stat_total %>% group_by(dept_id) %>% summarise(mean(ADI))
stat_total %>% group_by(cat_id) %>% summarise(mean(ADI))
stat_total %>% group_by(state_id) %>% summarise(mean(ADI))

ggplot(stat_total, aes(x = "", y = ADI)) + geom_boxplot()

summary(stat_total$lngth)

