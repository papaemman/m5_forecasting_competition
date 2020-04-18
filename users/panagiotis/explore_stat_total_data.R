#################################
#                               #
#  Explore stat total data      #
#                               #
#################################

library(dplyr)
slibrary(ggplot2)


## Import stat_total data

stat_total <- readRDS("data/processed/stat_total.rds")
View(stat_total)
colnames(stat_total)


# 30.490 item_ids per store_id
stat_total %>% group_by(store_id) %>% summarise(n())

# lnght
summary(stat_total$lngth)

stat_total %>% filter(lngth < 80)

1913 * 30490          # 58.327.370
sum(stat_total$lngth) # 45.942.500 (13.000.000 useless rows)



ggplot(stat_total) + geom_histogram(aes(lngth), binwidth = 10) +
  ggtitle("Length of demand time series \nexcluding the initial zero-demand interval")






# ADI (Average demand interval)
ggplot(stat_total) + geom_histogram(aes(ADI), binwidth = 1) +
  ggtitle("Average demand interval")

summary(stat_total$ADI)

# pz (probability of zero)
ggplot(stat_total) + 
  geom_histogram(aes(pz, fill=cat_id), binwidth = 0.01) +
  geom_vline(xintercept = 0.50, linetype = 3)+
  ggtitle("Probability of zero demand")

ggplot(stat_total) + 
  geom_histogram(aes(pz, fill = Type), binwidth = 0.01) +
  geom_vline(xintercept = 0.50, linetype = 3)+
  ggtitle("Probability of zero demand")

# type
ggplot(stat_total) + geom_bar(aes(Type, fill = cat_id)) +
  ggtitle("Demand time series type")


# Sales
summary(stat_total$Min)
summary(stat_total$Low25)
summary(stat_total$Mean)
summary(stat_total$Median)
summary(stat_total$Up25)
summary(stat_total$Max)

ggplot(stat_total) + 
  geom_histogram(aes(Mean), binwidth = 0.01, col = "lightblue") +
  ggtitle("Mean sales (demand)")

ggplot(stat_total) + 
  geom_histogram(aes(Min), binwidth = 0.01, col = "lightblue") +
  ggtitle("Min sales (demand)")


# Price
summary(stat_total$dollar_sales)
stat_total %>% filter(dollar_sales > 10000)

ggplot(stat_total) + 
  geom_histogram(aes(mean_price, fill = cat_id), binwidth = 1) +
  ggtitle("Mean price")

ggplot(stat_total) + 
  geom_histogram(aes(n_of_changes, fill = cat_id), binwidth = 1) +
  ggtitle("Number of changes in price")

ggplot(stat_total %>% filter(max_diff > 25)) + 
  geom_histogram(aes(max_diff, fill = cat_id), binwidth = 1) +
  ggtitle("Max difference in price | max_diff > 25")


# Dollar sales
ggplot(stat_total) + 
  geom_histogram(aes(dollar_sales, fill = cat_id), binwidth = 10) +
  ggtitle("Dollar sales", "based on last 28 days")

# Best sellers
ggplot(stat_total %>% filter(dollar_sales > 1000)) + 
  geom_histogram(aes(dollar_sales, fill = cat_id), binwidth = 10) +
  ggtitle("Dollar sales | > 1000", "based on last 28 days")

ggplot(stat_total %>% filter(dollar_sales < 100)) + 
  geom_histogram(aes(dollar_sales, fill = cat_id), binwidth = 1) +
  ggtitle("Dollar sales | < 100", "based on last 28 days")

# Mean prices vs mean sales
ggplot(stat_total) + 
  geom_point(aes(mean_price, Mean, col = cat_id), size = 0.8, alpha = 0.7)+
  ylab("Mean sales")+ 
  ggtitle("Average demand (sales) vs mean price")


# Mean price vs dollar sales
ggplot(stat_total) + 
  geom_point(aes(mean_price, dollar_sales, col = mean_sales), size = 0.8, alpha = 0.7)+
  ggtitle("Dollar sales vs mean price")