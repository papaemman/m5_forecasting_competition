#########################################
#                                       #
# Check weights_validation.csv dataset  #
#                                       #
#########################################

## Import packages
source("modules/main.R")

## 01. weights_validation dataset
weights_validation <- read.csv("data/raw/weights_validation.csv")
View(weights_validation)

str(weights_validation)
summary(weights_validation)

# For every aggreagation level
weights_validation %>%
  group_by(Level_id) %>% 
  summarise(Total_time_series = n(), Sum = sum(Weight))


## 02. Evaluation metric score

naive_wrmsse <- read.csv("data/submissions/exp_naive_WRMSSE.csv")
View(naive_wrmsse)


## 03. Stat_total dataset (dollar sales as weights)
stat_total <- readRDS("data/processed/stat_total.rds")

head(stat_total)

total_sales <- sum(stat_total$dollar_sales)

# Sanity check: Level 2
stat_total %>%
  group_by(state_id) %>%
  summarise(total_dollar_sales = sum(dollar_sales),
            weight = total_dollar_sales / total_sales)

weights_validation %>% filter(Level_id =="Level2")


# Sanity check: Level 3
stat_total %>%
  group_by(store_id) %>%
  summarise(total_dollar_sales = sum(dollar_sales),
            weight = total_dollar_sales / total_sales)

weights_validation %>% filter(Level_id =="Level3")


# Sanity check: Level 5
stat_total %>%
  group_by(dept_id) %>%
  summarise(total_dollar_sales = sum(dollar_sales),
            weight = total_dollar_sales / total_sales)

weights_validation %>% filter(Level_id =="Level5")


## Conclusion: All good!
# The weights_validation.csv file, contains the same information with stat_total$dollar_sales column!



