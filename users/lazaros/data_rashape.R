library(tidyverse)
library(magrittr)


sales_train_val <-read.csv("data/raw/sales_train_validation.csv")
read.csv("data/raw/calendar.csv") -> calendar
read.csv("data/raw/sample_submission.csv")-> sample_subm
read.csv("data/raw/sell_prices.csv") -> prices
dim(sales_train_val)

# reshape data 
long_DF <- sales_train_val %>% gather(id,n_sales, d_1:d_1913)
#dim(long_DF)
#nrow(long_DF)/1913
# long_DF[1:1000]
#long_DF$ %>% table

# merge calendar
long_DF %>% filter(item_id == "FOODS_3_380", store_id =="CA_1") -> item # just for an item
merge(item,calendar,by.x = "id",by.y = "d") -> item

# join prices
prices %>% filter(item_id == "FOODS_3_380",store_id =="CA_1" ) -> item_prices # just for an item

item <-left_join(item,item_prices,by = "wm_yr_wk")




# ggplot(item) + geom_line(aes(x= id , y = n_sales))
# 
# item %>% group_by(id) %>% summarise(total_sales_per_state = sum(n_sales,na.rm = TRUE)) -> te
# 
# te$id %<>% as.factor()
# 
# ggplot(te) + geom_line(aes(x = id, y = total_sales_per_state))
