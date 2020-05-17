#########################################################################
#                                                                       #
#   Prophet for all time-series and optimal combination with weights    #
#                                                                       #
#########################################################################


## 00. Load packages ----
library(hts)
library(ggplot2)
library(dplyr)
library(tictoc)

## 01. Import data ----

# Calendar
calendar <- read.csv("data/raw/calendar.csv", na.strings = c("_",""), stringsAsFactors = F)
calendar$date <- as.Date(calendar$date)

# Create external regressors
x_var <- calendar
x_var$snap <- x_var$snap_CA + x_var$snap_WI + x_var$snap_TX
x_var$holiday <- 0
x_var[is.na(x_var$event_type_1)==F,]$holiday <- 1
x_var <- x_var[,c("snap","holiday")]




## Sales correct order

sales <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F) 

state_id <- sapply(strsplit(sales$id, split = "_"), function(x){x[4]})
state_order <- state_id %>% unique()

store_id <- sapply(strsplit(sales$id, split = "_"), function(x){paste(x[4], x[5], sep = "_")})
store_order <- store_id %>% unique()

cat_id <- sapply(strsplit(sales$id, split = "_"), function(x){x[1]})
cat_order <- cat_id %>% unique()

dept_id <- sapply(strsplit(sales$id, split = "_"), function(x){paste(x[1], x[2], sep = "_")})
dept_order <- dept_id %>% unique()

state_cat_id <- paste(state_id, cat_id, sep = "_")
state_cat_order <- state_cat_id %>% unique()

state_dept_id <- paste(state_id, dept_id, sep = "_")
state_dept_order <- state_dept_id %>% unique()

store_cat_id <- paste(store_id, cat_id, sep = "_")
store_cat_order <- store_cat_id %>% unique()

store_dept_id <- paste(store_id, dept_id, sep = "_")
store_dept_order <-store_dept_id %>% unique() 

item_id <- sapply(strsplit(sales$id, split = "_"), function(x){paste(x[1], x[2], x[3], sep = "_")})
item_order <- item_id %>% unique()

item_state_id <- paste(item_id, state_id, sep = "_")
item_state_order <- item_state_id %>% unique()


## Import aggregated sales (and fix the order!!)

# H1
sales_h1 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_1.rds")

# H2
sales_h2 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_2.rds")
sales_h2 <- merge(data.frame(state_id = state_order, stringsAsFactors = F), sales_h2, sort = F)
sales_h2 <- t(sales_h2[,2:ncol(sales_h2)])

# H3
sales_h3 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_3.rds")
sales_h3 <- merge(data.frame(store_id = store_order, stringsAsFactors = F), sales_h3, sort = F)
sales_h3 <- t(sales_h3[,2:ncol(sales_h3)])

# H4
sales_h4 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_4.rds")
sales_h4 <- merge(data.frame(cat_id = cat_order, stringsAsFactors = F), sales_h4, sort = F)
sales_h4 <- t(sales_h4[,2:ncol(sales_h4)])

# H5
sales_h5 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_5.rds") 
sales_h5 <- merge(data.frame(dept_id = dept_order, stringsAsFactors = F), sales_h5, sort = F)
sales_h5 <- t(sales_h5[,2:ncol(sales_h5)])

# H6
sales_h6 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_6.rds")
sales_h6 <- sales_h6 %>% unite(col = "id",  state_id, cat_id, sep = "_", remove = T)
sales_h6 <- merge(data.frame(id = state_cat_order, stringsAsFactors = F), sales_h6, sort = F)
sales_h6 <- t(sales_h6[,2:ncol(sales_h6)])

# H7
sales_h7 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_7.rds")
sales_h7 <- sales_h7 %>% unite(col = "id",  state_id, dept_id, sep = "_", remove = T)
sales_h7 <- merge(data.frame(id = state_dept_order, stringsAsFactors = F), sales_h7, sort = F)
sales_h7 <- t(sales_h7[,2:ncol(sales_h7)])

# H8
sales_h8 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_8.rds")
sales_h8 <- sales_h8 %>% unite(col = "id",  store_id, cat_id, sep = "_", remove = T)
sales_h8 <- merge(data.frame(id = store_cat_order, stringsAsFactors = F), sales_h8, sort = F)
sales_h8 <- t(sales_h8[,2:ncol(sales_h8)])

# H9
sales_h9 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_9.rds")
sales_h9 <- sales_h9 %>% unite(col = "id",  store_id, dept_id, sep = "_", remove = T)
sales_h9 <- merge(data.frame(id = store_dept_order, stringsAsFactors = F), sales_h9, sort = F)
sales_h9 <- t(sales_h9[,2:ncol(sales_h9)])

# H10
sales_h10 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_10.rds")
sales_h10 <- merge(data.frame(item_id = item_order, stringsAsFactors = F), sales_h10, sort = F)
sales_h10 <- t(sales_h10[,2:ncol(sales_h10)])

# H11
sales_h11 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_11.rds")
sales_h11 <- sales_h11 %>% unite(col = "id",  item_id, state_id, sep = "_", remove = T)
sales_h11 <- merge(data.frame(id = item_state_order, stringsAsFactors = F), sales_h11, sort = F)
sales_h11 <- t(sales_h11[,2:ncol(sales_h11)])

# H12
sales_h12 <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F) 
sales_h12 <- t(sales_h12[,7:ncol(sales_h12)])


## Check group Levels
groups_matrix <- readRDS("groups_matrix.RDS")
dim(groups_matrix)


## Create data frame withe dates and sales
df <- data.frame(ds = calendar[1:length(sales_h1), "date"],
                 day = calendar[1:length(sales_h1), "weekday"],
                 wday = calendar[1:length(sales_h1), "wday"],
                 snap = x_var$snap[1:length(sales_h1)],
                 holiday = x_var$holiday[1:length(sales_h1)])

head(df)

df <- cbind(df, sales_h1 = as.matrix(sales_h1, ncol=1), sales_h2, sales_h3, sales_h4,
            sales_h5, sales_h6, sales_h7, sales_h8, sales_h9, sales_h10, sales_h11, sales_h12)


df[1:6, 1:20]
dim(df)        # 1913  42845 (42840 + 5)
colnames(df) <- c("ds", "day", "wday", "snap", "holiday", paste0("F",1:42840))

valid_df <- data.frame(ds = calendar[1914:1941,"date"],
                       day = calendar[1914:1941, "weekday"],
                       wday = calendar[1914:1941, "wday"],
                       snap = x_var$snap[1914:1941],
                       holiday = x_var$holiday[1914:1941])



## Make parallel forecastings with prophet_v2
source("modules/frc_methods.R")
library(foreach)
library(doSNOW)

# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 10, type = "SOCK", outfile = ""))

# Get forecasts using the prophet_frc_v2() function
tic()
frc_total_prophet_v2 <- foreach(tsid = 6:ncol(df),  # 6:ncol(df)
                                .combine = 'rbind', 
                                .packages = c('forecast','prophet')) %dopar% prophet_frc_v3(df_prophet = df, valid_prophet = valid_df, h = 28, tsid = tsid)
toc() # ~ 6 hours

# Save forecastings
saveRDS(object = frc_total_prophet_v2, file = "data/forecastings/frc_total_prophet_v2.RDS")

# Stop cluster and clean the memory
stopCluster(cl = cl) # This error happens when code from library(doSNOW) and library(parallel) is mixed.
showConnections()
gc()




## 07. Forecastings Optimal combination  ----

frc <- readRDS("frc_prophet_v2_full.rds")
frc <- t(frc)
dim(frc)

# Number of time series at each group level: 1 3 10 3 7 9 21 30 70 3049 9147 30490 = 42840
1+3+10+3+7+9+21+30+70+3049+9147 # 12350
12350 + 30490                   # 42840


## Optimal Combination for base forecastings (create reconciled forecastings)
weigths <- read.csv("data/raw/weights_validation.csv")
weights_vec <- weigths$Weight
length(weights_vec)



tic()  
frc_opt_v0 <- combinef(fcasts = frc,
                       groups = groups_matrix,
                       weights = NULL ,             #  NULL or weights_vec
                       keep ="bottom",              # c("gts", "all", "bottom")
                       algorithms = "cg"            # c("lu", "cg", "chol", "recursive", "slm")
)

dim(frc_opt_v0)


tic()  
frc_opt_v1 <- combinef(fcasts = frc,
                       groups = groups_matrix,
                       weights = weights_vec+0.000001,     #  NULL or weights_vec
                       keep ="bottom",                     # c("gts", "all", "bottom")
                       algorithms = "lu"                   # c("lu", "cg", "chol", "recursive", "slm")
)

frc_opt_v1




## Check prophet forecastings

colnames(df)

new_df <- data.frame(ds = calendar[1:(1913+28), "date"],
                     y = c(df$V1972, frc[,1972]),
                     type = c(rep("true", 1913), rep("frc", 28)))

p <- ggplot(new_df) + geom_line(aes(ds,y, col = type))
ggplotly(p)


## How much change the base forecastings, after optimal combination

sub_v0 <- create_submission_file(frc_opt_combination = frc_opt_v0)
sub_v1 <- create_submission_file(frc_opt_combination = frc_opt_v1)

write.csv(sub_v0, "sub_v0.csv", row.names = F)
write.csv(sub_v1, "sub_v1.csv", row.names = F)


sub_v0 %>% select(starts_with("F")) %>% head(30490) %>% colSums() - frc[,1]
sub_v1 %>% select(starts_with("F")) %>% head(30490) %>% colSums() - frc[,1]



## Create submission files

create_submission_file <- function(frc_opt_combination){
  
  # frc_opt_combination = frc_opt_v0
  
  dim(frc_opt_combination)
  
  frc_opt_combination <- t(frc_opt_combination)
  frc_opt_combination <- as.data.frame(frc_opt_combination)
  
  temp <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F) 
  frc_opt_combination$id <- temp$id
  colnames(frc_opt_combination) <- c(paste0("F", 1:28),"id")
  
  frc_opt_combination_eval <- frc_opt_combination
  frc_opt_combination_eval$id <- gsub(pattern = "validation", replacement = "evaluation", x = frc_opt_combination_eval$id)
  frc_opt_combination <- rbind(frc_opt_combination, frc_opt_combination_eval)
  
  
  sample_submission <- read.csv("data/pnt_submissions/sample_submission.csv", stringsAsFactors = F)
  head(sample_submission)
  dim(sample_submission)
  dim(frc_opt_combination)
  
  frc_opt_combination <- merge(data.frame(id = sample_submission[,c("id")]), frc_opt_combination, by = "id", sort = F)
  
  return(frc_opt_combination)
}
