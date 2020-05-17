#########################################################################
#                                                                       #
#   Prophet for all time-series and optimal combination with weights    #
#                                                                       #
#########################################################################


## 00. Load packages ----

library(tidyverse)
library(plotly)
library(tictoc)
library(hts)


## 01. Import data ----

## Calendar
calendar <- read.csv("data/raw/calendar.csv", na.strings = c("_",""), stringsAsFactors = F)
calendar$date <- as.Date(calendar$date)

# Create external regressors
x_var <- calendar
x_var$snap <- x_var$snap_CA + x_var$snap_WI + x_var$snap_TX
x_var$holiday <- 0
x_var[is.na(x_var$event_type_1)==F,]$holiday <- 1
x_var <- x_var[,c("snap","holiday")]


## Sales
sales_raw <- read.csv("data/raw/sales_train_validation.csv", stringsAsFactors = F) 


## 02. Define bts (bottom time series) matrix ----

# - bts is a time series matrix containing the bottom-level series
# - each column corresponds to a different time series (sales for item_id - store_id)
# - each row corresponds to a different day

sales_long <- t(sales_raw[,7:ncol(sales_raw)])
dim(sales_long)

bts <- ts(data = sales_long, frequency = 7)
dim(bts)

# nrow : number of days
# ncol : number of bottom level timeseries


## // Define groups and hierarhies for hts::gts object//

# head(sales$id)

# 1.  Total
# 2.  State
# 3.  Store
# 4.  Category
# 5.  Department
# 6.  State - Category
# 7.  State - Department
# 8.  Store - Category
# 9.  Store - Department
# 10. Product
# 11. Product - State
# 12. Product - Store

state_id <- sapply(strsplit(sales_raw$id, split = "_"), function(x){x[4]})
store_id <- sapply(strsplit(sales_raw$id, split = "_"), function(x){paste(x[4], x[5], sep = "_")})
cat_id <- sapply(strsplit(sales_raw$id, split = "_"), function(x){x[1]})
dept_id <- sapply(strsplit(sales_raw$id, split = "_"), function(x){paste(x[1], x[2], sep = "_")})
state_cat_id <- paste(state_id, cat_id, sep = "_")
state_dept_id <- paste(state_id, dept_id, sep = "_")
store_cat_id <- paste(store_id, cat_id, sep = "_")
store_dept_id <- paste(store_id, dept_id, sep = "_")
item_id <- sapply(strsplit(sales_raw$id, split = "_"), function(x){paste(x[1], x[2], x[3], sep = "_")})
item_state_id <- paste(item_id, state_id, sep = "_")


groups_names <- matrix(data = c(state_id, store_id, cat_id, dept_id, state_cat_id, state_dept_id,
                                store_cat_id, store_dept_id, item_id, item_state_id),
                       ncol = 30490, byrow = TRUE)

# groups_names[, 1:6]
# dim(bts)


## Define gts object
y <- gts(bts, groups = groups_names)
y


## 03. Create a data.frame object with all time series (in correct order as in groups_names)  ----

# Aggregated time series
aggregated_ts <- aggts(y, levels = c(0,1,2,3,4,5,6,7,8,9,10))
aggregated_ts_df <- as.data.frame(aggregated_ts)
dim(aggregated_ts_df)

# Bottom level time series
bts_df <- as.data.frame(bts)
dim(bts_df)


## Create data frame with dates and sales for all time series 
df <- data.frame(ds = calendar[1:nrow(bts_df), "date"],
                 day = calendar[1:nrow(bts_df), "weekday"],
                 wday = calendar[1:nrow(bts_df), "wday"],
                 snap = x_var$snap[1:nrow(bts_df)],
                 holiday = x_var$holiday[1:nrow(bts_df)])

head(df)

# cbind all time series
df <- cbind(df, aggregated_ts_df, bts_df)

df[1:6, 1:20]
dim(df)        # 1913  42845 (42840 + 5)
# colnames(df) <- c("ds", "day", "wday", "snap", "holiday", paste0("F",1:42840))


# Create valid_df (with snap for the prophet forecast)
valid_df <- data.frame(ds = calendar[1914:1941,"date"],
                       day = calendar[1914:1941, "weekday"],
                       wday = calendar[1914:1941, "wday"],
                       snap = x_var$snap[1914:1941],
                       holiday = x_var$holiday[1914:1941])




## 04. Make parallel forecastings with prophet_v3 ----

source("modules/frc_methods.R")
library(foreach)
library(doSNOW)


# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 11, type = "SOCK", outfile = ""))

# Get forecasts using the prophet_frc_v3() function
tic()
frc_total_prophet_v2 <- foreach(tsid = 6:ncol(df),  # 6:ncol(df)
                                .combine = 'rbind', 
                                .packages = c('forecast','prophet')) %dopar% prophet_frc_v3(df_prophet = df, valid_prophet = valid_df, h = 28, tsid = tsid)
toc() # ~ 6 hours


# Save forecastings
saveRDS(object = frc_total_prophet_v2, file = "frc_total_prophet_v2.RDS")

# Stop cluster and clean the memory
stopCluster(cl = cl) # This error happens when code from library(doSNOW) and library(parallel) is mixed.
showConnections()
gc()




## 05. Check-Plot forecastings -----
dim(df)

head(colnames(df))
frc_total_prophet_v2 <- readRDS("frc_total_prophet_v2.RDS")
dim(frc_total_prophet_v2)

new_df <- data.frame(ds = calendar[1:(1913+28), "date"],
                     y = c(df[,123+5], frc_total_prophet_v2[123,]),
                     type = c(rep("true", 1913), rep("frc", 28)))

p <- ggplot(new_df) + geom_line(aes(ds,y, col = type)) + geom_point(aes(ds,y, col = type))
ggplotly(p)



## 06. Optimal Combination for base forecastings (create reconciled forecastings) ----

frc_total_prophet_v2 <- readRDS("frc_total_prophet_v2.RDS")
frc_total_prophet_v2 <- t(frc_total_prophet_v2)
dim(frc_total_prophet_v2)

# Number of time series at each group level: 1 3 10 3 7 9 21 30 70 3049 9147 30490 = 42840
1+3+10+3+7+9+21+30+70+3049+9147 # 12350
12350 + 30490                   # 42840


groups_matrix <- get_groups(y)


# Ensemble with bts forecastings
submission_24 <- read.csv("data/pnt_submissions/submission_ensembled24.csv")
submission_24 <- submission_24 %>% filter(grepl(pattern = "validation", x = .$id))
submission_24 <- merge(x = data.frame(id = sales_raw$id, stringsAsFactors = F), y = submission_24, sort = F)
head(submission_24)
dim(submission_24)
submission_24$id <- NULL

dim(frc_total_prophet_v2)
frc_total_prophet_v2[,12351:ncol(frc_total_prophet_v2)] <- t(submission_24)


## 1. Without weights

frc_opt_v0 <- combinef(fcasts = frc_total_prophet_v2,
                       groups = groups_matrix,
                       weights = NULL ,             #  NULL or weights_vec
                       keep ="bottom",              # c("gts", "all", "bottom")
                       algorithms = "cg"            # c("lu", "cg", "chol", "recursive", "slm")
)

dim(frc_opt_v0)


## 2. With weights

weigths <- read.csv("data/raw/weights_validation.csv")
weights_vec <- weigths$Weight
length(weights_vec)

tic()  
frc_opt_v1 <- combinef(fcasts = frc_total_prophet_v2,
                       groups = groups_matrix,
                       weights = weights_vec+0.0000001,     #  NULL or weights_vec
                       keep ="bottom",                     # c("gts", "all", "bottom")
                       algorithms = "lu"                   # c("lu", "cg", "chol", "recursive", "slm")
)
toc()



## Check reconciled forecastings

## How much the base forecastings changed, after optimal combination
sub_v0 %>% select(starts_with("F")) %>% head(30490) %>% colSums() - frc_total_prophet_v2[,1]
sub_v1 %>% select(starts_with("F")) %>% head(30490) %>% colSums() - frc_total_prophet_v2[,1]


## 07. Create submission files ----

sub_v2 <- create_submission_file(frc_opt_combination = frc_opt_v0)
sub_v3 <- create_submission_file(frc_opt_combination = frc_opt_v1)

write.csv(sub_v2, "sub_v2.csv", row.names = F)
write.csv(sub_v3, "sub_v3.csv", row.names = F)


## Create submission files function

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
