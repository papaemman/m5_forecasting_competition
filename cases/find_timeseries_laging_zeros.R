##########################################
#                                        #
# Find timeseries with ladding zeros     #
#                                        #
##########################################

library(doSNOW)
library(foreach)

time_series_b <- readRDS("data/processed/time_series_b.rds")

count_last_sales <- function(x = time_series_b[[tsi]], N=28){

  ts <- x$x
  count <- sum(tail(ts, N))
  count <- c(count, paste0(x$item_id, "_", x$store_id))
  return(count)
}

count_last_sales(x = time_series_b[[1]], N = 28)

# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 3, type = "SOCK"))

# Get forecasts for local models
count_last_sales <- foreach(tsi = 1:length(time_series_b),
                     .combine = 'rbind') %dopar% count_last_sales(x = time_series_b[[tsi]], N = 60)

gc()
View(count_last_sales)
saveRDS(count_last_sales, "data/count_sales_last60_days.rds")

sum(count_last_sales == 0)
sum(count_last_sales <= 2)


#########
library(dplyr)

ensemble_submission <- read.csv("data/pnt_submissions/submission_ensembled24.csv")

head(ensemble_submission)

store_prod <- count_last_sales %>% as.data.frame() %>%  dplyr::filter(V1==0 | V1 ==1) %>% pull(V2) %>% as.character()

store_prod <- count_last_sales %>% as.data.frame() %>%  dplyr::filter(V1==0) %>% pull(V2) %>% as.character()

ensemble_submission %>% filter(id %in% paste0(store_prod, "_validation")) %>% View()

ensemble_submission %>% filter(id %in% paste0(store_prod, "_validation"))  %>% select(-id) %>% colSums()

ensemble_submission[ensemble_submission$id %in% paste0(store_prod, "_validation"), c("F1", "F2")] <- 0 

write.csv(ensemble_submission, file = "data/pnt_submissions/submission_ensembled24_zero_sales_last_60_f1_f2.csv", row.names = F)                               
