#################################
#                               #
#   Autocorrelation function    # 
#                               #
#################################

library(dplyr)
library(foreach)
library(doSNOW)

bts <- readRDS("data/processed/time_series_b.rds")

?pacf

bts[[1]]

temp <- bts[[21324]]$x
acf(temp)

# Some item_ids - stores_ids has high aauto-correlation in small lag values, while some other products
# has high auto-correlation values in big lags


run_acf_pos(1)
run_acf_pos(100)
run_acf_pos(9)
run_acf_val(21328)


cl = registerDoSNOW(makeCluster(spec = 12, type = "SOCK", outfile = ""))

acf_results <- foreach(tsid = 1:length(bts), # 1:length(bts)
                       .combine = "rbind"
                       ) %dopar% run_acf_pos(tsid)


acf_results <- as.data.frame(acf_results)

acf_results$dept_id <- sapply(bts, function(x){x$dept_id})
dim(acf_results)

acf_results %>% group_by(dept_id) %>% summarise(mean(V1), mean(V2), mean(V3))

acf_results %>% group_by(dept_id) %>% mutate(V = V2+V3+V4) %>% summarise(mean(V))

ggplot(acf_results)

run_acf_pos <- function(tsid){
  res <- acf(bts[[tsid]]$x, plot = F)
  vec <- as.numeric(res$acf)
  names(vec) <- res$lag
  vec <- sort(vec,decreasing = T)
  return(as.numeric(names(vec))[1:7])
}

run_acf_val <- function(tsid){
  res <- acf(bts[[tsid]]$x, plot = F)
  vec <- as.numeric(res$acf)
  names(vec) <- res$lag
  vec <- sort(vec,decreasing = T)
  return(vec)
}

